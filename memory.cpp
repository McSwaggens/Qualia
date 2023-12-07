#include "memory.h"
#include "general.h"
#include "assert.h"

static byte* AllocateVirtualPage(uint64 size, PageFlags flags)
{
	Assert(size && (size & 4095) == 0, "Invalid page size detected.");
	size = (size+4095) & -4096;

	uint64 protection = 1; // Pages are always readable on x86.
	if (flags & PAGE_FLAG_WRITE)   protection |= 2;
	if (flags & PAGE_FLAG_STACK)   protection |= 0x20000;
	if (flags & PAGE_FLAG_EXECUTE) protection |= 4;

	byte* page = (byte*)SystemCall(9, 0, size, protection, 0x22, -1, 0);
	page = (byte*)AssumeAligned(page, 4096);

	Assert(page != (byte*)-1, "Failed to map virtual page.");

	return page;
}

static void DeAllocateVirtualPage(byte* page, uint64 size)
{
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");
	SystemCall(11, (int64)page, size);
}

static const uint64 PAGE_CACHE_BITS = 16; // 256M max
static const uint64 PAGE_CACHE_SIZE = 16; // How many pages are cached per page size.

struct PageCache
{
	uint8 counts[PAGE_CACHE_BITS];
	byte* pages[PAGE_CACHE_SIZE * PAGE_CACHE_BITS];
};

static PageCache page_cache;

static void InitPageCache()
{
	ZeroMemory(&page_cache);

	page_cache.counts[0]  = 8;  // 4k
	page_cache.counts[1]  = 0;  // 8k
	page_cache.counts[2]  = 2;  // 16k
	page_cache.counts[3]  = 0;  // 32k
	page_cache.counts[4]  = 0;  // 64k
	page_cache.counts[5]  = 0;  // 128k
	page_cache.counts[6]  = 0;  // 256k
	page_cache.counts[7]  = 0;  // 512k
	page_cache.counts[8]  = 0;  // 1m
	page_cache.counts[9]  = 16; // 2m
	page_cache.counts[10] = 0;  // 4m
	page_cache.counts[11] = 0;  // 8m
	page_cache.counts[12] = 0;  // 16m
	page_cache.counts[13] = 0;  // 32m
	page_cache.counts[14] = 0;  // 64m
	page_cache.counts[15] = 0;  // 128m

	uint64 sum = 0;

	for (uint64 i = 0; i < PAGE_CACHE_BITS; i++)
		sum += page_cache.counts[i] << (i + 12);

	byte* p = (byte*)AllocateVirtualPage(sum, PAGE_FLAG_WRITE);

	for (uint64 i = 0; i < PAGE_CACHE_BITS; i++)
	{
		for (uint64 j = 0; j < page_cache.counts[i]; j++)
		{
			page_cache.pages[i * PAGE_CACHE_SIZE + j] = p;
			p += 1 << (i + 12);
		}
	}
}

static byte* GetPage(uint64 size)
{
	Assert(size && (size & 4095) == 0, "Invalid page size detected.");

	size = RaisePow2(size);
	int64 index = CountTrailingZeroes64(size >> 12);

	if (index < PAGE_CACHE_BITS) HOT
	{
		if (page_cache.counts[index]) HOT
		{
			byte* page = page_cache.pages[index * PAGE_CACHE_SIZE + page_cache.counts[index] - 1];
			page_cache.counts[index]--;
			return page;
		}

		StaticAssert(PAGE_CACHE_SIZE >= PAGE_CACHE_BITS);

		byte* pages = (byte*)AllocateVirtualPage(size * (PAGE_CACHE_SIZE - index), PAGE_FLAG_WRITE);
		page_cache.counts[index] = PAGE_CACHE_SIZE-index;

		for (uint32 i = 0; i < PAGE_CACHE_SIZE-index; i++)
		{
			page_cache.pages[index * PAGE_CACHE_SIZE + i] = pages;
			pages += size;
		}

		return pages;
	}

	return AllocateVirtualPage(size, PAGE_FLAG_WRITE);
}

static void RetirePage(byte* page, uint64 size)
{
	Assert(page && size);
	Assert((size & 4095) == 0, "Invalid page size detected.");

	size = RaisePow2(size);
	int64 index = CountTrailingZeroes64(size >> 12);

	if (index < PAGE_CACHE_BITS && page_cache.counts[index] < PAGE_CACHE_SIZE) HOT
	{
		page_cache.pages[index * PAGE_CACHE_SIZE + page_cache.counts[index]] = (byte*)page;
		page_cache.counts[index]++;
	}
	else
	{
		DeAllocateVirtualPage(page, size);
	}
}

struct ArenaPool
{
	void*  head;
	void*  data;
	uint32 count;
};

static const uint64 ARENA_MIN_POW    = 5;
static const uint64 ARENA_MIN_SIZE   = (1 << ARENA_MIN_POW);
static const uint64 ARENA_POOL_COUNT = (16 - ARENA_MIN_POW);

struct Arena
{
	ArenaPool pools[ARENA_POOL_COUNT];
};

static Arena arena;

static void InitGlobalArena()
{
	ZeroMemory(&arena);
	byte* pages = (byte*)GetPage((1<<21) * ARENA_POOL_COUNT);

	for (uint32 index = 0; index < ARENA_POOL_COUNT; index++)
	{
		uint32 pow = index + ARENA_MIN_POW;

		arena.pools[index].head = null;
		arena.pools[index].data = pages;
		arena.pools[index].count = 1<<(21-pow);

		pages += (1<<21);
	}
}

static uint64 GetArenaEffectiveSize(uint64 size)
{
	return RaisePow2(size-1 | ARENA_MIN_SIZE-1);
}

static byte* AllocateMemory(uint64 size)
{
	byte* result = null;

	Assert(size);

	size = GetArenaEffectiveSize(size);
	uint32 pow = CountTrailingZeroes64(size);
	uint32 index = pow - ARENA_MIN_POW;


	if (index < ARENA_POOL_COUNT) HOT
	{
		ArenaPool* pool = &arena.pools[index];

		if (pool->head)
		{
			byte* result = (byte*)pool->head;
			pool->head = *(byte**)result;
			return result;
		}
		else if (!pool->count)
		{
			pool->data = GetPage(1<<21);
			pool->count = 1<<(21-pow);
		}

		result = (byte*)pool->data;
		pool->data = (byte*)pool->data + size;
		pool->count--;
	}
	else
	{
		result = (byte*)GetPage(size);
	}

	result = (byte*)AssumeAligned(result, 1ll << ARENA_MIN_POW);
	return result;
}

static void DeAllocateMemory(void* px, uint64 size)
{
	byte* p = (byte*)px; // Sepples
	// Check if p is not null? Or leave that up to the caller?
	Assert(p && size);

	size = GetArenaEffectiveSize(size);
	uint32 pow = CountTrailingZeroes64(size);
	uint32 index = pow - ARENA_MIN_POW;

	if (index < ARENA_POOL_COUNT)
	{
		ArenaPool* pool = &arena.pools[index];
		*(void**)p = pool->head;
		pool->head = p;
	}
	else
	{
		RetirePage(p, size);
	}
}

static byte* ReAllocateMemory(void* px, uint64 old_size, uint64 new_size)
{
	byte* p = (byte*)px; // Sepples
	// @Todo: Deal with shrinking
	Assert(new_size > old_size); // @RemoveMe @FixMe
	Assert(new_size);
	Assert(new_size != old_size, "Redundant ReAllocation\n");


	if (new_size <= GetArenaEffectiveSize(old_size))
	{
		// Print("[SAME]   ReAllocate(%, %)\n", GetArenaEffectiveSize(old_size), GetArenaEffectiveSize(new_size));
		return p;
	}

	byte* result = (byte*)AllocateMemory(new_size);

	if (old_size)
	{
		CopyMemory(result, p, old_size);
		DeAllocateMemory(p, old_size);
	}

	Assert(result);

	return result;
}

static Stack CreateStack(uint64 size)
{
	Assert((size & 4095) == 0);

	Stack stack;
	stack.block = (Stack_Block*)GetPage(size);
	stack.block->previous = null;
	stack.block->size = size;
	stack.end = stack.block->data + size - sizeof(Stack_Block);
	stack.head = stack.block->data;
	return stack;
}

static void* StackAllocate(Stack* stack, uint64 size)
{
	// Print("StackAllocate(%)\n", size);
	byte* result = stack->head;
	stack->head += size;

	if (stack->head < stack->end) HOT
	{
		return result;
	}

	uint64 old_block_size = stack->block->size;
	uint64 new_block_size = (old_block_size << 1) | (size & -old_block_size);

	Stack_Block* old_block = stack->block;
	Stack_Block* new_block = (Stack_Block*)GetPage(new_block_size);

	new_block->previous = old_block;
	new_block->size = new_block_size;
	result = new_block->data;

	stack->end = new_block->data + new_block_size - sizeof(Stack_Block);
	stack->head = new_block->data + size;
	stack->block = new_block;

	return result;
}

static void FreeStack(Stack* stack)
{
	Stack_Block* block = stack->block;

	while (block)
	{
		Stack_Block* previous = block->previous;
		RetirePage((byte*)block, block->size);
		block = previous;
	}
}

// extern "C" void memcpy(void* dest, const void* src, unsigned long int count)
// {
// 	for (unsigned long int i = 0; i < count; i++)
// 	{
// 		((char*)dest)[i] = ((const char*)src)[i];
// 	}
// }

// extern "C" int memcmp(const void* p0, const void* p1, unsigned long int count)
// {
// 	// Print("memcmp(\"%\", \"%\", %)\n", String((const char*)p0, count), String((const char*)p1, count), count);

// 	const uint8* a8 = (const uint8*)p0;
// 	const uint8* b8 = (const uint8*)p1;

// 	uint64 i = 0;

// 	while (i + 64 <= count)
// 	{
// 		const uint64* a64 = (const uint64*)(a8+i);
// 		const uint64* b64 = (const uint64*)(b8+i);

// 		int64 buffer[8] = {
// 			(int64)ReverseBytes64(a64[0]) - (int64)ReverseBytes64(b64[0]),
// 			(int64)ReverseBytes64(a64[1]) - (int64)ReverseBytes64(b64[1]),
// 			(int64)ReverseBytes64(a64[2]) - (int64)ReverseBytes64(b64[2]),
// 			(int64)ReverseBytes64(a64[3]) - (int64)ReverseBytes64(b64[3]),
// 			(int64)ReverseBytes64(a64[4]) - (int64)ReverseBytes64(b64[4]),
// 			(int64)ReverseBytes64(a64[5]) - (int64)ReverseBytes64(b64[5]),
// 			(int64)ReverseBytes64(a64[6]) - (int64)ReverseBytes64(b64[6]),
// 			(int64)ReverseBytes64(a64[7]) - (int64)ReverseBytes64(b64[7]),
// 		};

// 		if (buffer[0] < 0) return -1;
// 		if (buffer[1] < 0) return -1;
// 		if (buffer[2] < 0) return -1;
// 		if (buffer[3] < 0) return -1;
// 		if (buffer[4] < 0) return -1;
// 		if (buffer[5] < 0) return -1;
// 		if (buffer[6] < 0) return -1;
// 		if (buffer[7] < 0) return -1;

// 		if (buffer[0] > 0) return  1;
// 		if (buffer[1] > 0) return  1;
// 		if (buffer[2] > 0) return  1;
// 		if (buffer[3] > 0) return  1;
// 		if (buffer[4] > 0) return  1;
// 		if (buffer[5] > 0) return  1;
// 		if (buffer[6] > 0) return  1;
// 		if (buffer[7] > 0) return  1;

// 		i += 64;
// 	}

// 	for (; i < count; i++)
// 	{
// 		if (a8[i] < b8[i]) return -1;
// 		if (a8[i] > b8[i]) return 1;
// 	}

// 	return 0;
// }

// extern "C" void* memset(void* begin, int v, unsigned long int count)
// {
// 	// @OptimizeMe?
// 	byte* p = (byte*)begin;
// 	for (uint64 i = 0; i < count; i++) p[i] = v;
// 	return p + count;
// }

// extern "C" int bcmp(const void* p0, const void* p1, unsigned long int count)
// {
// 	for (unsigned long int i = 0; i < count; i++)
// 	{
// 		if (((const char*)p0)[i] != ((const char*)p1)[i]) return 1;
// 	}

// 	return 0;
// }

// extern "C" void memmove(void* p0, const void* p1, unsigned long int count)
// {
// 	for (unsigned long int i = 0; i < count; i++)
// 	{
// 		((char*)p0)[i] = ((const char*)p1)[i];
// 	}
// }


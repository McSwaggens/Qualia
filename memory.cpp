#include "memory.h"
#include "util.h"
#include "assert.h"

// [ ffffffff_ffffffff_F7654321_08765432_10876543_21087654_3210XXXX_XXXXXXXX ]
// [ ffffffff_ffffffff_FTTTTTTT_TGGGDDDD_DDCCCCCC_BBBBBBAA_AAAAXXXX_XXXXXXXX ]
// [ ffffffff_ffffffff_FTTTTTTT_TGGGGGGG_GGMMMMMM_MMMKKKKK_KKKKXXXX_XXXXXXXX ]
// [ ffffffff_ffffffff_FTTTTTTT_T1631864_21521631_84215216_3184XXXX_XXXXXXXX ]
// [ 00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000 ]

static void* AllocateVirtualPage(u64 size, PageFlags flags)
{
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");

	// Print("AllocateVirtualPage(%k)\n", size >> 10);

	u32 protection = 1; // Pages are always readable on x86.
	if (flags & PAGE_WRITE_FLAG)   protection |= 2;
	if (flags & PAGE_EXECUTE_FLAG) protection |= 4;

	u32 inflags = 0x22;
	// if (prefault) inflags |= 0x8000;

	void* page = (void*)SystemCall(9, 0, size, protection, inflags, -1, 0);

	Assert(page != (void*)-1, "Failed to map virtual page.");

	return page;
}

static void DeAllocateVirtualPage(void* page, u64 size, PageFlags flags)
{
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");
	SystemCall(11, (u64)page, size);
}

static const u64 PAGE_CACHE_BITS = 16; // 256M max
static const u64 PAGE_CACHE_SIZE = 16; // How many pages are cached per page size.

struct PageCache
{
	u8 counts[PAGE_CACHE_BITS];
	void* pages[PAGE_CACHE_SIZE * PAGE_CACHE_BITS];
};

static PageCache page_cache;

static void InitPageCache()
{
	ZeroMemory(&page_cache);

	page_cache.counts[0]  = 8; // 4k
	page_cache.counts[1]  = 0; // 8k
	page_cache.counts[2]  = 2; // 16k
	page_cache.counts[3]  = 0; // 32k
	page_cache.counts[4]  = 0; // 64k
	page_cache.counts[5]  = 0; // 128k
	page_cache.counts[6]  = 0; // 256k
	page_cache.counts[7]  = 0; // 512k
	page_cache.counts[8]  = 0; // 1m
	page_cache.counts[9]  = 16; // 2m
	page_cache.counts[10] = 0; // 4m
	page_cache.counts[11] = 0; // 8m
	page_cache.counts[12] = 0; // 16m
	page_cache.counts[13] = 0; // 32m
	page_cache.counts[14] = 0; // 64m
	page_cache.counts[15] = 0; // 128m

	u64 sum = 0;

	for (u64 i = 0; i < PAGE_CACHE_BITS; i++)
	{
		sum += page_cache.counts[i] << (i + 11);
		Assert(page_cache.counts[i] <= PAGE_CACHE_SIZE);
	}

	char* p = (char*)AllocateVirtualPage(sum, PAGE_WRITE_FLAG);

	for (s64 i = PAGE_CACHE_BITS-1; i >= 0; i--)
	{
		for (u32 j = 0; j < page_cache.counts[i]; j++)
		{
			page_cache.pages[i * PAGE_CACHE_SIZE + j] = p;
			p += 1 << (i + 11);
		}
	}
}

static void* GetPage(u64 size)
{
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");

	size = NextPow2(size);
	s64 index = CountTrailingZeroes(size >> 12);

	if (index < PAGE_CACHE_BITS)
	{
		if (page_cache.counts[index])
		{
			void* page = page_cache.pages[index * PAGE_CACHE_SIZE + page_cache.counts[index] - 1];
			page_cache.counts[index]--;

			return page;
		}
		else
		{
			StaticAssert(PAGE_CACHE_SIZE >= PAGE_CACHE_BITS);
			Assert(index < PAGE_CACHE_SIZE);

			char* pages = (char*)AllocateVirtualPage(size * (PAGE_CACHE_SIZE - index), PAGE_WRITE_FLAG);
			page_cache.counts[index] = (PAGE_CACHE_SIZE-index-1);

			for (u32 i = 0; i < (PAGE_CACHE_SIZE-index-1); i++)
			{
				page_cache.pages[index * PAGE_CACHE_SIZE + i] = pages;
				pages += size;
			}

			return pages;
		}
	}

	return AllocateVirtualPage(size, PAGE_WRITE_FLAG);
}

static void RetirePage(void* page, u64 size)
{
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");

	size = NextPow2(size);
	s64 index = CountTrailingZeroes(size >> 12);

	if (index < PAGE_CACHE_BITS && page_cache.counts[index] < PAGE_CACHE_SIZE) HOT
	{
		page_cache.pages[index * PAGE_CACHE_SIZE + page_cache.counts[index]] = page;
		page_cache.counts[index]++;
	}
	else
	{
		DeAllocateVirtualPage(page, size, PAGE_WRITE_FLAG);
	}
}

struct ArenaPool
{
	void* head;
	void* data;
	u32   count;
};

#define ARENA_MIN_POW 3
#define ARENA_POOL_COUNT (16 - ARENA_MIN_POW)

struct Arena
{
	ArenaPool pools[ARENA_POOL_COUNT];
};

static Arena arena;

static void InitGlobalArena()
{
	ZeroMemory(&arena);
	char* pages = (char*)GetPage((1<<21) * ARENA_POOL_COUNT);

	for (u32 index = 0; index < ARENA_POOL_COUNT; index++)
	{
		u32 pow = index + ARENA_MIN_POW;

		arena.pools[index].head = null;
		arena.pools[index].data = pages;
		arena.pools[index].count = 1<<(21-pow);

		pages += (1<<21);
	}
}

static u64 GetArenaEffectiveSize(u64 size)
{
	return NextPow2((size-1) | 7);
}

static void* Allocate(u64 size)
{
	// Check if size is 0?
	Assert(size);

	// Print("Allocate(%) -> %\n", size, GetArenaEffectiveSize(size));

	// if (size == 768)
	// {
	// 	DebugBreak();
	// }

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes(size);
	u32 index = pow - ARENA_MIN_POW;

	void* result = null;

	if (index < ARENA_POOL_COUNT)
	{
		ArenaPool* pool = &arena.pools[index];

		if (pool->head)
		{
			void* result = pool->head;
			pool->head = *(char**)result;
			return result;
		}
		else if (!pool->count)
		{
			pool->data = GetPage(1<<21);
			pool->count = 1<<(21-pow);
		}

		result = pool->data;
		pool->data = (char*)pool->data + size;
		pool->count--;
	}
	else
	{
		result = GetPage(size);
	}

	// Print("result = %, size = %, pow = %\n", (u64)result, size, pow);
	// standard_output_buffer.Flush();

	return result;
}

static void DeAllocate(void* p, u64 size)
{
	// Check if p is not null? Or leave that up to the caller?
	Assert(p);
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes(size);
	u32 index = pow - ARENA_MIN_POW;

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

static void* ReAllocate(void* p, u64 old_size, u64 new_size)
{
	// @Todo: Deal with shrinking
	Assert(new_size > old_size); // @RemoveMe @FixMe
	Assert(new_size);
	Assert(new_size != old_size, "Redundant ReAllocation\n");

	if (new_size <= GetArenaEffectiveSize(old_size))
	{
		return p;
	}

	char* result = (char*)Allocate(new_size);

	if (old_size)
	{
		CopyMemory(result, (char*)p, old_size);
		DeAllocate(p, old_size);
	}

	Assert(result);

	// Print("ReAllocate(%, %, %) -> %\n", (u64)p, old_size, new_size, (u64)result);
	// standard_output_buffer.Flush();

	return result;
}

static Stack CreateStack(u64 size)
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

static void* StackAllocate(Stack* stack, u64 size)
{
	char* result = stack->head;
	stack->head += size;

	if (stack->head < stack->end) HOT
	{
		return result;
	}

	u64 old_block_size = stack->block->size;
	u64 new_block_size = (old_block_size << 1) | (size & -old_block_size);

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
		RetirePage(block, block->size);
		block = previous;
	}
}


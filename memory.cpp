#include "memory.h"
#include "general.h"
#include "assert.h"

static byte* AllocateVirtualPage(u64 size, PageFlags flags) {
	Assert(size && (size & 4095) == 0, "Invalid page size detected.");
	size = (size+4095) & -4096;

	u64 protection = 1; // Pages are always readable on x86.
	if (flags & PAGE_FLAG_WRITE)   protection |= 2;
	if (flags & PAGE_FLAG_STACK)   protection |= 0x20000;
	if (flags & PAGE_FLAG_EXECUTE) protection |= 4;

	byte* page = (byte*)SystemCall(9, 0, size, protection, 0x22, -1, 0);
	page = (byte*)AssumeAligned(page, 4096);

	Assert(page != (byte*)-1, "Failed to map virtual page.");

	return page;
}

static void DeAllocateVirtualPage(byte* page, u64 size) {
	Assert(size);
	Assert((size & 4095) == 0, "Invalid page size detected.");
	SystemCall(11, (s64)page, size);
}

struct ArenaPool
{
	void* head;
	void* data;
	u32 count;
};

static const u64 ARENA_MIN_POW    = 5;
static const u64 ARENA_MIN_SIZE   = (1 << ARENA_MIN_POW);
static const u64 ARENA_POOL_COUNT = (16 - ARENA_MIN_POW);

struct Arena
{
	ArenaPool pools[ARENA_POOL_COUNT];
};

static Arena arena;

static void InitGlobalArena() {
	ZeroMemory(&arena);
	byte* pages = AllocateVirtualPage((1<<21) * ARENA_POOL_COUNT, PAGE_FLAG_WRITE);
	// byte* pages = (byte*)AllocateVirtualPage((1<<21) * ARENA_POOL_COUNT, PAGE_FLAG_WRITE);

	for (u32 index = 0; index < ARENA_POOL_COUNT; index++) {
		ArenaPool* pool = &arena.pools[index];

		u32 pow = index + ARENA_MIN_POW;

		pool->head = null;
		pool->data = pages;
		pool->count = 1<<(21-pow);

		*(char*)pool->data = 42;

		pages += (1<<21);
	}
}

static u64 GetArenaEffectiveSize(u64 size) {
	return RaisePow2(size-1 | ARENA_MIN_SIZE-1);
}

static byte* AllocateMemory(u64 size) {
	byte* result = null;

	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes64(size);
	u32 index = pow - ARENA_MIN_POW;


	if (index < ARENA_POOL_COUNT) HOT
	{
		ArenaPool* pool = &arena.pools[index];

		if (pool->head) {
			byte* result = (byte*)pool->head;
			pool->head = *(byte**)result;
			return result;
		}
		else if (!pool->count) {
			pool->data = AllocateVirtualPage(1<<21, PAGE_FLAG_WRITE);
			pool->count = 1<<(21-pow);
		}

		result = (byte*)pool->data;
		pool->data = (byte*)pool->data + size;
		pool->count--;
	}
	else
	{
		result = (byte*)AllocateVirtualPage(size, PAGE_FLAG_WRITE);
	}

	result = (byte*)AssumeAligned(result, 1ll << ARENA_MIN_POW);
	return result;
}

static void DeAllocateMemory(void* px, u64 size) {
	byte* p = (byte*)px; // Sepples

	if (!p)
		return;

	// Check if p is not null? Or leave that up to the caller?
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes64(size);
	u32 index = pow - ARENA_MIN_POW;

	if (index < ARENA_POOL_COUNT) {
		ArenaPool* pool = &arena.pools[index];
		*(void**)p = pool->head;
		pool->head = p;
	}
	else
	{
		DeAllocateVirtualPage(p, size);
	}
}

static byte* ReAllocateMemory(void* px, u64 old_size, u64 new_size) {
	byte* p = (byte*)px; // Sepples
	// @Todo: Deal with shrinking
	Assert(new_size > old_size); // @RemoveMe @FixMe
	Assert(new_size);
	Assert(new_size != old_size, "Redundant ReAllocation\n");


	if (new_size <= GetArenaEffectiveSize(old_size)) {
		// Print("[SAME]   ReAllocate(%, %)\n", GetArenaEffectiveSize(old_size), GetArenaEffectiveSize(new_size));
		return p;
	}

	byte* result = (byte*)AllocateMemory(new_size);

	if (old_size) {
		CopyMemory(result, p, old_size);
		DeAllocateMemory(p, old_size);
	}

	Assert(result);

	return result;
}

static Stack CreateStack(u64 size) {
	Assert((size & 4095) == 0);

	Stack stack;
	stack.block = (Stack_Block*)AllocateVirtualPage(size, PAGE_FLAG_WRITE);
	stack.block->previous = null;
	stack.block->size = size;
	stack.end = stack.block->data + size - sizeof(Stack_Block);
	stack.head = stack.block->data;
	return stack;
}

static void* StackAllocate(Stack* stack, u64 size) {
	// Print("StackAllocate(%)\n", size);
	byte* result = stack->head;
	stack->head += size;

	if (stack->head < stack->end) HOT
	{
		return result;
	}

	u64 old_block_size = stack->block->size;
	u64 new_block_size = (old_block_size << 1) | (size & -old_block_size);

	Stack_Block* old_block = stack->block;
	Stack_Block* new_block = (Stack_Block*)AllocateVirtualPage(new_block_size, PAGE_FLAG_WRITE);

	new_block->previous = old_block;
	new_block->size = new_block_size;
	result = new_block->data;

	stack->end = new_block->data + new_block_size - sizeof(Stack_Block);
	stack->head = new_block->data + size;
	stack->block = new_block;

	return result;
}

// Remove this functionality?
static void FreeStack(Stack* stack) {
	Stack_Block* block = stack->block;

	while (block) {
		DeAllocateVirtualPage((byte*)block, block->size);
		block = block->previous;
	}
}


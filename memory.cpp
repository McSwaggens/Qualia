#include "memory.h"
#include "util.h"
#include "assert.h"

//
// On x86 both the page and directory table descriptors' have an 'R' flag.
//        What do you think the 'R' flag does?
//        If you guessed it's the 'R'ead flag, you clearly don't understand how fucking stupid Intel is.
//        It's even more funny because thay call it the 'read/write' flag, when in actuallity it just changes the write permission.
// So... what this means is that if you pass *JUST* the 'PROT_WRITE' flag into mmap, it's actually the same as 'PROT_READ | PROT_WRITE'.
//       PROT_READ doesn't actually do anything...
//       And of course the man pages aren't helpfull at all (as usual) and don't document this at all.
//       Searching this on the internet won't help either. I literally had to infer this from the Intel x86 spec.
//
//                                             x86 was a mistake.
//
void* AllocateVirtualPage(u64 size, bool write, bool execute, bool prefault)
{
	Assert(size == (size & -PageSize), "Invalid page size detected.");

	u32 protection = 1; // Might as well put READ flag on by default, not like it matters anyway... >:(
	if (write)   protection |= 2;
	if (execute) protection |= 4;

	u32 flags = 0x22 | (1<<21);
	if (prefault) flags |= 0x8000;

	void* page = (void*)SystemCall(9, 0, size, protection, flags, -1, 0);
	Assert(page != (void*)-1, "Failed to map virtual page.");
	return page;
}

void DeAllocateVirtualPage(void* page, u64 size)
{
	SystemCall(11, (u64)page, size);
}

Stack_Allocator NewStackAllocator(u64 size)
{
	Stack_Allocator stack;
	stack.block = (Stack_Allocator_Block*)AllocateVirtualPage(size);
	stack.block->previous = null;
	stack.block_size = size;
	stack.head = stack.block->data;
	return stack;
}

struct ArenaPool
{
	void* head;
	void* data;
	u32   count;
};

struct Arena
{
	ArenaPool pools[16-3];
} arena;

void InitGlobalArena()
{
	ZeroMemory(&arena);
	char* pages = (char*)AllocateVirtualPage((1<<21)*13);

	for (u32 index = 0; index < 13; index++)
	{
		u32 pow = index + 3;

		arena.pools[index].head = null;
		arena.pools[index].data = pages;
		arena.pools[index].count = 1<<(21-pow);

		pages += (1<<21);
	}
}

u64 GetArenaEffectiveSize(u64 size)
{
	return NextPow2((size-1) | 7);
}

void* Allocate(u64 size)
{
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes(size);
	u32 index = pow - 3;

	ArenaPool* pool = &arena.pools[index];

	if (pool->head)
	{
		void* result = pool->head;
		pool->head = *(char**)result;
		return result;
	}
	else if (!pool->count)
	{
		pool->data = AllocateVirtualPage(1<<21);
		pool->count = 1<<(21-pow);
	}

	void* result = pool->data;
	pool->data = (char*)pool->data + size;
	pool->count--;
	return result;
}

void DeAllocate(void* p, u64 size)
{
	Assert(p);
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = CountTrailingZeroes(size);
	u32 index = pow - 3;

	ArenaPool* pool = &arena.pools[index];
	*(void**)p = pool->head;
	pool->head = p;
}

void* ReAllocate(void* p, u64 old_size, u64 new_size)
{
	// @Todo: Deal with shrinking
	Assert(new_size > old_size); // @RemoveMe @FixMe

	Assert(new_size);


	if (new_size <= old_size)
	{
		Assert(new_size != old_size, "Redundant ReAllocation\n");

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


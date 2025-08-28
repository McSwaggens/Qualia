#include "memory.h"
#include "general.h"
#include "assert.h"
#include "math.h"
#include "print.h"

#include "os.h"

static const u64 GA_MIN_POW  = 4;
static const u64 GA_MIN_SIZE = 1llu << GA_MIN_POW;
static const u64 GA_LOWER_MASK =  (OS::PAGE_SIZE-1) & -GA_MIN_SIZE;
static const u64 GA_UPPER_MASK = -OS::PAGE_SIZE;

struct GlobalAllocatorPool {
	byte* stack_head = null;
	byte* stack_tail = null;
	byte* linked_list_head = null;

	bool IsStackEmpty()      { return stack_head >= stack_tail; }
	bool IsLinkedListEmpty() { return !linked_list_head; }
	bool IsEmpty() { return IsLinkedListEmpty() && IsStackEmpty(); }

	void InsertLinkedList(byte* p) {
		*(byte**)p = linked_list_head;
		linked_list_head = p;
	}

	void SetStack(byte* p, u64 size) {
		Assert(stack_head == stack_tail);
		Assert(size);
		Assert(p);

		stack_head = p;
		stack_tail = p + size;
	}

	byte* TakeLinkedList() {
		Assert(!IsLinkedListEmpty());

		byte* result = linked_list_head;
		linked_list_head = *(byte**)linked_list_head;

		return result;
	}

	byte* TakeStack(u64 size) {
		Assert(!IsStackEmpty());

		byte* result = stack_head;
		stack_head += size;

		return result;
	}
};

struct GlobalAllocator {
	u64 map = 0;
	GlobalAllocatorPool pools[64] = { };

	void Init() {
		map = GA_LOWER_MASK;

		u64   block_size = PopCount(map) * OS::PAGE_SIZE;
		byte* block = OS::AllocateVirtualMemory(block_size);

		byte* p = block;
		for (u32 i = 0; i < PopCount(map); i++) {
			pools[GA_MIN_POW + i].SetStack(p, OS::PAGE_SIZE);
			p += OS::PAGE_SIZE;
		}

		Assert(p == block + block_size);
	}

	u64 NormalizeSize(u64 size) {
		return RaisePow2((size + (GA_MIN_SIZE-1)) & -GA_MIN_SIZE);
	}

	void InsertSingle(u64 bit, u64 pool_index, byte* block) {
		GlobalAllocatorPool* pool = &pools[pool_index];

		if (pool->IsStackEmpty()) pool->SetStack(block, bit);
		else                      pool->InsertLinkedList(block);

		map |= bit;
	}

	byte* Take(u64 pool_index) {
		GlobalAllocatorPool* pool = &pools[pool_index];
		u64 bit = 1llu << pool_index;

		Assert(map & bit);
		Assert(!pool->IsEmpty());

		byte* result;
		if (!pool->IsLinkedListEmpty()) result = pool->TakeLinkedList();
		else                            result = pool->TakeStack(bit);

		if (pool->IsEmpty())
			map ^= bit;

		Assert(result);
		return result;
	}

	void Fill(u64 bit, u64 index) {
		Assert(pools[index].IsEmpty());
		Assert(~map & bit);
		Assert(PopCount(bit) == 1);

		u64 upper_map = map & GA_UPPER_MASK & -bit;

		if (!upper_map) {
			u64 block_size = Max(bit << 4llu, OS::PAGE_SIZE); // This is not great.. :(
			byte* block = (byte*)OS::AllocateVirtualMemory(block_size);
			Assert(block_size > bit * 2);
			Assert(block);

			pools[index].SetStack(block, block_size);
			map |= bit;

			Assert(!pools[index].IsEmpty());
			return;
		}

		// Spill.
		u64 take_index = Ctz64(upper_map);
		u64 block_size = 1llu << take_index;
		byte* block = Take(take_index);
		Assert(block_size > bit);

		pools[index].SetStack(block, block_size);
		Assert(!pools[index].IsEmpty());
		map |= bit;

		return;
	}

	byte* Allocate(u64 size) {
		u64 bit   = NormalizeSize(size);
		u64 index = Ctz64(bit);

		if (!(map & bit))
			Fill(bit, index);

		return Take(index);
	}

	void Free(byte* p, u64 size) {
		u64 bit   = NormalizeSize(size);
		u64 index = Ctz64(bit);

		InsertSingle(bit, index, p);
	}
} static global_allocator;

static void* AllocMemory(u64 size) {
	// Print("AllocMemory(size = %)\n", size);
	return global_allocator.Allocate(size);
}

static void FreeMemory(void* p, u64 size) {
	// Print("FreeMemory(p = %, size = %)\n", p, size);
	if (!p) return;
	global_allocator.Free((byte*)p, size);
}

static void* ReAllocMemory(void* p, u64 old_size, u64 new_size) {
	// Print("ReAllocMemory(p = %, old_size = %, new_size = %)\n", p, old_size, new_size);
	u64 old_real_size = old_size;

	old_size = global_allocator.NormalizeSize(old_size);
	new_size = global_allocator.NormalizeSize(new_size);

	if (old_size == new_size)
		return p;

	void* result = global_allocator.Allocate(new_size);

	if (old_size)
	{
		CopyMemory((byte*)result, (byte*)p, old_real_size);
		FreeMemory(p, old_size);
	}
	else Assert(!p);

	return result;
}

static void* CopyAllocMemory(void* p, u64 size) {
	// Print("CopyAllocMemory(p = %, size = %)\n", p, size);
	void* result = global_allocator.Allocate(size);
	CopyMemory((byte*)result, (byte*)p, size);

	return result;
}

static void InitGlobalAllocator() {
	global_allocator.Init();
}

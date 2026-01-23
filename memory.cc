#include "general.h"
#include "memory.h"
#include "assert.h"
#include "math.h"
#include "print.h"
#include "global_allocator.h"

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


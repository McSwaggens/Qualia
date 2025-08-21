#include "memory.h"
#include "general.h"
#include "assert.h"

#include "os.h"

struct ArenaPool {
	// @cleanme Put functions inside here
	void* head;
	void* data;
	u32 count;
};

static const u64 ARENA_MIN_POW    = 5;
static const u64 ARENA_MIN_SIZE   = (1 << ARENA_MIN_POW);
static const u64 ARENA_POOL_COUNT = (16 - ARENA_MIN_POW);

struct Arena {
	// @cleanme Put functions inside here
	ArenaPool pools[ARENA_POOL_COUNT];
};

static Arena arena;

static void InitGlobalArena() {
	ZeroMemory(&arena);
	byte* pages = OS::AllocateVirtualMemory((1<<21) * ARENA_POOL_COUNT);

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
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = Ctz64(size);
	u32 index = pow - ARENA_MIN_POW;

	if (index >= ARENA_POOL_COUNT)
		return OS::AllocateVirtualMemory(size);


	ArenaPool* pool = &arena.pools[index];

	if (pool->head) {
		byte* result = (byte*)pool->head;
		pool->head = *(byte**)result;
		return result;
	}

	if (!pool->count) {
		pool->data = OS::AllocateVirtualMemory(1<<21);
		pool->count = 1<<(21-pow);
	}

	byte* result = (byte*)pool->data;
	pool->data = (byte*)pool->data + size;
	pool->count--;

	result = (byte*)AssumeAligned(result, 1ll << ARENA_MIN_POW);
	return result;
}

static void DeAllocateMemory(void* px, u64 size) {
	byte* p = (byte*)px; // Sepples

	if (!p) return;

	// Check if p is not null? Or leave that up to the caller?
	Assert(size);

	size = GetArenaEffectiveSize(size);
	u32 pow = Ctz64(size);
	u32 index = pow - ARENA_MIN_POW;

	if (index >= ARENA_POOL_COUNT) {
		OS::FreeVirtualMemory(p, size);
		return;
	}

	ArenaPool* pool = &arena.pools[index];
	*(void**)p = pool->head;
	pool->head = p;
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


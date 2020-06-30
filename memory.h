#pragma once
#include "int.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>

#include "array.h"

#define ATTRIBUTE_ALLOCATOR __attribute__((malloc))

static constexpr u64 PageSize = 4096;

void* AllocateVirtualPage(u64 size);
void DeAllocateVirtualPage(void* page, u64 size);

template<typename T>
static constexpr void CopyMemory(T* dest, const T* src, u64 count = 1)
{
	if (IsConstEval())
	{
		for (u64 i = 0; i < count; i++) dest[i] = src[i];
	}
	else __builtin_memcpy(dest, src, sizeof(T) * count);
}

template<typename T>
static constexpr void FillMemory(T* dest, u64 count, T value)
{
	for (u64 i = 0; i < count; i++) dest[i] = value;
}

template<typename T>
static constexpr void FillMemory(T* begin, T* end, T value)
{
	for (; begin < end; begin++) *begin = value;
}

template<typename T>
static constexpr bool CompareMemory(const T* a, const T* b, u64 count = 1)
{
	return __builtin_memcmp(a, b, sizeof(T) * count) == 0;
}

template<typename T>
static constexpr bool Compare(const T* a, const T* b, u64 count = 1)
{
	for (u64 i = 0; i < count; i++)
	{
		if (!Compare(a[i], b[i])) return false;
	}
	return true;
}

// template<typename T>
// static constexpr bool Compare(T a, T b)
// {
// 	return CompareMemory(&a, &b);
// }

template<typename T>
static constexpr void ZeroMemory(T* begin, T* end)
{
	if (IsConstEval())
	{
		char* p = reinterpret_cast<char*>(begin);
		char* e = reinterpret_cast<char*>(end);
		for (; p < e; p++) *p = 0;
		// @FixMe @Performance
	}
	else __builtin_memset(begin, 0, sizeof(T) * (end - begin));
}

template<typename T>
static constexpr void ZeroMemory(T* p, u64 count = 1)
{
	if (IsConstEval())
	{
		char* bytes = reinterpret_cast<char*>(p);
		for (u64 i = 0; i < count * sizeof(T); i++) bytes[i] = 0;
		// @FixMe @Performance
	}
	else __builtin_memset(p, 0, sizeof(T) * count);
}

template<typename T>
[[nodiscard]]
static inline T* Allocate(u64 count = 1)
{
	return static_cast<T*>(malloc(sizeof(T) * count));
}

template<typename T>
static inline void ReAllocate(T*& p, u64 count)
{
	p = static_cast<T*>(realloc(p, sizeof(T) * count));
}

template<typename T>
static inline void Free(T* p)
{
	free(p);
}

struct Stack_Allocator_Block
{
	Stack_Allocator_Block* previous;
	char data[];
};

struct Stack_Allocator
{
	Stack_Allocator_Block* block;
	char* head;
	u64 block_size;

	void Init(u64 size = 4096 * 1024 * 4)
	{
		block = Cast(Stack_Allocator_Block*, AllocateVirtualPage(size));
		block->previous = null;
		block_size = size;
		head = block->data;
	}

	void Free()
	{
		while (block)
		{
			Stack_Allocator_Block* previous = block->previous;
			DeAllocateVirtualPage(block, block_size);
			block = previous;
		}
	}

	template<typename T>
	T* Allocate(u32 count = 1) ATTRIBUTE_ALLOCATOR
	{
		u64 size = sizeof(T) * count;

		// @Todo @Bug: Check if allocation is larger than block_size.
		if (head + size >= block->data + block_size)
		{
			Stack_Allocator_Block* new_block = Cast(Stack_Allocator_Block*, AllocateVirtualPage(size));
			new_block->previous = block;
			head = new_block->data;
			block = new_block;
		}

		T* p = Cast(T*, head);
		head += size;

		return p;
	}

	template<typename T>
	Array<T> CreateArray(u32 count)
	{
		Array<T> array;
		array.count = count;
		array.data = array.count ? Allocate<T>(count) : null;
		return array;
	}
};



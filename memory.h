#pragma once

#include "int.h"
#include "util.h"
#include "array.h"

#define ALLOCATOR __attribute__((malloc))
#define ALLIGNED(n) __attribute__((assume_aligned((n))))

ALLIGNED(4096)
void* AllocateVirtualPage(u64 size, bool write = true, bool execute = false, bool prefault = false);
void DeAllocateVirtualPage(void* page, u64 size);

void InitGlobalArena();

ALLIGNED(8)
void* Allocate(u64 size);
ALLIGNED(8)
void* ReAllocate(void* p, u64 old_size, u64 new_size);
void DeAllocate(void* p, u64 size);

template<typename T>
[[nodiscard]]
static inline T* Allocate(u64 count = 1)
{
	return static_cast<T*>(Allocate(sizeof(T) * count));
}

template<typename T>
static inline void ReAllocate(T*& p, u32 old_count, u64 new_count)
{
	p = static_cast<T*>(ReAllocate((void*)p, sizeof(T) * old_count, sizeof(T) * new_count));
}

template<typename T>
static inline void DeAllocate(T* p, u64 count = 1)
{
	DeAllocate((void*)p, count*sizeof(T));
}

template<typename T>
static inline Array<T> AllocateArray(u32 count)
{
	Array<T> array;
	array.data = Allocate<T>(count);
	array.count = count;
	return array;
}

template<typename T>
static inline void DeAllocateArray(Array<T> array)
{
	DeAllocate(array.data, array.count);
}

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

template<typename T>
static constexpr void ZeroMemory(T* begin, T* end)
{
	if (IsConstEval())
	{
		char* p = reinterpret_cast<char*>(begin);
		char* e = reinterpret_cast<char*>(end);
		for (; p < e; p++) *p = 0;
	}
	else __builtin_memset(begin, 0, sizeof(T) * (end - begin));
}

template<typename T>
static constexpr void ZeroMemory(T* p, u64 count = 1)
{
	ZeroMemory(p, p + count);
}

template<typename T>
static constexpr void ZeroMemory(Array<T> array)
{
	ZeroMemory(array.data, array.count);
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
	T* Allocate(u32 count = 1) ALLOCATOR
	{
		u64 size = sizeof(T) * count;

		if (head + size >= block->data + block_size - sizeof(Stack_Allocator_Block))
		{
			block_size <<= 1;
			block_size = block_size | (size & -block_size);
			Stack_Allocator_Block* new_block = (Stack_Allocator_Block*)AllocateVirtualPage(block_size);
			new_block->previous = block;
			head = new_block->data;
			block = new_block;
		}

		T* p = (T*)head;
		head += size;

		return p;
	}

	template<typename T>
	Array<T> AllocateArray(u32 count)
	{
		Array<T> array;
		array.data = count ? Allocate<T>(count) : null;
		array.count = count;
		return array;
	}
};

Stack_Allocator NewStackAllocator(u64 size = 1<<21);


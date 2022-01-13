#pragma once

#include "util.h"
#include "array.h"

#define ALLOCATOR __attribute__((malloc))
#define ALLIGNED(n) __attribute__((assume_aligned((n))))

enum PageFlags
{
	PAGE_WRITE_FLAG   = (1 << 0),
	PAGE_EXECUTE_FLAG = (1 << 1),
};

ALLIGNED(4096)
static void* AllocateVirtualPage(u64 size, PageFlags flags);
static void DeAllocateVirtualPage(void* page, u64 size, PageFlags flags);

static void* GetPage(u64 size);
static void RetirePage(void* page, u64 size);

static void InitPageCache();
static void InitGlobalArena();

struct Stack_Block
{
	Stack_Block* previous;
	u64 size;
	char data[];
};

struct Stack
{
	Stack_Block* block;
	char* head;
	char* end;
};

static Stack CreateStack(u64 size = 1<<21);
static void FreeStack(Stack* stack);
static void* StackAllocate(Stack* stack, u64 size) ALLOCATOR;

template<typename T>
static T* StackAllocate(Stack* stack)
{
	return (T*)StackAllocate(stack, sizeof(T));
}

template<typename T>
static T* StackAllocate(Stack* stack, u64 count)
{
	return (T*)StackAllocate(stack, sizeof(T) * count);
}

template<typename T>
static Array<T> StackAllocateArray(Stack* stack, u64 count)
{
	Array<T> array;
	array.data = count ? StackAllocate<T>(stack, count) : null;
	array.count = count;
	return array;
}

ALLIGNED(8)
static void* Allocate(u64 size);
ALLIGNED(8)
static void* ReAllocate(void* p, u64 old_size, u64 new_size);
static void DeAllocate(void* p, u64 size);

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
static inline Array<T> AllocateArray(u64 count)
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
static constexpr void ZeroArray(Array<T> array)
{
	ZeroMemory(array.data, array.count);
}


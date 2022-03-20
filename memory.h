#pragma once

#include "general.h"
#include "array.h"

// ------------------------------------------- //

enum PageFlags
{
	PAGE_FLAG_WRITE   = Bit(0),
	PAGE_FLAG_EXECUTE = Bit(1),
	PAGE_FLAG_STACK   = Bit(2),
};

static void InitPageCache();
static void InitGlobalArena();

static byte* AllocateVirtualPage(uint64 size, PageFlags flags);
static void  DeAllocateVirtualPage(byte* page, uint64 size, PageFlags flags);

static byte* GetPage(uint64 size);
static void  RetirePage(byte* page, uint64 size);

// ------------------------------------------- //

static byte* AllocateMemory(uint64 size);
static byte* ReAllocateMemory(byte* p, uint64 old_size, uint64 new_size);
static void  DeAllocateMemory(byte* p, uint64 size);

// ------------------------------------------- //

template<typename T>
static inline T* Allocate(uint64 count = 1)
{
	return (T*)AllocateMemory(count * sizeof(T));
}

template<typename T>
static inline T* ReAllocate(T* p, uint64 old_count, uint64 new_count)
{
	return (T*)ReAllocateMemory((byte*)p, old_count * sizeof(T), new_count * sizeof(T));
}

template<typename T>
static inline void DeAllocate(T* p, uint64 count = 1)
{
	DeAllocateMemory((byte*)p, count * sizeof(T));
}

// ------------------------------------------- //

template<typename T>
static inline void DeAllocateArray(Array<T> array)
{
	DeAllocateMemory(array.data, sizeof(T) * array.count);
}

template<typename T>
static inline Array<T> AllocateArray(uint64 count)
{
	return Array<T>((T*)AllocateMemory(count * sizeof(T)), count);
}

// ------------------------------------------- //

struct Stack_Block
{
	Stack_Block* previous;
	uint64 size;
	char data[];
};

struct Stack
{
	Stack_Block* block;
	byte* head;
	byte* end;
};

static Stack CreateStack(uint64 size);
static void  FreeStack(Stack* stack);
static void* StackAllocate(Stack* stack, uint64 size);

template<typename T>
static inline T* StackAllocate(Stack* stack)
{
	return (T*)StackAllocate(stack, sizeof(T));
}

template<typename T>
static inline T* StackAllocate(Stack* stack, uint64 count)
{
	return (T*)StackAllocate(stack, count * sizeof(T));
}

template<typename T>
static Array<T> StackAllocateArray(Stack* stack, uint64 count)
{
	Array<T> array;
	array.data = count ? StackAllocate<T>(stack, count) : null;
	array.count = count;
	return array;
}

// ------------------------------------------- //

template<typename T>
static void CopyMemory(T* dest, const T* src, uint64 count = 1)
{
	if (IsConstEval())
	{
		for (uint64 i = 0; i < count; i++) dest[i] = src[i];
	}
	else __builtin_memcpy(dest, src, sizeof(T) * count);
}

template<typename T>
static void FillMemory(T* dest, uint64 count, T value)
{
	for (uint64 i = 0; i < count; i++) dest[i] = value;
}

template<typename T>
static void FillMemory(T* begin, T* end, T value)
{
	for (; begin < end; begin++) *begin = value;
}

template<typename T>
static inline bool CompareMemory(const T* a, const T* b, uint64 count = 1)
{
	return __builtin_memcmp(a, b, sizeof(T) * count) == 0;
}

template<typename T>
static inline bool Compare(const T* a, const T* b, uint64 count = 1)
{
	for (uint64 i = 0; i < count; i++)
	{
		if (!Compare(a[i], b[i])) return false;
	}

	return true;
}

template<typename T>
static inline void ZeroMemory(T* begin, T* end)
{
	if (IsConstEval())
	{
		for (char* p = (char*)begin; p < (char*)end; p++) *p = 0;
	}
	else __builtin_memset((char*)begin, 0, (char*)end - (char*)begin);
}

template<typename T>
static inline void ZeroMemory(T* p, uint64 count = 1)
{
	ZeroMemory(p, p + count);
}

template<typename T>
static inline void ZeroArray(Array<T> array)
{
	ZeroMemory(array.data, array.count);
}


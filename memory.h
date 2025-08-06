#ifndef MEMORY_H
#define MEMORY_H

#include "general.h"
#include "array.h"

// @cleanme Move Allocators into separate file(s)
// @cleanme Move other functions into general.h?

// ------------------------------------------- //

static void InitGlobalArena();

// ------------------------------------------- //

static byte* AllocateMemory(u64 size);
static byte* ReAllocateMemory(void* p, u64 old_size, u64 new_size);
static void  DeAllocateMemory(void* p, u64 size);

// ------------------------------------------- //

template<typename T>
static inline T* Allocate(u64 count = 1) {
	return (T*)AllocateMemory(count * sizeof(T));
}

template<typename T>
static inline T* ReAllocate(T* p, u64 old_count, u64 new_count) {
	return (T*)ReAllocateMemory((byte*)p, old_count * sizeof(T), new_count * sizeof(T));
}

template<typename T>
static inline void DeAllocate(T* p, u64 count = 1) {
	DeAllocateMemory((byte*)p, count * sizeof(T));
}

// ------------------------------------------- //

template<typename T>
static inline void DeAllocateArray(Array<T> array) {
	DeAllocateMemory(array.data, sizeof(T) * array.count);
}

template<typename T>
static inline Array<T> AllocateArray(u64 count) {
	return Array<T>((T*)AllocateMemory(count * sizeof(T)), count);
}

// ------------------------------------------- //

// @cleanme Change to Copy
template<typename T>
static void CopyMemory(T* dest, const T* src, u64 count = 1) {
	__builtin_memcpy(dest, src, sizeof(T) * count);
}

// @cleanme Change to Fill
template<typename T>
static void FillMemory(T* dest, u64 count, T value) {
	for (u64 i = 0; i < count; i++) dest[i] = value;
}

// @cleanme Change to Fill
template<typename T>
static void FillMemory(T* begin, T* end, T value) {
	for (; begin < end; begin++) *begin = value;
}

// @cleanme Change to Compare
// @cleanme return int? Or maybe use <=> operator now?
template<typename T>
static inline bool CompareMemory(const T* a, const T* b, u64 count = 1) {
	return __builtin_memcmp(a, b, sizeof(T) * count) == 0;
}

template<typename T>
static inline bool Compare(const T* a, const T* b, u64 count = 1) {
	for (u64 i = 0; i < count; i++) {
		if (!Compare(a[i], b[i])) return false;
	}

	return true;
}

template<typename T>
static inline void ZeroMemory(T* begin, T* end) {
	__builtin_memset((char*)begin, 0, (char*)end - (char*)begin);
}

template<typename T>
static inline void ZeroMemory(T* p, u64 count = 1) {
	ZeroMemory(p, p + count);
}

template<typename T>
static inline void ZeroArray(Array<T> array) {
	ZeroMemory(array.data, array.count);
}

#endif // MEMORY_H

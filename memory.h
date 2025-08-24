#ifndef MEMORY_H
#define MEMORY_H

#include "general.h"
#include "array.h"

// @cleanme Move Allocators into separate file(s)
// @cleanme Move other functions into general.h?

// ------------------------------------------- //

static void InitGlobalAllocator();

// ------------------------------------------- //

static void* AllocMemory(u64 size);
static void* ReAllocMemory(void* p, u64 old_size, u64 new_size);
static void  FreeMemory(void* p, u64 size);
static void* CopyAllocMemory(void* p, u64 size);

// ------------------------------------------- //

template<typename T>
static inline T* Alloc(u64 count = 1) {
	return (T*)AllocMemory(count * sizeof(T));
}

template<typename T>
static inline T* ReAlloc(T* p, u64 old_count, u64 new_count) {
	return (T*)ReAllocMemory((byte*)p, old_count * sizeof(T), new_count * sizeof(T));
}

template<typename T>
static inline void Free(T* p, u64 count = 1) {
	FreeMemory((byte*)p, count * sizeof(T));
}

template<typename T>
static inline T* CopyAlloc(T* p, u64 count) {
	T* result = Alloc<T>(count);
	Copy(result, p, count);
	return result;
}

// ------------------------------------------- //

template<typename T>
static inline void FreeArray(Array<T> array) {
	FreeMemory(array.data, sizeof(T) * array.count);
}

template<typename T>
static inline Array<T> AllocArray(u64 count) {
	return Array<T>((T*)AllocMemory(count * sizeof(T)), count);
}

// ------------------------------------------- //

static void CopyMemory(byte* dest, const byte* src, u64 count) {
	__builtin_memcpy(dest, src, count);
}

template<typename T>
static void Copy(T* dest, const T* src, u64 count) {
	CopyMemory((byte*)dest, (byte*)src, count * sizeof(T));
}

// ------------------------------------------- //

static void MoveMemory(byte* dest, const byte* src, u64 count) {
	__builtin_memmove(dest, src, count);
}

template<typename T>
static void Move(T* dest, const T* src, u64 count) {
	MoveMemory((byte*)dest, (byte*)src, count * sizeof(T));
}

// ------------------------------------------- //

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

// ------------------------------------------- //

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
static inline void Zero(T* p, u64 count = 1) {
	ZeroMemory(p, p + count);
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

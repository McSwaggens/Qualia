#ifndef ALLOC_H
#define ALLOC_H

#include "general.h"

static void  InitGlobalAllocator();
static void* AllocMemory(u64 size);
static void  FreeMemory(void* p, u64 size);
static void* ReAllocMemory(void* p, u64 old_size, u64 new_size);
static void* CopyAllocMemory(void* p, u64 size);

template<typename T> static T*   Alloc(u64 count = 1)                      { return (T*)AllocMemory(sizeof(T) * count); }
template<typename T> static T*   ReAlloc(T* p, u64 old_size, u64 new_size) { return (T*)ReAllocMemory(p, old_size * sizeof(T), new_size * sizeof(T)); }
template<typename T> static T*   CopyAlloc(T* p, u64 count = 1)            { return (T*)CopyAllocMemory(p, sizeof(T) * count); }
template<typename T> static void Free(T* p, u64 count = 1)                 { FreeMemory(p, sizeof(T) * count); }

#endif // ALLOC_H

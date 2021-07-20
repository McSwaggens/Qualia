#pragma once

#include "int.h"
#include "array.h"
#include "memory.h"
#include "print.h"

#define POOL_SIZE 1024
#define BUCKET_SIZE (4096 * 1024)

struct Alloc
{
	void* data;
	u64 size;
};

Alloc ExpandArray(char* p, u64 size);
void ReleaseArray(char* p, u64 free_size);
void InitArrayPool();

template<typename T>
struct Pooled_Array
{
	T* data;
	u64 size;
	u32 count;
	u32 cap;

	void Add(T value)
	{
		if (count >= cap)
		{
			Alloc alloc = ExpandArray((char*)data, size);
			data = (T*)alloc.data;
			size = alloc.size;
			cap = size / sizeof(T);
		}

		data[count++] = value;
	}

	bool IsEmpty()
	{
		return !count;
	}

	Array<T> Lock()
	{
		if (data)
		{
			ReleaseArray((char*)(data + count), size - count * sizeof(T));
		}

		return Array(data, count);
	}

	void Release()
	{
		ReleaseArray((char*)data, size);
	}

	void Reset()
	{
		count = 0;
	}
};

template<typename T>
Pooled_Array<T> NewPooledArray()
{
	Pooled_Array<T> result;
	result.data = null;
	result.count = 0;
	result.cap = 0;
	result.size = 0;
	return result;
}


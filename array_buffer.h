#pragma once

#include "array.h"
#include "memory.h"
#include "print.h"

#define POOL_SIZE 1024
#define BUCKET_SIZE (4096 * 1024)

struct Array_Buffer_Entry
{
	void* data;
	uint64 size;
};

static Array_Buffer_Entry GetArrayBufferEntry(uint64 min_size);
static void ReleaseArrayBuffer(void* data, uint64 size);
static void InitArrayBufferPool();

template<typename T>
struct Array_Buffer
{
	T* data;
	uint64 count;
	uint64 capacity;
	uint64 size;

	void Add(T value)
	{
		if (count+1 >= capacity) COLD
		{
			Array_Buffer_Entry entry = GetArrayBufferEntry(size + sizeof(T));

			if (count) COLD
			{
				CopyMemory((T*)entry.data, data, count);
				ReleaseArrayBuffer(data, size);
			}

			data = (T*)entry.data;
			size = entry.size;
			capacity = size / sizeof(T);
		}

		data[count] = value;
		count++;
	}

	Array<T> Lock()
	{
		if (data)
		{
			ReleaseArrayBuffer(data + count, size - count * sizeof(T));
		}

		return Array(data, count);
	}

	void Release()
	{
		ReleaseArrayBuffer((void*)data, size);
	}

	void Reset()
	{
		count = 0;
	}
};

template<typename T>
static Array_Buffer<T> CreateArrayBuffer()
{
	Array_Buffer<T> result;
	result.data = null;
	result.count = 0;
	result.capacity = 0;
	result.size = 0;
	return result;
}


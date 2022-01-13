#include "pooled_array.h"
#include "memory.h"
#include "assert.h"

static List<Array_Buffer_Entry> array_buffers;

static void InitArrayBufferPool()
{
	array_buffers = null;
}

static Array_Buffer_Entry GetArrayBufferEntry(u64 current_size)
{
	if (array_buffers.count && array_buffers.Last().size > current_size) HOT
	{
		return array_buffers.Pop();
	}

	u64 new_size = 1 << 14;

	if (current_size >= new_size)
	{
		new_size = current_size << 2;
	}

	Array_Buffer_Entry entry;
	entry.size = new_size;
	entry.data = GetPage(new_size);

	return entry;
}

static void ReleaseArrayBuffer(void* data, u64 size)
{
	if (size >= 512)
	{
		Array_Buffer_Entry entry;
		entry.data = data;
		entry.size = size;

		array_buffers.Add(entry);
	}

	// Buffer not worth keeping
}


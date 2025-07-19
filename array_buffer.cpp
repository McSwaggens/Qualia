#include "array_buffer.h"
#include "memory.h"
#include "assert.h"

static List<Array_Buffer_Entry> array_buffers;

static void InitArrayBufferPool()
{
	ZeroMemory(&array_buffers);
}

static Array_Buffer_Entry GetArrayBufferEntry(u64 min_size)
{
	if (array_buffers.count && array_buffers.Last().size > min_size) HOT
	{
		Array_Buffer_Entry entry = array_buffers[array_buffers.count-1];
		array_buffers.count--;
		return entry;
	}

	u64 new_size = 1 << 14;

	if (min_size >= new_size) COLD
	{
		new_size = RaisePow2(min_size << 2);
	}

	Array_Buffer_Entry entry;
	entry.size = new_size;
	entry.data = AllocateVirtualPage(new_size, PAGE_FLAG_WRITE);

	return entry;
}

static void ReleaseArrayBuffer(void* data, u64 size)
{
	if (size < 512) // Buffer not worth keeping
		return; // @Todo: Release to global allocator

	Array_Buffer_Entry entry;
	entry.data = data;
	entry.size = size;

	array_buffers.Add(entry);
}


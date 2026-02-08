#include "array_buffer.h"
#include "alloc.h"
#include "assert.h"
#include "os.h"

static List<ArrayBuffer_Entry> array_buffers = { };

static ArrayBuffer_Entry GetArrayBufferEntry(u64 min_size) {
	if (array_buffers.count && array_buffers.Last().size > min_size) HOT
		return array_buffers[--array_buffers.count];

	u64 new_size = 1 << 14;

	if (min_size >= new_size) COLD
		new_size = RaisePow2(min_size << 2);

	ArrayBuffer_Entry entry;
	entry.size = new_size;
	entry.data = OS::AllocateVirtualMemory(new_size);

	return entry;
}

static void ReleaseArrayBuffer(void* data, u64 size) {
	if (size < 512) // Buffer not worth keeping
		return; // @Todo: Release to global allocator

	ArrayBuffer_Entry entry;
	entry.data = data;
	entry.size = size;

	array_buffers.Add(entry);
}


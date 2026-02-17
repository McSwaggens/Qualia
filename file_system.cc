#include "file_system.h"
#include "alloc.h"
#include "array.h"
#include "general.h"
#include "os.h"
#include "string.h"
#include "assert.h"

File File::Open(String path) {
	char cpath[path.length+1];
	path.Copy(cpath);
	cpath[path.length] = 0;
	return File(OS::OpenFile(cpath));
}

void File::Write(byte* data, u64 size) {
	Assert(IsValid());
	OS::WriteFile(handle, data, size);
}

void File::Write(Array<byte> data) {
	Assert(IsValid());
	Write(data, data.length);
}

void File::Read(byte* data, u64 size) {
	Assert(IsValid());
	OS::ReadFile(handle, data, size);
}

void File::Read(Array<byte> data) {
	Assert(IsValid());
	Read(data, data.length);
}

void File::Close() {
	Assert(IsValid());
	OS::CloseFile(handle);
}

u64 File::Size() {
	Assert(IsValid());
	return OS::QueryFileSize(handle);
}

bool File::DoesExist(String path) {
	Assert(path);
	char cpath[path.length+1];
	path.Copy(cpath);
	cpath[path.length] = 0;
	return OS::DoesFileExist(cpath);
}

Array<byte> File::Load(String path, u64 left_pad, u64 right_pad) {
	Assert(path);

	File file = File::Open(path);
	if (!file.IsValid())
		return null;

	u64 size = file.Size();
	if (!size) {
		file.Close();
		return null;
	}

	byte* data = (byte*)AllocMemory(left_pad + size + right_pad);

	file.Read(data + left_pad, size);

	ZeroMemory(data, left_pad);
	ZeroMemory(data + left_pad + size, right_pad);

	return Array<byte>(data + left_pad, size);
}

void OutputBuffer::Flush() {
	Assert(file.IsValid());
	file.Write(data, head);
	head = 0;
}

void OutputBuffer::Write(char c) {
	Assert(file.IsValid());

	if (head >= OUTPUT_BUFFER_SIZE) COLD
		Flush();

	data[head++] = c;
}

void OutputBuffer::Write(const char* src, u64 size) {
	Assert(file.IsValid());

	if (head + size <= OUTPUT_BUFFER_SIZE) HOT {
		CopyMemory(data + head, src, size);
		head += size;
		return;
	}

	Flush();

	if (size >= OUTPUT_BUFFER_SIZE) COLD {
		Write(src, size);
		return;
	}

	CopyMemory(data, src, size);
	head = size;
}

void OutputBuffer::Write(String string) {
	Assert(file.IsValid());
	Write(string.data, string.length);
}

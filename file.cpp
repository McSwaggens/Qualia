#include "file.h"
#include "memory.h"
#include "print.h"

OutputBuffer standard_output_buffer { .file = standard_output_file, .index = 0 };
OutputBuffer standard_error_buffer  { .file = standard_error_file,  .index = 0 };

File OpenFile(String path, FileMode mode, FileAccess access)
{
	s32 flags;
	u32 perm = 0x180;

	switch (access)
	{
		case FILE_ACCESS_READ:       flags = 0; break;
		case FILE_ACCESS_WRITE:      flags = 1; break;
		case FILE_ACCESS_READ_WRITE: flags = 2; break;
	}

	switch (mode)
	{
		case FILE_MODE_OPEN:                                      break;
		case FILE_MODE_APPEND:             flags |= 0x400;        break;
		case FILE_MODE_TRUNCATE:           flags |= 0x200;        break;
		case FILE_MODE_CREATE:             flags |= 0x40 | 0x80;  break;
		case FILE_MODE_CREATE_OR_OPEN:     flags |= 0x40;         break;
		case FILE_MODE_CREATE_OR_APPEND:   flags |= 0x40 | 0x400; break;
		case FILE_MODE_CREATE_OR_TRUNCATE: flags |= 0x40 | 0x200; break;
	}

	if (path.length >= 4096)
	{
		return -1;
	}

	char cpath[path.length+1];
	CopyMemory(cpath, (const char*)path, path.length);
	cpath[path.length] = 0;

	File file;
	file.id = SystemCall(2, (u64)cpath, flags, perm);
	return file;
}

u64 File::Read(void* dest, u64 size)
{
	return SystemCall(0, id, (u64)dest, size);
}

u64 File::Write(const void* data, u64 size)
{
	return SystemCall(1, id, (u64)data, size);
}

u64 File::QuerySize()
{
	struct Status
	{
		u64 dev;
		u64 ino;
		u64 nlink;

		u32 mode;
		u32 uid;
		u32 gid;
		u32 padding0;

		u64 rdev;
		s64 size;
		s64 block_size;
		s64 blocks;

		u64 atime;
		u64 atime_nsec;
		u64 mtime;
		u64 mtime_nsec;
		u64 ctime;
		u64 ctime_nsec;

		s64 padding1[3];
	} status;

	SystemCall(5, id, (u64)&status);

	return status.size;
}

void File::Close()
{
	SystemCall(3, id);
}

void OutputBuffer::Flush()
{
	file.Write(data, index);
	index = 0;
}

void OutputBuffer::Write(char c)
{
	data[index++] = c;
	if (index == sizeof data)
	{
		Flush();
	}
}

void OutputBuffer::Write(const char* src, u64 size)
{
	if (index + size <= sizeof data)
	{
		CopyMemory(data + index, src, size);
		index += size;
	}
	else
	{
		Flush();
		file.Write(src, size);
	}
}

bool DoesFileExist(String path)
{
	char cpath[path.length+1];
	CopyMemory(cpath, (const char*)path, path.length);
	cpath[path.length] = 0;
	// F = 0
	// X = 1
	// W = 2
	// R = 4
	return SystemCall(21, path, 0) == 0;
}

Span<char> LoadFile(String path, u32 padding)
{
	File file = OpenFile(path, FILE_MODE_OPEN, FILE_ACCESS_READ);
	if (!file)
	{
		Print("Error: Could not open file %\n", path);
		Fail();
	}

	u64 size = file.QuerySize();
	if (!size)
	{
		Print("Error: File is empty: %\n", path);
		Fail();
	}

	char* data = Allocate<char>(size + padding);
	file.Read(data, size);
	file.Close();
	ZeroMemory(data + size, padding);

	return Span(data, size);
}


#include "file.h"
#include "memory.h"
#include "print.h"
#include <unistd.h>
#include <fcntl.h>
#include <linux/limits.h>
#include <linux/types.h>
#include <sys/stat.h>

OutputBuffer standard_output_buffer { .file = standard_output_file, .index = 0 };
OutputBuffer standard_error_buffer  { .file = standard_error_file,  .index = 0 };

File OpenFile(String path, FileMode mode, FileAccess access)
{
	s32 flags;
	mode_t perm = S_IRUSR | S_IWUSR;

	switch (access)
	{
		case FILE_ACCESS_READ:       flags = O_RDONLY; break;
		case FILE_ACCESS_WRITE:      flags = O_WRONLY; break;
		case FILE_ACCESS_READ_WRITE: flags = O_RDWR;   break;
	}

	switch (mode)
	{
		case FILE_MODE_OPEN:                                            break;
		case FILE_MODE_APPEND:             flags |= O_APPEND;           break;
		case FILE_MODE_TRUNCATE:           flags |= O_TRUNC;            break;
		case FILE_MODE_CREATE:             flags |= O_CREAT | O_EXCL;   break;
		case FILE_MODE_CREATE_OR_OPEN:     flags |= O_CREAT;            break;
		case FILE_MODE_CREATE_OR_APPEND:   flags |= O_CREAT | O_APPEND; break;
		case FILE_MODE_CREATE_OR_TRUNCATE: flags |= O_CREAT | O_TRUNC;  break;
	}

	if (path.length >= PATH_MAX)
	{
		return -1;
	}

	char cpath[path.length+1];
	CopyMemory(cpath, (const char*)path, path.length);
	cpath[path.length] = 0;

	File file;
	file.id = open(cpath, flags, perm);
	return file;
}

u64 File::Read(void* dest, u64 size)
{
	return read(id, dest, size);
}

u64 File::Write(const void* data, u64 size)
{
	return write(id, data, size);
}

u64 File::QuerySize()
{
	struct stat64 stat;
	fstat64(id, &stat);
	return stat.st_size;
}

void File::Close()
{
	close(id);
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
	return access(path, F_OK) == 0;
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


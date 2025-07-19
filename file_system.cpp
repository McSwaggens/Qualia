#include "file_system.h"
#include "memory.h"
#include "array.h"
#include "general.h"
#include "list.h"
#include "string.h"
#include "memory.h"
#include "print.h"
#include "assert.h"

static FileHandle FileOpen(String path, FileMode mode, FileAccessFlags access_flags)
{
	s32 flags;
	u32 perm = 0x180;

	switch (access_flags)
	{
		case FILE_ACCESS_READ:  flags = 0; break;
		case FILE_ACCESS_WRITE: flags = 1; break;
		case FILE_ACCESS_READ | FILE_ACCESS_WRITE: flags = 2; break;
	}


	const u32 LINUX_READ_ONLY   = 0x0;
	const u32 LINUX_WRITE_ONLY  = 0x1;
	const u32 LINUX_READ_WRITE  = 0x2;
	const u32 LINUX_CREATE      = 0x40;  // With an E!
	const u32 LINUX_EXCLUSIVE   = 0x80;  // Error if we tried creating a file and it already exists.
	const u32 LINUX_NO_TTY      = 0x100; // Prevent obtaining a terminal handle (e.g. Ctrl+c stuff)
	const u32 LINUX_TRUNCATE    = 0x200;
	const u32 LINUX_APPEND      = 0x400;
	const u32 LINUX_NON_BLOCK   = 0x800;
	const u32 LINUX_DIRECTORY   = 0x10000;
	const u32 LINUX_AUTO_CLOSE  = 0x80000; // @RemoveMe?

	switch (mode)
	{
		// Just have a FILE_MODE_CREATE flag?
		case FILE_MODE_OPEN:                                        break;
		case FILE_MODE_APPEND:             flags |= LINUX_APPEND;   break;
		case FILE_MODE_TRUNCATE:           flags |= LINUX_TRUNCATE; break;
		case FILE_MODE_CREATE:             flags |= LINUX_CREATE | LINUX_EXCLUSIVE; break;
		case FILE_MODE_CREATE_OR_OPEN:     flags |= LINUX_CREATE;                   break;
		case FILE_MODE_CREATE_OR_APPEND:   flags |= LINUX_CREATE | LINUX_APPEND;    break;
		case FILE_MODE_CREATE_OR_TRUNCATE: flags |= LINUX_CREATE | LINUX_TRUNCATE;  break;
	}

	if (path.length >= 4096)
	{
		return -1;
	}

	char cpath[path.length+1];
	CopyMemory(cpath, path.data, path.length);
	cpath[path.length] = 0;

	FileHandle handle = NULL_FILE_HANDLE;
	handle = SystemCall(2, (s64)cpath, flags, perm);
	return handle;
}

static void FileRead(FileHandle file_handle, byte* dest, u64 size)
{
	SystemCall(0, file_handle, (s64)dest, size);
}

static void FileWrite(FileHandle file_handle, const byte* data, u64 size)
{
	SystemCall(1, file_handle, (s64)data, size);
}

static u64 FileQuerySize(FileHandle file_handle)
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

	SystemCall(5, file_handle, (s64)&status);
	return status.size;
}

static void FileClose(FileHandle file_handle)
{
	SystemCall(3, file_handle);
}

static void BufferFlush(OutputBuffer* buffer)
{
	FileWrite(buffer->handle, buffer->data, buffer->current_index);
	buffer->current_index = 0;
}

static void BufferWriteByte(OutputBuffer* buffer, char c)
{
	if (buffer->current_index >= OUTPUT_BUFFER_SIZE) COLD
	{
		BufferFlush(buffer);
	}

	buffer->data[buffer->current_index++] = c;
}

static void BufferWriteData(OutputBuffer* buffer, const char* src, u64 size)
{
	if (buffer->current_index + size <= OUTPUT_BUFFER_SIZE) HOT
	{
		CopyMemory(buffer->data + buffer->current_index, src, size);
		buffer->current_index += size;
		return;
	}

	BufferFlush(buffer);

	if (size >= OUTPUT_BUFFER_SIZE) COLD
	{
		FileWrite(buffer->handle, src, size);
		return;
	}

	CopyMemory(buffer->data, src, size);
	buffer->current_index = size;
}

static inline void BufferWriteString(OutputBuffer* buffer, String string)
{
	BufferWriteData(buffer, string.data, string.length);
}

static bool FileDoesExist(String path)
{
	char cpath[path.length+1];
	CopyMemory(cpath, path.data, path.length);
	cpath[path.length] = 0;
	// F = 0
	// X = 1
	// W = 2
	// R = 4
	return SystemCall(21, (s64)cpath, 0) == 0;
}

static Array<byte> FileLoad(String path, u64 left, u64 right)
{
	Array<byte> result = { null, 0 };
	FileHandle handle = FileOpen(path, FILE_MODE_OPEN, FILE_ACCESS_READ);

	if (!IsFileHandleValid(handle)) COLD
	{
		return result;
	}

	u64 size = FileQuerySize(handle);
	result.count = size;

	if (size) HOT
	{
		byte* data = AllocateMemory(size + left + right);
		result.elements = data + left;

		FileRead(handle, data + left, size);

		ZeroMemory(data, left);
		ZeroMemory(data + left + size, right);
	}

	FileClose(handle);
	return result;
}

static const u8 LINUX_FILE_TYPE_UNKNOWN   = 0;
static const u8 LINUX_FILE_TYPE_FIFO      = 1;
static const u8 LINUX_FILE_TYPE_CHARACTER = 2;
static const u8 LINUX_FILE_TYPE_DIRECTORY = 4;
static const u8 LINUX_FILE_TYPE_BLOCK     = 6;
static const u8 LINUX_FILE_TYPE_REGULAR   = 8;
static const u8 LINUX_FILE_TYPE_LINK      = 10;
static const u8 LINUX_FILE_TYPE_SOCKET    = 12;

struct Linux_Directory_Entry
{
	u64 inode;
	s64 next_entry_offset;
	u16 length;
	u8  type;
	char name[];
};

static String GetCurrentDirectoryString()
{
	char buffer[4096];
	u64 length = SystemCall(79, (s64)buffer, 4096);
	String string = AllocateString(length, 0);
	CopyMemory(string.data, buffer, length);
	Assert(length && buffer[length-1] != 0);
	return string;
}

static DirectoryHandle OpenDirectory(String path)
{
	DirectoryHandle directory = NULL_DIRECTORY_HANDLE;
	return directory;
}

static Array<Linux_Directory_Entry> QueryLinuxDirectoryEntries(String path)
{
	char cpath[path.length+1];
	CopyMemory(cpath, path.data, path.length);
	cpath[path.length] = 0;

	FileHandle file;
	ZeroMemory(&file);

	Array<Linux_Directory_Entry> entries = AllocateArray<Linux_Directory_Entry>(256);
	SystemCall(217, (s64)cpath, (s64)entries.elements, entries.count);

	return entries;
}

static DirectoryHandle OpenCurrentDirectory()
{
	// @FixMe @Optimization
	return OpenDirectory(GetCurrentDirectoryString());
}

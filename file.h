#pragma once

#include "string.h"
#include "span.h"


struct File
{
	s32 id;

	constexpr File() = default;
	constexpr File(s32 file_handle) : id(file_handle) { }
	constexpr operator  s32() const { return id; }
	constexpr operator bool() const { return static_cast<bool>(id >= 0); }

	u64 Read(void* dest, u64 size);
	u64 Write(const void* data, u64 size);
	u64 QuerySize();
	void Close();
};


struct OutputBuffer
{
	File file;
	u32 index;
	char data[4096];

	void Flush();
	void Write(char c);
	void Write(const char* src, u64 size);

	template<u64 N>
	void Write(const char (&s)[N])
	{
		Write(s, N-1);
	}
};


static const File standard_input_file  = 0;
static const File standard_output_file = 1;
static const File standard_error_file  = 2;

extern OutputBuffer standard_output_buffer;
extern OutputBuffer standard_error_buffer;


enum FileMode
{
	FILE_MODE_OPEN,     // Open an existing file.
	FILE_MODE_APPEND,   // Open an existing file and go to the end.
	FILE_MODE_TRUNCATE, // Open and truncate an existing file.
	FILE_MODE_CREATE,   // Create a file that doesn't already exist.
						// ProTip: Believe it or not; there is an 'E' at the end of 'create'!

	FILE_MODE_CREATE_OR_OPEN,     // Open or create a file.
	FILE_MODE_CREATE_OR_APPEND,   // Create file if it doesn't already exist, otherwise go the end.
	FILE_MODE_CREATE_OR_TRUNCATE, // Truncate an existing file, otherwise create one.
};


enum FileAccess
{
	FILE_ACCESS_READ,
	FILE_ACCESS_WRITE,
	FILE_ACCESS_READ_WRITE,
};


File OpenFile(String path, FileMode mode = FILE_MODE_OPEN, FileAccess access = FILE_ACCESS_READ);
Span<char> LoadFile(String path, u32 padding = 0);
bool DoesFileExist(String path);



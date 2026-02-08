#pragma once

#include "string.h"
#include "array.h"

#include "os.h"

struct File {
	OS::FileHandle handle = -1;

	File() = default;
	File(OS::FileHandle handle) : handle(handle) { }

	static File Open(String path);
	static Array<byte> Load(String path, u64 padding_left = 0, u64 padding_right = 0);
	static bool DoesExist(String path);

	void Write(byte* data, u64 size);
	void Write(Array<byte> data);
	void Read(byte* data, u64 size);
	void Read(Array<byte> data);
	u64  Size();
	bool IsValid() { return handle != OS::INVALID_FILE_HANDLE; }
	void Close();

};

static const u64 OUTPUT_BUFFER_SIZE = 4096 * 2;

struct OutputBuffer {
	File file;
	u64  head;
	byte data[OUTPUT_BUFFER_SIZE];

	void Flush();
	void Write(char c);
	void Write(const char* src, u64 size);
	void Write(String string);
};

static OutputBuffer output_buffer { .file = OS::OUTPUT_FILE_HANDLE, .head = 0 };
static OutputBuffer error_buffer  { .file = OS::ERROR_FILE_HANDLE,  .head = 0 };

enum FileMode {
	FILE_MODE_OPEN,     // Open an existing file.
	FILE_MODE_APPEND,   // Open an existing file and go to the end.
	FILE_MODE_TRUNCATE, // Open and truncate an existing file.
	FILE_MODE_CREATE,   // Create a file that doesn't already exist.
						// ProTip: Believe it or not; there is an 'e' at the end of 'create'!

	FILE_MODE_CREATE_OR_OPEN,     // Open or create a file.
	FILE_MODE_CREATE_OR_APPEND,   // Create file if it doesn't already exist, otherwise go to the end.
	FILE_MODE_CREATE_OR_TRUNCATE, // Truncate an existing file, otherwise create one.
};




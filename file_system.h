#pragma once

#include "string.h"

typedef s32 FileHandle;
typedef s32 DirectoryHandle;

static const FileHandle      NULL_FILE_HANDLE      = -1;
static const DirectoryHandle NULL_DIRECTORY_HANDLE = -1;

static inline bool IsFileHandleValid(FileHandle handle)           { return handle >= 0; }
static inline bool IsDirectoryHandleValid(DirectoryHandle handle) { return handle >= 0; }

static const u64 OUTPUT_BUFFER_SIZE = 4096 * 2;

struct OutputBuffer
{
	FileHandle handle;
	u64 current_index;
	byte data[OUTPUT_BUFFER_SIZE];
};

static const FileHandle UNIX_INPUT_FILE_HANDLE  = 0;
static const FileHandle UNIX_OUTPUT_FILE_HANDLE = 1;
static const FileHandle UNIX_ERROR_FILE_HANDLE  = 2;

static OutputBuffer unix_output_buffer { .handle = UNIX_OUTPUT_FILE_HANDLE, .current_index = 0 };
static OutputBuffer unix_error_buffer  { .handle = UNIX_ERROR_FILE_HANDLE,  .current_index = 0 };

enum FileMode
{
	FILE_MODE_OPEN,     // Open an existing file.
	FILE_MODE_APPEND,   // Open an existing file and go to the end.
	FILE_MODE_TRUNCATE, // Open and truncate an existing file.
	FILE_MODE_CREATE,   // Create a file that doesn't already exist.
						// ProTip: Believe it or not; there is an 'e' at the end of 'create'!

	FILE_MODE_CREATE_OR_OPEN,     // Open or create a file.
	FILE_MODE_CREATE_OR_APPEND,   // Create file if it doesn't already exist, otherwise go to the end.
	FILE_MODE_CREATE_OR_TRUNCATE, // Truncate an existing file, otherwise create one.
};


typedef u32 FileAccessFlags;
static const FileAccessFlags FILE_ACCESS_READ  = 0x1;
static const FileAccessFlags FILE_ACCESS_WRITE = 0x2;

static Array<byte> FileLoad(String path, u64 padding_left, u64 padding_right);
static FileHandle  FileOpen(String path, FileMode mode, FileAccessFlags access_flags);
static void        FileClose(FileHandle file);
static bool        FileDoesExist(String path);
static void        FileRead(FileHandle file, byte* dest, u64 size);
static void        FileWrite(FileHandle file, const byte* data, u64 size);
static u64      FileQuerySize(FileHandle file);

static void BufferWriteString(OutputBuffer* buffer, String string);
static void BufferWriteData(OutputBuffer* buffer, const char* src, u64 size);
static void BufferWriteByte(OutputBuffer* buffer, byte b);
static void BufferFlush(OutputBuffer* buffer);

static String GetCurrentDirectoryString();
static DirectoryHandle OpenDirectory(String path);
static DirectoryHandle OpenCurrentDirectory();


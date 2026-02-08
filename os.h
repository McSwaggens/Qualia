#pragma once

#include "general.h"

// Operating system file must implement all these functions :)

namespace OS {
	enum PageFlags {
		PAGE_FLAG_WRITE   = 0x01,
		PAGE_FLAG_EXECUTE = 0x02,
		PAGE_FLAG_STACK   = 0x04,
	};

	static constexpr u64 PAGE_SIZE = 4096;

	using ThreadID = s32;

	[[noreturn]]
	void Terminate(bool success = true);

	byte* AllocateVirtualMemory(u64 size, PageFlags flags = PAGE_FLAG_WRITE);
	void  FreeVirtualMemory(byte* page, u64 size);

	using FileHandle = s32;

	static const FileHandle INVALID_FILE_HANDLE = -1;
	static const FileHandle INPUT_FILE_HANDLE   =  0;
	static const FileHandle OUTPUT_FILE_HANDLE  =  1;
	static const FileHandle ERROR_FILE_HANDLE   =  2;

	FileHandle OpenFile(const char* cstring_path);
	void CloseFile(FileHandle handle);
	s32 WriteFile(FileHandle handle, const byte* data, u64 length);
	s32 ReadFile(FileHandle handle, byte* out_data, u64 out_length);
	u64 QueryFileSize(FileHandle handle);
	bool DoesFileExist(const char* cstring_path);

	ThreadID CreateThread(byte* stack_top, int (*fn)(void*), void* arg);
	ThreadID GetCurrentThreadID();
	void SendPauseSignal(ThreadID tid);
	void InstallPauseHandler(void (*handler)(int));
	void FutexWait(volatile s32* addr, s32 expected);
	void FutexWake(volatile s32* addr);
}

#include "os.h"

#include <sys/mman.h>
#include <sys/stat.h>

#include <unistd.h>
#include <fcntl.h>

#include "print.cc"

[[noreturn]]
void OS::Terminate(bool success) {
	_exit(!success);
	Unreachable();
}

#include "print.h"

byte* OS::AllocateVirtualMemory(u64 size, PageFlags flags) {
	Print("OS::AllocateVirtualMemory(%)\n", size);
	u64 protection = PROT_READ; // Pages are always readable on x86.
	if (flags & PAGE_FLAG_WRITE)   protection |= PROT_WRITE;
	// if (flags & PAGE_FLAG_STACK)   protection |= PROT_GROWSDOWN;
	if (flags & PAGE_FLAG_EXECUTE) protection |= PROT_EXEC;

	u64 mflags = MAP_ANONYMOUS | MAP_PRIVATE;
	// if (flags & PAGE_FLAG_STACK) protection |= PROT_GROWSDOWN;

	byte* page = (byte*)mmap(0, size, protection, mflags, -1, 0);

	page = (byte*)AssumeAligned(page, 4096);

	return page;
}

void OS::FreeVirtualMemory(byte* page, u64 size) {
	munmap(page, size);
}

OS::FileHandle OS::OpenFile(const char* path) {
	return open(path, O_APPEND | O_CREAT);
}

void OS::CloseFile(FileHandle handle) {
	close(handle);
}

s32 OS::WriteFile(FileHandle handle, const byte* data, u64 length) {
	return write(handle, data, length);
}

s32 OS::ReadFile(FileHandle handle, byte* out_data, u64 out_length) {
	return read(handle, out_data, out_length);
}

u64 OS::QueryFileSize(FileHandle handle) {
	struct stat info;
	fstat(handle, &info);
	return info.st_size;
}

bool OS::DoesFileExist(const char* cstring_path) {
	return access(cstring_path, F_OK) == 0;
}


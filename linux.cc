#include "os.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>

#include <sched.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/futex.h>

[[noreturn]]
void OS::Terminate(bool success) {
	_exit(!success);
	Unreachable();
}

byte* OS::AllocateVirtualMemory(u64 size, PageFlags flags) {
	u64 protection = PROT_READ; // Pages are always readable on x86.
	if (flags & PAGE_FLAG_WRITE)   protection |= PROT_WRITE;
	// if (flags & PAGE_FLAG_STACK)   protection |= PROT_GROWSDOWN;
	if (flags & PAGE_FLAG_EXECUTE) protection |= PROT_EXEC;

	u64 mflags = MAP_ANONYMOUS | MAP_PRIVATE;

	if (size >= 8llu << 30)
		mflags |= MAP_NORESERVE;

	byte* page = (byte*)mmap(0, size, protection, mflags, -1, 0);

	page = (byte*)AssumeAligned(page, 4096);

	if (page == (void*)-1)
		return 0;

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

OS::ThreadID OS::CreateThread(byte* stack_top, int (*fn)(void*), void* arg) {
	int flags = CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD | CLONE_SYSVSEM;
	return clone(fn, stack_top, flags, arg);
}

OS::ThreadID OS::GetCurrentThreadID() {
	return syscall(SYS_gettid);
}

void OS::SendPauseSignal(ThreadID tid) {
	syscall(SYS_tgkill, getpid(), tid, SIGUSR1);
}

void OS::InstallPauseHandler(void (*handler)(int)) {
	struct sigaction sa = {};
	sa.sa_handler = handler;
	sa.sa_flags = SA_RESTART;
	sigemptyset(&sa.sa_mask);
	sigaction(SIGUSR1, &sa, null);
}

void OS::FutexWait(volatile s32* addr, s32 expected) {
	syscall(SYS_futex, (s32*)addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, expected, null, null, 0);
}

void OS::FutexWake(volatile s32* addr) {
	syscall(SYS_futex, (s32*)addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, null, null, 0);
}


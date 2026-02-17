#include "general.h"
#include "os.h"

#include <sys/mman.h>
#include <sys/stat.h>

#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>

extern "C" int __ulock_wait(u32 operation, void* addr, u64 value, u32 timeout);
extern "C" int __ulock_wake(u32 operation, void* addr, u64 wake_value);

[[noreturn]]
void OS::Terminate(bool success) {
	_exit(!success);
	Unreachable();
}

byte* OS::AllocateVirtualMemory(u64 size, PageFlags flags) {
	size = RaiseMultiplePow2(size, OS::PAGE_SIZE);

	u64 protection = PROT_READ;
	if (flags & PAGE_FLAG_WRITE)   protection |= PROT_WRITE;
	if (flags & PAGE_FLAG_EXECUTE) protection |= PROT_EXEC;

	u64 mflags = MAP_ANONYMOUS | MAP_PRIVATE;

	byte* page = (byte*)mmap(0, size, protection, mflags, -1, 0);

	page = (byte*)AssumeAligned(page, OS::PAGE_SIZE);

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

// Threading

static constexpr s32 MAX_THREADS = 64;
static pthread_t thread_table[MAX_THREADS];
static volatile s32 thread_count = 1; // slot 0 reserved for main thread

struct MacOSThreadArgs {
	int (*fn)(void*);
	void* arg;
};

static MacOSThreadArgs thread_args[MAX_THREADS];

static void* ThreadWrapper(void* raw) {
	MacOSThreadArgs* args = (MacOSThreadArgs*)raw;
	args->fn(args->arg);
	return null;
}

__attribute__((constructor))
static void RegisterMainThread() {
	thread_table[0] = pthread_self();
}

OS::ThreadID OS::CreateThread(byte* stack_top, int (*fn)(void*), void* arg) {
	s32 id = __sync_fetch_and_add(&thread_count, 1);
	if (id >= MAX_THREADS) return -1;

	thread_args[id] = { fn, arg };
	pthread_create(&thread_table[id], null, ThreadWrapper, &thread_args[id]);

	return id;
}

OS::ThreadID OS::GetCurrentThreadID() {
	pthread_t self = pthread_self();
	for (s32 i = 0; i < thread_count; i++) {
		if (pthread_equal(self, thread_table[i]))
			return i;
	}
	return 0;
}

void OS::SendPauseSignal(ThreadID tid) {
	if (tid >= 0 && tid < thread_count)
		pthread_kill(thread_table[tid], SIGUSR1);
}

void OS::InstallPauseHandler(void (*handler)(int)) {
	struct sigaction sa = {};
	sa.sa_handler = handler;
	sa.sa_flags = SA_RESTART;
	sigemptyset(&sa.sa_mask);
	sigaction(SIGUSR1, &sa, null);
}

void OS::FutexWait(volatile s32* addr, s32 expected) {
	__ulock_wait(1, (void*)addr, (u64)expected, 0);
}

void OS::FutexWake(volatile s32* addr) {
	__ulock_wake(1, (void*)addr, 0);
}

#include "thread.h"
#include "assert.h"
#include "memory.h"
#include "os.h"

FixedBuffer<Thread, 256> Thread::thread_pool;

struct ThreadCreateInfo {
	void (*function)(ThreadID);
	ThreadID id;
};

static int ThreadEntry(void* arg) {
	ThreadCreateInfo info = *(ThreadCreateInfo*)arg;
	info.function(info.id);
	return 0;
}

static void ThreadPauseHandler(int) {
	OS::ThreadID tid = OS::GetCurrentThreadID();
	for (u64 i = 0; i < Thread::thread_pool.head; i++) {
		Thread& t = Thread::thread_pool[i];
		if (t.system_thread_id == tid) {
			while (t.status == Thread::PAUSED)
				OS::FutexWait((volatile s32*)&t.status, Thread::PAUSED);
			return;
		}
	}
}

void Thread::Init() {
	OS::InstallPauseHandler(ThreadPauseHandler);
}

Thread* Thread::Create(u64 stack_size, void (*thread_start_function)(ThreadID id)) {
	Thread* thread = thread_pool.Add();
	ThreadID id = (ThreadID)thread_pool.ToIndex(thread);

	byte* stack_memory = OS::AllocateVirtualMemory(stack_size, OS::PAGE_FLAG_WRITE);
	Assert(stack_memory != null);

	thread->stack = Array<byte>(stack_memory, stack_size);
	thread->status = RUNNING;

	// Place create info at the base of the stack (low address, away from the top)
	ThreadCreateInfo* info = (ThreadCreateInfo*)stack_memory;
	info->function = thread_start_function;
	info->id = id;

	thread->system_thread_id = OS::CreateThread(stack_memory + stack_size, ThreadEntry, info);
	Assert(thread->system_thread_id > 0);

	return thread;
}

void Thread::Pause() {
	status = PAUSED;
	OS::SendPauseSignal(system_thread_id);
}

void Thread::Resume() {
	status = RUNNING;
	OS::FutexWake((volatile s32*)&status);
}

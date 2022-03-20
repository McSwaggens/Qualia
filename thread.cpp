#include "thread.h"
#include "print.h"
#include "assert.h"
#include "memory.h"
#include "linux.h"

static void InitThread(void)
{
}

static void KillCurrentThread(void)
{
}

static int32 GetSystemThreadID(void)
{
	return (int32)SystemCall(LINUX_SYSCALL_GET_TID);
}

static const uint64 THREAD_STACK_SIZE_DEFAULT = 1<<24;

static ThreadID CreateThread(uint64 stack_size, void (*thread_start_function)(ThreadID id))
{
	byte* stack_top = AllocateVirtualPage(stack_size, PAGE_FLAG_STACK);
	ThreadID thread_id = thread_id_counter;
	thread_id_counter++;

	struct CloneArguments
	{
		uint64 flags;
		uint64 pidfd; // FileHandle*
		uint64 child_tid; // uint64*
		uint64 parent_tid; // uint64*
		uint64 exit_signal;
		uint64 stack; // byte*
		uint64 stack_size;
		uint64 tls;
		uint64 set_tid; // uint64*
		uint64 set_tid_size; // uint64
		uint64 cgroup; // uint64
	};

	CloneArguments args;
	ZeroMemory(&args);

	args.flags
		= LINUX_CLONE_FLAG_THREAD
		| LINUX_CLONE_FLAG_VM
		| LINUX_CLONE_FLAG_FS
		| LINUX_CLONE_FLAG_IO
		| LINUX_CLONE_FLAG_FILES
		| LINUX_CLONE_FLAG_SIGHAND
		| LINUX_CLONE_FLAG_PARENT_SET_TID;

	int32 pidfd = 0;
	int32 parent_tid = 0;

	args.pidfd        = (uint64)&pidfd;
	args.child_tid    = (uint64)null; // uint64*
	args.parent_tid   = (uint64)&parent_tid; // uint64*
	args.exit_signal  = (uint64)0;
	args.stack        = (uint64)stack_top + stack_size; // byte*
	args.stack_size   = (uint64)stack_size;
	args.tls          = (uint64)0;
	args.set_tid      = (uint64)null; // uint64*
	args.set_tid_size = (uint64)0; // uint64
	args.cgroup       = (uint64)0; // uint64

	int32 linux_thread_id = (int32)SystemCall(LINUX_SYSCALL_CLONE3, (uint64)&args, sizeof(args));

	if (linux_thread_id < 0)
	{
		Assert();
	}
	else if (linux_thread_id == 0)
	{
		thread_start_function(thread_id);
		KillCurrentThread();
		AssertUnreachable();
	}

	return thread_id;
}


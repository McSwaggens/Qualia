#include "thread.h"
#include "assert.h"
#include "memory.h"
#include "linux.h"

static void InitThread(void)
{
}

static void KillCurrentThread(void)
{
}

static s32 GetSystemThreadID(void)
{
	return (s32)SystemCall(LINUX_SYSCALL_GET_TID);
}

static const u64 THREAD_STACK_SIZE_DEFAULT = 1<<24;

static ThreadID CreateThread(u64 stack_size, void (*thread_start_function)(ThreadID id))
{
	byte* stack_top = AllocateVirtualPage(stack_size, PAGE_FLAG_STACK);
	ThreadID thread_id = thread_id_counter;
	thread_id_counter++;

	struct CloneArguments
	{
		u64 flags;
		u64 pidfd; // FileHandle*
		u64 child_tid; // uint64*
		u64 parent_tid; // uint64*
		u64 exit_signal;
		u64 stack; // byte*
		u64 stack_size;
		u64 tls;
		u64 set_tid; // uint64*
		u64 set_tid_size; // uint64
		u64 cgroup; // uint64
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

	s32 pidfd = 0;
	s32 parent_tid = 0;

	args.pidfd        = (u64)&pidfd;
	args.child_tid    = (u64)null; // uint64*
	args.parent_tid   = (u64)&parent_tid; // uint64*
	args.exit_signal  = (u64)0;
	args.stack        = (u64)stack_top + stack_size; // byte*
	args.stack_size   = (u64)stack_size;
	args.tls          = (u64)0;
	args.set_tid      = (u64)null; // uint64*
	args.set_tid_size = (u64)0; // uint64
	args.cgroup       = (u64)0; // uint64

	s32 linux_thread_id = (s32)SystemCall(LINUX_SYSCALL_CLONE3, (u64)&args, sizeof(args));

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


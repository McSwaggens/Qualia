#include "file_system.h"
#include "general.h"
#include "linux.h"

[[noreturn]]
static void ExitProcess(bool success) {
	BufferFlush(&unix_output_buffer);
	BufferFlush(&unix_error_buffer);
	SystemCall(LINUX_SYSCALL_EXIT, !success);
	Unreachable();
}


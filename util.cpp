#include "util.h"
#include "file.h"

// For some (god forsaken) reason, using this declaration doesn't work
//     even though the signature is exactly the same...
//     So I'm extremely sad that I have to include unistd.h
//     boo...
//     @FixMe @Todo: Roll custom syscall implementation using an assembler.
// extern long int syscall(long int id, ...) __attribute__ ((__nothrow__, __leaf__));

#include <unistd.h>

u64 SystemCall(u64 rax, u64 rdi, u64 rsi, u64 rdx, u64 r10, u64 r8, u64 r9)
{
	return syscall(rax, rdi, rsi, rdx, r10, r8, r9);
}

[[noreturn]]
void ExitProcess(bool failure)
{
	standard_output_buffer.Flush();
	standard_error_buffer.Flush();
	SystemCall(60, failure);
	Unreachable();
}

[[noreturn]]
void Fail()
{
	ExitProcess(true);
}

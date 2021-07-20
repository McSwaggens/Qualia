#include "util.h"
#include "file.h"

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

#include "util.h"
#include "file.h"
#include <stdlib.h>

[[noreturn]]
void ExitProcess(bool failure)
{
	standard_output_buffer.Flush();
	standard_error_buffer.Flush();
	exit(failure ? EXIT_FAILURE : EXIT_SUCCESS);
}

[[noreturn]]
void Fail()
{
	ExitProcess(true);
}

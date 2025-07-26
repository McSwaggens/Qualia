#include "assert.h"

#include "os.h"
#include "print.h"
#include "string.h"

static void Assert(bool b, const char* desc, InternalLocation loc) {
	if (!IsDebug() || b) HOT
		return;

	Print("%:%: error: Assertion failed in %\n", ToString(loc.file), loc.line, ToString(loc.function));

	if (desc)
		Print("%:%: error: %\n", ToString(loc.file), loc.line, ToString(desc));

	DebugBreak();

	output_buffer.Flush();
	error_buffer.Flush();
	OS::Terminate(false);
}


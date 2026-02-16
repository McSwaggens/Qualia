#include "assert.h"

#include "os.h"
#include "print.h"
#include "string.h"

static void Assert(bool b, const char* desc, InternalLocation loc) {
	if (!IsDebug() || b) HOT
		return;

	Print("%:%: error: Assertion failed in %\n", CString(loc.file), loc.line, CString(loc.function));

	if (desc)
		Print("%:%: error: %\n", CString(loc.file), loc.line, CString(desc));

	PrintStackTrace();
	DebugBreak();

	output_buffer.Flush();
	error_buffer.Flush();
	OS::Terminate(false);
}


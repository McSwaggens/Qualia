#pragma once

#include "print.h"
#include "general.h"
#include "string.h"

#define StaticAssert(x) static_assert(x)

struct InternalLocation {
	String file;
	String function;
	s32 line;
	s32 offset;
};

static constexpr InternalLocation GetInternalLocation(
	String file     = GetInternalFileName(),
	String function = GetInternalFunctionName(),
	s32  line     = GetInternalLineNumber(),
	s32  offset   = GetInternalColumnNumber()) {
	InternalLocation location;
	location.file = file;
	location.function = function;
	location.line = line;
	location.offset = offset;
	return location;
}

static void Assert(bool b = false, String desc = null, InternalLocation loc = GetInternalLocation()) {
	if (IsDebug() && !b) {
		Print("%:%: error: Assertion failed in %\n", loc.file, loc.line, loc.function);

		if (desc) {
			Print("%:%: error: %\n", loc.file, loc.line, desc);
		}

		// DebugBreak();

		ExitProcess(false);
	}
}

// [[noreturn]]
// static void Assert(String message, InternalLocation loc = GetSourcePosition())
// {
// 	if (IsDebug())
// 	{
// 		Print("%:%: error: % (%)\n", loc.file, loc.line, message, loc.function);
// 	}

// 	ExitProcess(false);
// }


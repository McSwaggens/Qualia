#pragma once

#include "general.h"

#define StaticAssert(x) static_assert(x)
#define Log(x) Print(#x " = %\n", x);

struct InternalLocation {
	const char* file;
	const char* function;
	s32 line;
	s32 offset;
};

static InternalLocation GetInternalLocation(
	const char* file     = GetInternalFileName(),
	const char* function = GetInternalFunctionName(),
	s32  line            = GetInternalLineNumber(),
	s32  offset          = GetInternalColumnNumber()) {
	return (InternalLocation){
		.file     = file,
		.function = function,
		.line     = line,
		.offset   = offset,
	};
}

static void Assert(bool b = false, const char* desc = null, InternalLocation loc = GetInternalLocation());


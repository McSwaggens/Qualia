#pragma once

#include "int.h"
#include "print.h"
#include "util.h"
#include "string.h"

#define StaticAssert(x) static_assert(x)

struct InternalSourceLocation
{
	s32 line;
	String function;
	String file;
};

static constexpr InternalSourceLocation GetSourcePosition(
	s32 line = __builtin_LINE(),
	const char* function = __builtin_FUNCTION(),
	const char* file = __builtin_FILE())
{
	InternalSourceLocation pos;
	pos.line = line;
	pos.function = ToString(function);
	pos.file = ToString(file);
	return pos;
}

static void Assert(bool b = false, String desc = null, InternalSourceLocation pos = GetSourcePosition())
{
	if (!b)
	{
		if (IsDebug())
		{
			Print("%:%: error: Assertion failed in %\n", pos.file, pos.line, pos.function);

			if (desc)
			{
				Print("%:%: error: %\n", pos.file, pos.line, desc);
			}
		}

		Fail();
	}
}

static void Assert(String message, InternalSourceLocation pos = GetSourcePosition())
{
	if (IsDebug())
	{
		Print("%:%: error: % (%)\n", pos.file, pos.line, message, pos.function);
	}

	Fail();
}

template<u32 N>
static void Assert(const char (&s)[N], InternalSourceLocation pos = GetSourcePosition())
{
	Assert(String(s, N-1), pos);
}


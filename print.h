#pragma once

#include "int.h"
#include "string.h"
#include "file.h"
#include "span.h"
#include "list.h"

void Write(OutputBuffer* buffer, u8  n);
void Write(OutputBuffer* buffer, u16 n);
void Write(OutputBuffer* buffer, u32 n);
void Write(OutputBuffer* buffer, u64 n);

void Write(OutputBuffer* buffer, s8  n);
void Write(OutputBuffer* buffer, s16 n);
void Write(OutputBuffer* buffer, s32 n);
void Write(OutputBuffer* buffer, s64 n);

// Need this, otherwise sizeof won't work...
static inline void Write(OutputBuffer* buffer, unsigned long int n)
{
	Write(buffer, (u64)n);
}

void Write(OutputBuffer* buffer, f32 n);
void Write(OutputBuffer* buffer, f64 n);

static inline void Write(OutputBuffer* buffer, String str)
{
	if (str)
	{
		buffer->Write(str, str.length);
	}
	else
	{
		buffer->Write("<null-string>", 13);
	}
}

// Need to provide explicit definition otherwise it will default to bool... Thanks C++!
template<u64 size>
static inline void Write(OutputBuffer* buffer, const char (&s)[size])
{
	if constexpr (size < 2) return;
	if constexpr (size == 2) buffer->Write(s[0]);
	else buffer->Write(s, size-1);
}

static inline void Write(OutputBuffer* buffer, char c)
{
	buffer->Write(c);
}

struct Token;

void Write(OutputBuffer* buffer, Span<Token> span);

template<typename T>
static void Write(OutputBuffer* buffer, Span<T> span)
{
	for (u64 i = 0; i < span.Length(); i++)
	{
		if (i != 0) Write(buffer, " ");
		Write(buffer, span[i]);
	}
}

static void Write(OutputBuffer* buffer, Span<char> span)
{
	buffer->Write(span, span.Length());
	// Write(buffer, "{ ");
	// for (u64 i = 0; i < span.Length(); i++)
	// {
	// 	if (i != 0) Write(buffer, ", ");
	// 	Write(buffer, span[i]);
	// }
	// Write(buffer, " }");
}

template<typename T>
static void Write(OutputBuffer* buffer, List<T> list)
{
	Write(buffer, (Span<T>)list);
}

template<typename ...Args>
static void Print(OutputBuffer* buffer, String format, Args&&... args)
{
	char* end = format.data + format.length;
	char* p = format.data;

	auto internal_print = [=, &p]<typename T>(T&& t)
	{
		char* start = p;

		while (p < end && *p != '%') p++;

		if (start != p)
		{
			buffer->Write(start, p-start);
		}

		if (p < end)
		{
			Write(buffer, t);
			p++;
		}
	};

	(internal_print(args),...);

	if (p < end)
	{
		buffer->Write(p, end - p);
	}
}

template<typename ...Args>
static void Print(String format, Args&&... args)
{
	Print(&standard_output_buffer, format, args...);
}

static inline void Write(OutputBuffer* buffer, bool b)
{
	if (b)
	{
		buffer->Write("true", 4);
	}
	else {
		buffer->Write("false", 5);
	}
}


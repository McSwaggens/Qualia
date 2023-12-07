#pragma once

#include "general.h"
#include "string.h"
#include "file_system.h"
#include "list.h"
#include "ascii.h"

struct IntFormat
{
	Base base;
	uint64 value;
};

static inline IntFormat Hex(uint64 n) { return (IntFormat){ .base = BASE_HEX,    .value = n }; }
static inline IntFormat Bin(uint64 n) { return (IntFormat){ .base = BASE_BINARY, .value = n }; }

static void Write(OutputBuffer* buffer, IntFormat format);

static void Write(OutputBuffer* buffer, char   c);
static void Write(OutputBuffer* buffer, uint8  n);
static void Write(OutputBuffer* buffer, uint16 n);
static void Write(OutputBuffer* buffer, uint32 n);
static void Write(OutputBuffer* buffer, uint64 n);

static void Write(OutputBuffer* buffer, int8  n);
static void Write(OutputBuffer* buffer, int16 n);
static void Write(OutputBuffer* buffer, int32 n);
static void Write(OutputBuffer* buffer, int64 n);

static void Write(OutputBuffer* buffer, float32 n);
static void Write(OutputBuffer* buffer, float64 n);

static void Write(OutputBuffer* buffer, unsigned long int n); // Need this, otherwise sizeof won't work...
static void Write(OutputBuffer* buffer, void* p);

static void Write(OutputBuffer* buffer, String str);

template<typename T>
static void Write(OutputBuffer* buffer, List<T> list)
{
	Write(buffer, list.ToSpan());
}

template<typename T>
static void Write(OutputBuffer* buffer, Array<T> array)
{
	BufferWriteString(buffer, "{ ");

	for (uint64 i = 0; i < array.count; i++)
	{
		if (i != 0) BufferWriteString(buffer, ", ");
		Write(buffer, array[i]);
	}

	BufferWriteString(buffer, " }");
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
			BufferWriteData(buffer, start, p-start);
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
		BufferWriteData(buffer, p, end - p);
	}
}

template<typename ...Args>
static void Print(String format, Args&&... args)
{
	Print(&unix_output_buffer, format, args...);
}

static void Write(OutputBuffer* buffer, bool b)
{
	if (b) BufferWriteString(buffer, "true");
	else   BufferWriteString(buffer, "false");
}


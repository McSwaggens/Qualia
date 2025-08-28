#ifndef PRINT_H
#define PRINT_H

#include "general.h"
#include "string.h"
#include "file_system.h"
#include "list.h"
#include "ascii.h"

struct IntFormat {
	Base base;
	u64 value;
};

static inline IntFormat Hex(u64 n) { return (IntFormat){ .base = BASE_HEX,    .value = n }; }
static inline IntFormat Bin(u64 n) { return (IntFormat){ .base = BASE_BINARY, .value = n }; }

static void Write(OutputBuffer* buffer, IntFormat format);

static void Write(OutputBuffer* buffer, char   c);
static void Write(OutputBuffer* buffer, u8  n);
static void Write(OutputBuffer* buffer, u16 n);
static void Write(OutputBuffer* buffer, u32 n);
static void Write(OutputBuffer* buffer, u64 n);

static void Write(OutputBuffer* buffer, s8  n);
static void Write(OutputBuffer* buffer, s16 n);
static void Write(OutputBuffer* buffer, s32 n);
static void Write(OutputBuffer* buffer, s64 n);

static void Write(OutputBuffer* buffer, float32 n);
static void Write(OutputBuffer* buffer, float64 n);

static void Write(OutputBuffer* buffer, unsigned long int n); // Need this, otherwise sizeof won't work...
static void Write(OutputBuffer* buffer, void* p);

static void Write(OutputBuffer* buffer, String str);

template<typename T>
static void Write(OutputBuffer* buffer, Array<T> array) {
	buffer->Write("{ ");

	for (u64 i = 0; i < array.length; i++) {
		if (i != 0) buffer->Write(", ");
		Write(buffer, array[i]);
	}

	buffer->Write(" }");
}

template<typename T>
static void Write(OutputBuffer* buffer, List<T> list) {
	Write(buffer, list.ToArray());
}

template<typename ...Args>
static void Print(OutputBuffer* buffer, String format, Args&&... args) {
	char* end = format.data + format.length;
	char* p = format.data;

	auto internal_print = [=, &p]<typename T>(T&& t) {
		char* start = p;

		while (p < end && *p != '%')
			p++;

		if (start != p)
			buffer->Write(start, p-start);

		if (p < end) {
			Write(buffer, t);
			p++;
		}
	};

	(internal_print(args),...);

	if (p < end)
		buffer->Write(p, end - p);

	if (IsDebug())
		buffer->Flush();
}

template<typename ...Args>
static void Print(String format, Args&&... args) {
	Print(&output_buffer, format, args...);
}

static void Write(OutputBuffer* buffer, bool b) {
	if (b) buffer->Write("true");
	else   buffer->Write("false");
}

#endif // PRINT_H

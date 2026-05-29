#pragma once

#include "general.h"
#include "string.h"
#include "file_system.h"
#include "list.h"
#include "ascii.h"

struct Hex {
	u64 value;
	constexpr Hex(u64 value) : value(value) { }
	void Print(OutputBuffer* buffer) const;
};

struct Bin {
	u64 value;
	constexpr Bin(u64 value) : value(value) { }
	void Print(OutputBuffer* buffer) const;
};

static void Print(OutputBuffer* buffer, char c);
static void Print(OutputBuffer* buffer, u8  n);
static void Print(OutputBuffer* buffer, u16 n);
static void Print(OutputBuffer* buffer, u32 n);
static void Print(OutputBuffer* buffer, u64 n);

static void Print(OutputBuffer* buffer, s8  n);
static void Print(OutputBuffer* buffer, s16 n);
static void Print(OutputBuffer* buffer, s32 n);
static void Print(OutputBuffer* buffer, s64 n);

static void Print(OutputBuffer* buffer, float32 n);
static void Print(OutputBuffer* buffer, float64 n);

static void Print(OutputBuffer* buffer, unsigned long int n); // Need this, otherwise sizeof won't work...
static void Print(OutputBuffer* buffer, void* p);
static void Print(OutputBuffer* buffer, bool b);

// String is the format type, so it gets a dedicated non-template overload.
// A non-template beats both the variadic format function and the member
// fallback below on a tie, so a lone String is never ambiguous.
static void Print(OutputBuffer* buffer, String str);

// Fallback: any type with a member T::Print(OutputBuffer*).
template<typename T>
requires requires (T& value, OutputBuffer* buffer) { value.Print(buffer); }
static void Print(OutputBuffer* buffer, T&& value) {
	value.Print(buffer);
}

// Fallback: any pointer whose pointee has T::Print(OutputBuffer*).
template<typename T>
requires requires (T* ptr, OutputBuffer* buffer) { ptr->Print(buffer); }
static void Print(OutputBuffer* buffer, T* ptr) {
	if (!ptr) {
		buffer->Write("null");
		return;
	}

	ptr->Print(buffer);
}

template<typename... Args>
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
			Print(buffer, t);
			p++;
		}
	};

	(internal_print(args),...);

	if (p < end)
		buffer->Write(p, end - p);

	if (IsDebug())
		buffer->Flush();
}

#define DebugVar(x) do { Print(#x " = %\n", x); output_buffer.Flush(); } while (0);

template<typename... Args>
static void Print(String format, Args&&... args) {
	Print(&output_buffer, format, args...);
}

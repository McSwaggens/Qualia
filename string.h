#pragma once

#include "int.h"
#include "memory.h"
#include "span.h"

struct String
{
	char* data;
	u32   length;
	u32   capacity;

	constexpr String() = default;
	constexpr String(Null) : data(null), length(0), capacity(0) { }
	constexpr String(char* data, u32 length, u32 cap = 0) : data(data), length(length), capacity(cap) { }
	constexpr String(const char* data, u32 length, u32 cap = 0) : data(const_cast<char*>(data)), length(length), capacity(cap) { }

	template<u32 N>
	constexpr String(const char (&s)[N]) : data(const_cast<char*>(s)), length(N-1), capacity(0) { }

	constexpr operator Span<char>() { return { data, data + length }; }
	constexpr operator char*() { return data; }
	constexpr operator bool() const { return data == null; }

	constexpr char& operator[](u32 n) { return data[n]; }
	constexpr char  operator[](u32 n) const { return data[n]; }

	constexpr Span<char> ToSpan()
	{
		return { data, data + length };
	}

	constexpr char* Begin() { return data; }
	constexpr char* End()   { return data + length; }

	constexpr const char* Begin() const { return data; }
	constexpr const char* End()   const { return data + length; }

	constexpr void Clear()
	{
		length = 0;
	}

	void Add(String other)
	{
		if (length + other.length >= capacity)
		{
			capacity = capacity * 2 + other.length;
			ReAllocate(data, capacity);
		}

		CopyMemory(data + length, other.data, other.length);
		length += other.length;
	}

	void Add(char c)
	{
		if (length + 1 >= capacity)
		{
			capacity += length + 1;
			ReAllocate(data, capacity);
		}

		data[length++] = c;
	}
};

[[nodiscard]]
static inline String CreateString(u64 capacity = 16)
{
	return String(Allocate<char>(capacity), 0, capacity);
}

[[nodiscard]]
static String CopyString(String string)
{
	String copy = String(Allocate<char>(string.length), string.length, string.capacity);
	CopyMemory(copy.data, string.data, string.length);
	return copy;
}

static constexpr bool Compare(String a, String b)
{
	return a.length == b.length && CompareMemory(a.data, b.data, a.length);
}

static constexpr bool CompareStrings(String a, String b)
{
	return a.length == b.length && CompareMemory(a.data, b.data, a.length);
}

static constexpr u64 CStringLength(const char* s)
{
	// What kind of demented "person" would "design" a string like this?
	// It's an extremely simple concept, but still they managed to fuck it up somehow.
	//                    C was a mistake.
	const char* start = s;
	while (*s) s++;
	return s - start;
}

[[nodiscard]]
static constexpr String ToString(const char* cstr)
{
	return String(cstr, CStringLength(cstr));
}


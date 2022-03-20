#pragma once

#include "memory.h"
#include "span.h"

struct String
{
	char* data;
	uint32 length;
	uint32 capacity;

	constexpr String() = default;
	constexpr String(Null) : data(null), length(0), capacity(0) { }
	constexpr String(char* data, uint32 length, uint32 cap = 0) : data(data), length(length), capacity(cap) { }
	constexpr String(char* begin, char* end) : data(begin), length(end-begin), capacity(0) { }
	constexpr String(const char* data, uint32 length, uint32 cap = 0) : data(const_cast<char*>(data)), length(length), capacity(cap) { }

	template<uint64 N>
	constexpr String(const char (&s)[N]) : data(const_cast<char*>(s)), length(N-1), capacity(0) { }
	constexpr operator bool() const { return data != null; }

	constexpr char& operator[](uint32 n) { return data[n]; }
	constexpr char  operator[](uint32 n) const { return data[n]; }

	constexpr char* Begin() { return data; }
	constexpr char* End()   { return data + length; }

	constexpr const char* Begin() const { return data; }
	constexpr const char* End()   const { return data + length; }

	constexpr void Clear()
	{
		length = 0;
	}

	void Append(String other)
	{
		if (length + other.length >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity = capacity * 2 + other.length;
			data = ReAllocate(data, old_capacity, capacity);
		}

		CopyMemory(data + length, other.data, other.length);
		length += other.length;
	}

	void Add(char c)
	{
		if (length + 1 >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity += length + 1;
			data = ReAllocate(data, old_capacity, capacity);
		}

		data[length++] = c;
	}
};

static inline String AllocateString(uint64 length, uint64 extra_capacity)
{
	return String(Allocate<char>(length+extra_capacity), length, length+extra_capacity);
}

static inline String StackAllocateString(Stack* stack, uint64 length)
{
	return String(StackAllocate<char>(stack, length), length, 0);
}

static inline void DeAllocateString(String string)
{
	DeAllocate(string.data, string.length);
}

static String DuplicateString(String string)
{
	String copy = String(Allocate<char>(string.length), string.length, string.capacity);
	CopyMemory(copy.data, string.data, string.length);
	return copy;
}

static String DuplicateString(char* s, uint64 length)
{
	String copy = AllocateString(length, 0);
	CopyMemory(copy.data, s, length);
	return copy;
}

static constexpr bool CompareString(String a, String b)
{
	return a.length == b.length && CompareMemory(a.data, b.data, a.length);
}

template<uint64 N>
static inline bool CompareStringRaw(const char* a, const char (&b)[N])
{
	return CompareMemory(a, b, N-1);
}

static constexpr uint64 CStringLength(const char* s)
{
	// What kind of demented "person" would "design" a string like this?
	// It's an extremely simple concept, but still they managed to fuck it up somehow.
	//                    C was a mistake.
	const char* start = s;
	while (*s) s++;
	return s - start;
}

static constexpr String ToString(const char* cstr)
{
	return String(cstr, CStringLength(cstr));
}


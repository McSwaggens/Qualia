#pragma once

#include "memory.h"

#include "assert.h"

struct String {
	// @todo Short String Optimization!
	u32 length;
	char* data;
	u32 capacity;

	String() = default;
	constexpr String(Null) : data(null), length(0), capacity(0) { }
	constexpr String(char* data, u32 length, u32 cap = 0) : data(data), length(length), capacity(cap) { }
	constexpr String(char* begin, char* end) : data(begin), length(end-begin), capacity(0) { }
	constexpr String(const char* data, u32 length, u32 cap = 0) : data(const_cast<char*>(data)), length(length), capacity(cap) { }

	template<u64 N>
	constexpr String(const char (&s)[N]) : data(const_cast<char*>(s)), length(N-1), capacity(0) { }

	constexpr  operator       char*() { return data; }
	constexpr  operator const char*() const { return data; }

	constexpr       char* Begin()       { return data; }
	constexpr const char* Begin() const { return data; }

	constexpr       char* End()       { return data + length; }
	constexpr const char* End() const { return data + length; }

	constexpr       char* begin()       { return data; }
	constexpr const char* begin() const { return data; }

	constexpr       char* end()       { return data + length; }
	constexpr const char* end() const { return data + length; }

	constexpr operator bool() const { return length != 0; }

	char& operator[](u32 n)       { Assert(n < length); return data[n]; }
	char  operator[](u32 n) const { Assert(n < length); return data[n]; }

	template<u64 N>
	constexpr bool operator ==(const char (&s)[N]) {
		if (length != N-1)
			return false;

		if (!CompareMemory(data, s, N-1))
			return false;

		return true;
	}

	constexpr bool operator ==(String o) {
		if (length != o.length)
			return false;

		if (data == o.data)
			return true;

		if (!CompareMemory(data, o.data, length))
			return false;

		return true;
	}

	constexpr bool operator !=(String o) {
		return !(*this == o);
	}

	constexpr void Clear() {
		length = 0;
	}

	void Append(String other) {
		if (length + other.length >= capacity) {
			u32 old_capacity = capacity;
			capacity = capacity * 2 + other.length;
			data = ReAlloc(data, old_capacity, capacity);
		}

		CopyMemory(data + length, other.data, other.length);
		length += other.length;
	}

	void Add(char c) {
		if (length + 1 >= capacity) {
			u32 old_capacity = capacity;
			capacity += length + 1;
			data = ReAlloc(data, old_capacity, capacity);
		}

		data[length++] = c;
	}

	constexpr bool StartsWith(String s) {
		if (length < s.length)
			return false;

		if (!CompareMemory(data, s.data, s.length))
			return false;

		return true;
	}

	constexpr bool EndsWith(String s) {
		if (length < s.length)
			return false;

		if (!CompareMemory(End() - s.length, s.data, s.length))
			return false;

		return true;
	}

	String Copy() {
		String copy = String(Alloc<char>(length), length, capacity);
		CopyMemory(copy.data, data, length);
		return copy;
	}

	void Copy(char* out) {
		CopyMemory(out, data, length);
	}

	void Free() {
		::Free(data, capacity);

		data = null;
		length = 0;
		capacity = 0;
	}
};

static inline String AllocateString(u64 length, u64 extra_capacity) {
	return String(Alloc<char>(length+extra_capacity), length, length+extra_capacity);
}

template<u64 N>
static inline bool CompareStringRaw(const char* a, const char (&b)[N]) {
	return CompareMemory(a, b, N-1);
}

static constexpr u64 CStringLength(const char* s) {
	const char* start = s;
	while (*s) s++;
	return s - start;
}

static constexpr String ToString(const char* cstr) {
	return String(cstr, CStringLength(cstr));
}


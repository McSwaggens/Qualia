#pragma once

#include "int.h"

#include "assert.h"

template<typename T>
struct Array {
	T*  data   = null;
	u64 length = 0;

	constexpr Array() = default;
	constexpr Array(T* elements, u64 count) : data(elements), length(count) { }
	constexpr Array(T* begin, T* end) : data(begin), length(end - begin) { Assert(begin <= end); }
	constexpr Array(Null) : data(null), length(0) { }
	template<u64 N>
	constexpr Array(const T (&a)[N]) : data((T*)a), length(N) { }

	constexpr operator T*() { return data; }
	constexpr operator const T*() const { return data; }
	constexpr operator bool() const { return static_cast<bool>(data); }
	constexpr T& operator[](u64 n)       { Assert(n < length); return data[n]; }
	constexpr T  operator[](u64 n) const { Assert(n < length); return data[n]; }

	constexpr       T* Begin()       { return data; }
	constexpr const T* Begin() const { return data; }
	constexpr       T* begin()       { return data; }
	constexpr const T* begin() const { return data; }

	constexpr       T* End()       { return data + length; }
	constexpr const T* End() const { return data + length; }
	constexpr       T* end()       { return data + length; }
	constexpr const T* end() const { return data + length; }

	constexpr bool operator ==(Array<T> o) {
		if (length != o.length)
			return false;

		if (data != o.data)
			return false;

		for (u64 i = 0; i < length; i++)
			if (!Compare(data[i], o.data[i]))
				return false;

		return true;
	}

	constexpr bool operator !=(Array<T> o) { return !(*this == o); }
};

template<typename T>
static inline bool Compare(Array<T> a, Array<T> b) {
	return a == b;
}


#pragma once

#include "int.h"

template<typename T>
struct Array
{
	T* elements;
	u64 count;

	constexpr Array() = default;
	constexpr Array(T* elements, u64 count) : elements(elements), count(count) { }
	constexpr Array(Null) : elements(null), count(0) { }

	constexpr operator T*() { return elements; }
	constexpr operator const T*() const { return elements; }
	constexpr operator bool() const { return static_cast<bool>(elements); }
	constexpr T& operator[](u64 n) { return elements[n]; }
	constexpr T  operator[](u64 n) const { return elements[n]; }

	constexpr T* Begin() { return elements; }
	constexpr T* End()   { return elements + count; }
};

template<typename T>
static inline bool Compare(Array<T> a, Array<T> b) {
	return a.count == b.count && (a.elements == b.elements || Compare(a.elements, b.elements, a.count));
}


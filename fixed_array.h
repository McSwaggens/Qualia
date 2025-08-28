#ifndef FIXED_ARRAY_H
#define FIXED_ARRAY_H

#include "int.h"
#include "array.h"
#include "assert.h"

template<typename T, u64 N>
struct FixedArray {
	static constexpr u64 COUNT = N;

	T data[COUNT] = { };

	constexpr FixedArray() = default;
	constexpr FixedArray(T (&a)[COUNT]) : data(a) { }

	template<typename U>
	constexpr FixedArray(FixedArray<U, COUNT> o) : data(o.data) { }

	constexpr T& operator[](u64 n)       { Assert(n < COUNT); return data[n]; }
	constexpr T  operator[](u64 n) const { Assert(n < COUNT); return data[n]; }

	constexpr       T* Begin()       { return data; }
	constexpr const T* Begin() const { return data; }
	constexpr       T* begin()       { return data; }
	constexpr const T* begin() const { return data; }

	constexpr       T* End()       { return data + COUNT; }
	constexpr const T* End() const { return data + COUNT; }
	constexpr       T* end()       { return data + COUNT; }
	constexpr const T* end() const { return data + COUNT; }

	constexpr          Array<T> ToArray() { return { data, COUNT }; }
	constexpr operator Array<T>() { return ToArray(); }

	constexpr bool operator ==(FixedArray<T, COUNT> o) {
		for (u64 i = 0; i < COUNT; i++)
			if (data[i] != o.data[i])
				return false;

		return true;
	}

	constexpr bool operator !=(FixedArray<T, COUNT> o) { return !(*this == o); }
};

template<typename U, u64 N, typename V, u64 M>
static constexpr int Compare(FixedArray<U, N> a, FixedArray<V, M> b) {
	return Compare(a.ToArray(), b.ToArray());
}


#endif // FIXED_ARRAY_H

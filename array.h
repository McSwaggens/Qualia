#pragma once

#include "int.h"
#include "initlist.h"

template<typename T>
struct Array
{
	T* data;
	u32 count;

	constexpr Array() = default;
	constexpr Array(T* data, u32 count) : data(data), count(count) { }
	constexpr Array(InitList<T> initlist) : data(const_cast<T*>(initlist.begin())), count(initlist.size()) { }
	constexpr Array(Null) : data(null), count(0) { }
	constexpr operator T*() { return data; }
	constexpr operator const T*() const { return data; }
	constexpr operator bool() const { return static_cast<bool>(data); }
	constexpr T& operator[](u32 n) { return data[n]; }
	constexpr T  operator[](u32 n) const { return data[n]; }

	constexpr T* Begin() { return data; }
	constexpr T* End()   { return data + count; }
};

template<typename T>
static inline bool Compare(Array<T> a, Array<T> b)
{
	return a.count == b.count && (a.data == b.data || Compare(a.data, b.data, a.count));
}


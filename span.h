#pragma once

#include "int.h"
#include "initlist.h"

template<typename T>
struct Span
{
	T* data; // Dog-shit-ass language won't allow me to use `begin` and `end`
	T* end;  //  because range based for loops will TRY TO FUCKING CALL THEM!

	constexpr Span() = default;
	constexpr Span(Null) : data(null), end(null) { }
	constexpr Span(T* begin, T* end) : data(begin), end(end) { }
	constexpr Span(T* begin, u64 length) : data(begin), end(begin+length) { }
	constexpr Span(InitList<T> list) : data(list.begin()), end(list.end()) { }
	constexpr operator bool() const { return static_cast<bool>(data); }
	constexpr operator T*() { return data; }
	constexpr T& operator[](u64 n) { return data[n]; }
	constexpr T  operator[](u64 n) const { return data[n]; }
	constexpr u64 Length() { return end - data; }
	constexpr T* End() { return end; }
};

template<typename T>
static constexpr u64 Length(Span<T> span)
{
	return span.end - span.data;
}

template<typename T>
static constexpr void Zero(Span<T> span)
{
	Zero(span.data, span.end);
}

template<typename T>
static constexpr T* begin(Span<T> span)
{
	return span.data;
}

template<typename T>
static constexpr T* end(Span<T> span)
{
	return span.end;
}


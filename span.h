#pragma once

#include "int.h"
#include "initlist.h"

template<typename T>
struct Span
{
	// T* data; // Dog-shit-ass language won't allow me to use `begin` and `end`
	// T* end;  //  because range based for loops will TRY TO FUCKING CALL THEM!

	T* begin;
	T* end;

	constexpr Span() = default;
	constexpr Span(Null) : begin(null), end(null) { }
	constexpr Span(T* begin, T* end) : begin(begin), end(end) { }
	constexpr Span(T* begin, u64 length) : begin(begin), end(begin+length) { }
	constexpr Span(InitList<T> list) : begin(list.begin()), end(list.end()) { }
	constexpr operator bool() const { return static_cast<bool>(begin); }
	constexpr operator T*() { return begin; }
	constexpr T& operator[](u64 n) { return begin[n]; }
	constexpr T  operator[](u64 n) const { return begin[n]; }
	constexpr u64 Length() { return end - begin; }
	constexpr T* Begin() { return begin; }
	constexpr T* End() { return end; }
};

template<typename T>
static constexpr u64 Length(Span<T> span)
{
	return span.end - span.begin;
}

template<typename T>
static constexpr void ZeroMemory(Span<T> span)
{
	ZeroMemory(span.begin, span.end);
}


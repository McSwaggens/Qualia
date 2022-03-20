#pragma once

template<typename T>
struct Span
{
	T* begin;
	T* end;

	constexpr Span() = default;
	constexpr Span(Null) : begin(null), end(null) { }
	constexpr Span(T* begin, T* end) : begin(begin), end(end) { }
	constexpr operator bool() const { return begin != end; }
	constexpr T& operator[](uint64 n) { return begin[n]; }
	constexpr T  operator[](uint64 n) const { return begin[n]; }
	constexpr uint64 Length() { return end - begin; }
};

template<typename T>
static constexpr void ZeroMemory(Span<T> span)
{
	ZeroMemory(span.begin, span.end);
}


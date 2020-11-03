#pragma once

#include "int.h"

#define Cast(E, T) ((T)E)

static consteval bool IsDebug()
{
#if !defined(NDEBUG)
	return true;
#else
	return false;
#endif
}

static constexpr bool IsConstEval()
{
	return __builtin_is_constant_evaluated();
}

template<u64 Alignment>
[[nodiscard]]
static constexpr auto* AssumeAligned(auto* p)
{
	return __builtin_assume_aligned(p, Alignment);
}

static constexpr void Assume(bool b)
{
	__builtin_assume(b);
}

#define Unreachable() __builtin_unreachable()
// static inline void Unreachable()
// {
// 	__builtin_unreachable();
// }

template<typename T, typename... Args>
static constexpr bool IsAnyOf(T t, Args... args)
{
	static_assert(sizeof...(args) > 0);
	return ((t == args) || ...);
}

template<typename T>
static constexpr T Max(T a, T b)
{
	return a >= b ? a : b;
}

template<typename T>
static constexpr T Min(T a, T b)
{
	return a < b ? a : b;
}

template<typename T>
static constexpr T Clamp(T v, T min, T max)
{
	return v < min ? min : v > max ? max : v;
}

template<typename T>
static constexpr void Swap(T& a, T& b)
{
	T c = a;
	a = b;
	b = c;
}

static constexpr auto Abs(auto n)
{
	return n >= 0 ? n : -n;
}

static constexpr auto Pow(auto base, auto exponent)
{
	return __builtin_powl(base, exponent); // @Bug @FixMe: This isn't correct.
}

static constexpr auto MaskLowerBits(auto n, auto bits)   { return n & (-1 >> (sizeof(n) * 8 - bits)); }
static constexpr auto MaskLowerBytes(auto n, auto bytes) { return n & (-1 >> (sizeof(n) - bytes) * 8); }

static constexpr auto MaskUpperBits(auto n, auto bits)   { return n >> (sizeof(n) * 8 - bits); }
static constexpr auto MaskUpperBytes(auto n, auto bytes) { return n >> (sizeof(n) - bytes) * 8; }

static constexpr bool IsUpperCase(char c)
{
	return c >= 'A' && c <= 'Z';
}

static constexpr bool IsLowerCase(char c)
{
	return c >= 'a' && c <= 'z';
}

static constexpr bool IsLetter(char c)
{
	return IsUpperCase(c) || IsLowerCase(c);
}

enum DigitBase
{
	Base2  = 2,
	Base8  = 8,
	Base10 = 10,
	Base16 = 16,
	Binary = Base2,
	Octal = Base8,
	Decimal = Base10,
	Hexadecimal = Base16,
};

static constexpr bool IsDigit(char c, DigitBase base = Base10)
{
	switch (base)
	{
		case Base16: return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
		case Base10: return c >= '0' && c <= '9';
		case Base8:  return c >= '0' && c <= '7';
		case Base2:  return c == '0' || c == '1';
	}
}

static constexpr u32 DigitToInt(char c, DigitBase base = Base10)
{
	// @CleanMe This should be cleaned up...
	switch (base)
	{
		case Base16:
			if (c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f') return (c & 0xDF) - ('A' - 10);

		case Base10:
		case Base8:
		case Base2:
			return c - '0';
	}
}

static constexpr bool IsWhiteSpace(char c)
{
	return c == '\t' || c == ' '; // @FixMe: Think this needs to be expanded?
}

static constexpr bool IsPrintable(char c)
{
	return c >= ' ' && c < 0x7F;
}

static constexpr bool IsPow2(u64 n)
{
	return n != 0 && (n & n-1) == 0;
}

static constexpr u64 NextPow2(u8 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n++;
	return n;
}

static constexpr u64 NextPow2(u16 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n++;
	return n;
}

static constexpr u64 NextPow2(u32 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n |= n >> 16;
	n++;
	return n;
}

static constexpr u64 NextPow2(u64 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n |= n >> 16;
	n |= n >> 32;
	n++;
	return n;
}

static constexpr u32 CountBits(u32 n)
{
	return __builtin_popcount(n);
}

static constexpr u64 CountBits(u64 n)
{
	return __builtin_popcountll(n);
}

template<typename T>
static constexpr void Reverse(T* items, u64 count)
{
	for (u64 i = 0; i < count / 2; i++)
	{
		Swap(items[i], items[count-i-1]);
	}
}

[[noreturn]]
void ExitProcess(bool failure);

[[noreturn]]
void Fail();


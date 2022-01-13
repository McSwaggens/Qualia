#pragma once

#include "int.h"

#define COLD [[unlikely]]
#define HOT [[likely]]

#if defined(DEBUG)
#define DebugBreak() __builtin_debugtrap()
#endif

static void InitTime();

static consteval bool IsDebug()
{
#if defined(DEBUG)
	return true;
#else
	return false;
#endif
}

static bool IsConstEval()
{
	return __builtin_is_constant_evaluated();
}

template<u64 Alignment>
[[nodiscard]]
static auto* AssumeAligned(auto* p)
{
	return __builtin_assume_aligned(p, Alignment);
}

static void Assume(bool b)
{
	__builtin_assume(b);
}

#define Unreachable() __builtin_unreachable()

template<typename T>
static T Max(T a, T b)
{
	return a >= b ? a : b;
}

template<typename T>
static T Min(T a, T b)
{
	return a < b ? a : b;
}

template<typename T>
static T Clamp(T v, T min, T max)
{
	return v < min ? min : v > max ? max : v;
}

template<typename T>
static void Swap(T& a, T& b)
{
	T c = a;
	a = b;
	b = c;
}

static auto Abs(auto n)
{
	return n >= 0 ? n : -n;
}

static auto Pow(auto base, auto exponent)
{
	return __builtin_powl(base, exponent); // @Bug @FixMe: This isn't correct.
}

static auto MaskLowerBits(auto n, u64 bits)   { return n & (-1 >> (sizeof(n) * 8 - bits)); }
static auto MaskLowerBytes(auto n, u64 bytes) { return n & (-1 >> (sizeof(n) - bytes) * 8); }

static auto MaskUpperBits(auto n, u64 bits)   { return n >> (sizeof(n) * 8 - bits); }
static auto MaskUpperBytes(auto n, u64 bytes) { return n >> (sizeof(n) - bytes) * 8; }

static bool IsLowerCase(char c)
{
	return (u8)(c - 'a') <= (u8)('z' - 'a');
}

static bool IsUpperCase(char c)
{
	return (u8)(c - 'A') <= (u8)('Z' - 'A');
}

static bool IsLowerCaseQuick(char c)
{
	return c & 0x20;
}

static bool IsUpperCaseQuick(char c)
{
	return !(c & 0x20);
}

static bool IsLetter(char c)
{
	return (u8)((c | 0x20) - 'a') <= (u8)('z' - 'a');
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

static bool IsDigit(char c, DigitBase base = Base10)
{
	switch (base)
	{
		case Base16: return (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
		case Base10: return c >= '0' && c <= '9';
		case Base8:  return c >= '0' && c <= '7';
		case Base2:  return c == '0' || c == '1';
	}
}

static u32 DigitToInt(char c, DigitBase base = Base10)
{
	// @CleanMe This should be cleaned up...
	switch (base)
	{
		case Base16:
			if ((c & 0xDF) - 'A' < 6) return (c & 0xDF) - ('A' - 10);

		case Base10:
		case Base8:
		case Base2:
			return c - '0';
	}
}

static bool IsWhiteSpace(char c)
{
	return c == '\t' || c == ' '; // @FixMe: Think this needs to be expanded?
}

static bool IsPrintable(char c)
{
	return c >= ' ' && c < 0x7F;
}

static constexpr u32 CountBits32(u32 n) { return __builtin_popcount(n); }
static constexpr u64 CountBits64(u64 n) { return __builtin_popcountll(n); }
static constexpr u64 CountBits(u64 n)   { return CountBits64(n); }

static constexpr u32 CountLeadingZeroes32(u32 n) { return __builtin_clz(n); }
static constexpr u64 CountLeadingZeroes64(u64 n) { return __builtin_clzll(n); }
static constexpr u64 CountLeadingZeroes(u64 n)   { return CountLeadingZeroes64(n); }

static constexpr u32 CountTrailingZeroes32(u32 n) { return __builtin_ctz(n); }
static constexpr u64 CountTrailingZeroes64(u64 n) { return __builtin_ctzll(n); }
static constexpr u64 CountTrailingZeroes(u64 n)   { return CountTrailingZeroes64(n); }

static bool IsPow2(u64 n)
{
	return CountBits(n) == 1; // @Todo @Speed: Measure performance of both.
	// return n != 0 && (n & n-1) == 0;
}

// @Note: NextPow2(2^n) = 2^n
static u64 NextPow2(u8 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n++;
	return n;
}

static u64 NextPow2(u16 n)
{
	n--;
	n |= n >> 1;
	n |= n >> 2;
	n |= n >> 4;
	n |= n >> 8;
	n++;
	return n;
}

static u64 NextPow2(u32 n)
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

static u64 NextPow2(u64 n)
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

// @Note: 2^n -> 2^(n+1)
// @Note: Doesn't work for n = 0
static inline u64 NextPow2_Fast(u64 n)
{
	return 1 << (64 - CountTrailingZeroes(n));
}

[[noreturn]]
void ExitProcess(bool failure);

[[noreturn]]
void Fail();

extern "C" u64 SystemCall(u64 rax, u64 rdi = 0, u64 rsi = 0, u64 rdx = 0, u64 r10 = 0, u64 r8 = 0, u64 r9 = 0);


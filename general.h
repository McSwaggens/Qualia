#ifndef GENERAL_H
#define GENERAL_H

#include "int.h"

#if defined(DEBUG)
	#define DebugBreak() __builtin_debugtrap()
	#define DebugAssert(condition) if (!(condition)) __builtin_debugtrap()
	#define IsDebug() true
#else
	#define IsDebug() false
	#define DebugAssert(condition) { }
	#define DebugBreak() { }
#endif

#define COUNT(a) (sizeof(a)/sizeof((a)[0]))

#define CFUNC extern "C"

#define COLD [[unlikely]]
#define HOT [[likely]]

// static inline bool IsConstEval()  { return __builtin_is_constant_evaluated(); }
static inline void Assume(bool b) { __builtin_assume(b); }

#define AssumeAligned(p, alignment) __builtin_assume_aligned(p, alignment)
#define Unreachable() __builtin_unreachable()
#define AssertUnreachable() { Assert(); Unreachable(); }

#define GetInternalFunctionName() __builtin_FUNCTION()
#define GetInternalFileName()     __builtin_FILE()
#define GetInternalLineNumber()   __builtin_LINE()
#define GetInternalColumnNumber() __builtin_COLUMN()

template<typename T>
static inline void Swap(T& a, T& b) {
	T tmp = a;
	a = b;
	b = tmp;
}

static u64 MaskLowerBits64(u64 n, u64 bits)   { return n & (-1 >> (sizeof(n) * 8 - bits)); }
static u64 MaskUpperBits64(u64 n, u64 bits)   { return n >> (sizeof(n) * 8 - bits); }

static inline bool CheckedAdd(u64 a, u64 b, u64* result)      { return __builtin_uaddll_overflow(a, b, result); }
static inline bool CheckedSubtract(u64 a, u64 b, u64* result) { return __builtin_usubll_overflow(a, b, result); }
static inline bool CheckedMultiply(u64 a, u64 b, u64* result) { return __builtin_umulll_overflow(a, b, result); }

static inline bool CheckedAdd_Signed(s64 a, s64 b, s64* result)      { return __builtin_saddll_overflow(a, b, result); }
static inline bool CheckedSubtract_Signed(s64 a, s64 b, s64* result) { return __builtin_ssubll_overflow(a, b, result); }
static inline bool CheckedMultiply_Signed(s64 a, s64 b, s64* result) { return __builtin_smulll_overflow(a, b, result); }

static inline u64 ToBigEndian64(u64 n) { return __builtin_bswap64(n); }
static inline u32 ToBigEndian32(u32 n) { return __builtin_bswap32(n); }
static inline u16 ToBigEndian16(u16 n) { return __builtin_bswap16(n); }

static inline u64 ToLittleEndian64(u64 n) { return __builtin_bswap64(n); }
static inline u32 ToLittleEndian32(u32 n) { return __builtin_bswap32(n); }
static inline u16 ToLittleEndian16(u16 n) { return __builtin_bswap16(n); }

static inline u64 ReverseBytes64(u64 n) { return __builtin_bswap64(n); }
static inline u32 ReverseBytes32(u32 n) { return __builtin_bswap32(n); }
static inline u16 ReverseBytes16(u16 n) { return __builtin_bswap16(n); }

// @Warning: Clang implements this is with ~20 instructions
static inline u64 ReverseBits64(u64 n) { return __builtin_bitreverse64(n); }
static inline u32 ReverseBits32(u32 n) { return __builtin_bitreverse32(n); }
static inline u16 ReverseBits16(u16 n) { return __builtin_bitreverse16(n); }
static inline u8  ReverseBits8(u8 n)   { return __builtin_bitreverse16(n) >> 8u; }

// popcnt
static inline u64 PopCount64(u64 n) { return __builtin_popcountll(n); }
static inline u32 PopCount32(u32 n) { return __builtin_popcount(n); }
static inline u16 PopCount16(u16 n) { return __builtin_popcount((u32)n); }
static inline u8  PopCount8(u8 n)   { return __builtin_popcount((u32)n); }
static inline u64 PopCount(u64 n) { return __builtin_popcountll(n); }

// lzcnt
// `n == 0` check is here to prevent clang from braking the program. Branch is removed.
static inline u64 Clz64(u64 n) { return n == (u64)0 ? (u64)64 : __builtin_clzll(n); }
static inline u32 Clz32(u32 n) { return n == 0 ? 32 : __builtin_clz(n); }
static inline u32 Clz16(u16 n) { return n == 0 ? 16 : Clz32(n) - (32-16); }
static inline u32 Clz8(u8 n)   { return n == 0 ? 8  : Clz32(n) - (32-8); }

// tzcnt
static inline u64 Ctz64(u64 n) { return n == (u64)0 ? (u64)64 : __builtin_ctzll(n); }
static inline u32 Ctz32(u32 n) { return n == 0 ? 32 : __builtin_ctz(n); }
static inline u32 Ctz16(u16 n) { return n == 0 ? 16 : Ctz32(n); }
static inline u32 Ctz8(u8 n)   { return n == 0 ? 8  : Ctz32(n); }

static inline u64 Boi64(u64 n) { return 64llu-Clz64(n); }
static inline u32 Boi32(u32 n) { return 32-Clz32(n); }
static inline u32 Boi16(u16 n) { return 16-Clz16(n); }
static inline u32 Boi8(u8 n)   { return 8-Clz8(n); }
static inline u64 Boi(u64 n) { return 64llu-Clz64(n); }

static inline bool IsPow2(u64 n) { return PopCount64(n) == 1; }

// @Note: 2^n -> 2^(n+1)
static inline u64 NextPow2(u64 n) { return 1llu << Boi64(n); }

// @Note: NextPow2(2^n) = 2^n
static u64 RaisePow2(u64 n) {
	return IsPow2(n) ? n : NextPow2(n);
}

CFUNC s64 SystemCall(s64 rax, s64 rdi = 0, s64 rsi = 0, s64 rdx = 0, s64 r10 = 0, s64 r8 = 0, s64 r9 = 0);

static inline u64 ReadPerformanceCounter() { return __builtin_readcyclecounter(); } // rdtsc

#endif // GENERAL_H

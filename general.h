#pragma once

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

#define GetInternalFunctionName() ToString(__builtin_FUNCTION())
#define GetInternalFileName()     ToString(__builtin_FILE())
#define GetInternalLineNumber()   __builtin_LINE()
#define GetInternalColumnNumber() __builtin_COLUMN()

static inline auto Abs(auto n) { return n >= 0 ? n : -n; }

template<typename T>
static inline void Swap(T* a, T* b)
{
	T tmp = *a;
	*a = *b;
	*b = tmp;
}

template<typename T>
static inline T Max(T a, T b) { return a >= b ? a : b; }

template<typename T>
static inline T Min(T a, T b) { return a <= b ? a : b; }

template<typename T>
static inline T Clamp(T v, T min, T max) { return v < min ? min : v > max ? max : v; }

static auto Pow(auto base, auto exponent)
{
	return __builtin_powl(base, exponent); // @Bug @FixMe: This isn't correct.
}

static uint64 MaskLowerBits64(uint64 n, uint64 bits)   { return n & (-1 >> (sizeof(n) * 8 - bits)); }
static uint64 MaskUpperBits64(uint64 n, uint64 bits)   { return n >> (sizeof(n) * 8 - bits); }

static inline bool CheckedAdd(uint64 a, uint64 b, uint64* result)      { return __builtin_uaddll_overflow(a, b, result); }
static inline bool CheckedSubtract(uint64 a, uint64 b, uint64* result) { return __builtin_usubll_overflow(a, b, result); }
static inline bool CheckedMultiply(uint64 a, uint64 b, uint64* result) { return __builtin_umulll_overflow(a, b, result); }

static inline bool CheckedAdd_Signed(int64 a, int64 b, int64* result)      { return __builtin_saddll_overflow(a, b, result); }
static inline bool CheckedSubtract_Signed(int64 a, int64 b, int64* result) { return __builtin_ssubll_overflow(a, b, result); }
static inline bool CheckedMultiply_Signed(int64 a, int64 b, int64* result) { return __builtin_smulll_overflow(a, b, result); }

static inline uint64 ToBigEndian64(uint64 n) { return __builtin_bswap64(n); }
static inline uint32 ToBigEndian32(uint32 n) { return __builtin_bswap32(n); }
static inline uint16 ToBigEndian16(uint16 n) { return __builtin_bswap16(n); }

static inline uint64 ToLittleEndian64(uint64 n) { return __builtin_bswap64(n); }
static inline uint32 ToLittleEndian32(uint32 n) { return __builtin_bswap32(n); }
static inline uint16 ToLittleEndian16(uint16 n) { return __builtin_bswap16(n); }

static inline uint64 ReverseBytes64(uint64 n) { return __builtin_bswap64(n); }
static inline uint32 ReverseBytes32(uint32 n) { return __builtin_bswap32(n); }
static inline uint16 ReverseBytes16(uint16 n) { return __builtin_bswap16(n); }

// @Warning: Clang implements this is with ~20 instructions
static inline uint64 ReverseBits64(uint64 n) { return __builtin_bitreverse64(n); }
static inline uint32 ReverseBits32(uint32 n) { return __builtin_bitreverse32(n); }
static inline uint16 ReverseBits16(uint16 n) { return __builtin_bitreverse16(n); }
static inline uint8  ReverseBits8(uint8 n)   { return __builtin_bitreverse16(n) >> 8u; }

// popcnt
static inline uint64 CountBits64(uint64 n) { return __builtin_popcountll(n); }
static inline uint32 CountBits32(uint32 n) { return __builtin_popcount(n); }
static inline uint16 CountBits16(uint16 n) { return __builtin_popcount((uint32)n); }
static inline uint8  CountBits8(uint8 n)   { return __builtin_popcount((uint32)n); }

CFUNC uint64 TzCnt(uint64 rax); // tzcnt 0 = 64
CFUNC uint64 LzCnt(uint64 rax); // lzcnt 0 = 64

// lzcnt
// `n == 0` check is here to prevent clang from braking the program. Branch is removed.
static inline uint64 CountLeadingZeroes64(uint64 n) { return n == (uint64)0 ? (uint64)64 : __builtin_clzll(n); }
static inline uint32 CountLeadingZeroes32(uint32 n) { return n == 0 ? 32 : __builtin_clz(n); }
static inline uint32 CountLeadingZeroes16(uint16 n) { return n == 0 ? 16 : CountLeadingZeroes32(n) - (32-16); }
static inline uint32 CountLeadingZeroes8(uint8 n)   { return n == 0 ? 8  : CountLeadingZeroes32(n) - (32-8); }

// tzcnt
static inline uint64 CountTrailingZeroes64(uint64 n) { return n == (uint64)0 ? (uint64)64 : __builtin_ctzll(n); }
static inline uint32 CountTrailingZeroes32(uint32 n) { return n == 0 ? 32 : __builtin_ctz(n); }
static inline uint32 CountTrailingZeroes16(uint16 n) { return n == 0 ? 16 : CountTrailingZeroes32(n); }
static inline uint32 CountTrailingZeroes8(uint8 n)   { return n == 0 ? 8  : CountTrailingZeroes32(n); }

static inline uint64 BitsOfInformation64(uint64 n) { return 64llu-CountLeadingZeroes64(n); }
static inline uint32 BitsOfInformation32(uint32 n) { return 32-CountLeadingZeroes32(n); }
static inline uint32 BitsOfInformation16(uint16 n) { return 16-CountLeadingZeroes16(n); }
static inline uint32 BitsOfInformation8(uint8 n)   { return 8-CountLeadingZeroes8(n); }

static inline bool IsPow2(uint64 n) { return CountBits64(n) == 1; }

// @Note: 2^n -> 2^(n+1)
static inline uint64 NextPow2(uint64 n) { return 1 << BitsOfInformation64(n); }

// @Note: NextPow2(2^n) = 2^n
static uint64 RaisePow2(uint64 n)
{
	return IsPow2(n) ? n : NextPow2(n);
}

[[noreturn]]
static void ExitProcess(bool success);

extern "C" int64 SystemCall(int64 rax, int64 rdi = 0, int64 rsi = 0, int64 rdx = 0, int64 r10 = 0, int64 r8 = 0, int64 r9 = 0);

static inline uint64 ReadPerformanceCounter() { return __builtin_readcyclecounter(); } // rdtsc


#ifndef MATH_H
#define MATH_H

#include "int.h"

struct Float32 {
	union {
		float32 fp;
		s32 i;
		struct {
			bool sign : 1;
			u32 exponent : 8;
			u32 mantissa : 23;
		};
	};

	constexpr operator float32() { return fp; }
};

struct Float64 {
	union {
		float64 fp;
		s64 i;
		struct {
			bool sign     : 1;
			u64  exponent : 11;
			u64  mantissa : 52;
		};
	};

	constexpr operator float64() { return fp; }
};

static_assert(sizeof(Float32) == 4);
static_assert(sizeof(Float64) == 8);

static inline s8  Min(s8 a,  s8 b)  { return a <= b ? a : b; }
static inline s16 Min(s16 a, s16 b) { return a <= b ? a : b; }
static inline s32 Min(s32 a, s32 b) { return a <= b ? a : b; }
static inline s64 Min(s64 a, s64 b) { return a <= b ? a : b; }

static inline u8  Min(u8 a,  u8 b)  { return a <= b ? a : b; }
static inline u16 Min(u16 a, u16 b) { return a <= b ? a : b; }
static inline u32 Min(u32 a, u32 b) { return a <= b ? a : b; }
static inline u64 Min(u64 a, u64 b) { return a <= b ? a : b; }

static inline float32 Min(float32 a, float32 b) { return a <= b ? a : b; }
static inline float64 Min(float64 a, float64 b) { return a <= b ? a : b; }

static inline s8  Max(s8 a,  s8 b)  { return a <= b ? a : b; }
static inline s16 Max(s16 a, s16 b) { return a <= b ? a : b; }
static inline s32 Max(s32 a, s32 b) { return a <= b ? a : b; }
static inline s64 Max(s64 a, s64 b) { return a <= b ? a : b; }

static inline u8  Max(u8 a,  u8 b)  { return a <= b ? a : b; }
static inline u16 Max(u16 a, u16 b) { return a <= b ? a : b; }
static inline u32 Max(u32 a, u32 b) { return a <= b ? a : b; }
static inline u64 Max(u64 a, u64 b) { return a <= b ? a : b; }

static inline float32 Max(float32 a, float32 b) { return a <= b ? a : b; }
static inline float64 Max(float64 a, float64 b) { return a <= b ? a : b; }

template<typename T>
static inline T Clamp(T v, T min, T max) { return v < min ? min : v > max ? max : v; }

static float64 Pow(float64 base, float64 exponent) { return __builtin_powl(base, exponent); } // @Bug @FixMe: This isn't correct.
static float32 Pow(float32 base, float32 exponent) { return __builtin_powl(base, exponent); } // @Bug @FixMe: This isn't correct.

static u64 Pow(u64 base, u64 exponent) { return __builtin_powl(base, exponent); }
static s64 Pow(s64 base, s64 exponent) { return __builtin_powl(base, exponent); }
static u32 Pow(u32 base, u32 exponent) { return __builtin_powl(base, exponent); }
static s32 Pow(s32 base, s32 exponent) { return __builtin_powl(base, exponent); }

template<typename T>
static inline T Abs(T n) { return n >= 0 ? n : -n; }

static inline u64 CarryAdd64(u64 a, u64 b, u64 carry_in, u64* carry_out) { return __builtin_addcll(a, b, carry_in, carry_out); }
static inline u32 CarryAdd32(u32 a, u32 b, u32 carry_in, u32* carry_out) { return __builtin_addc  (a, b, carry_in, carry_out); }
static inline u16 CarryAdd16(u16 a, u16 b, u16 carry_in, u16* carry_out) { return __builtin_addcs (a, b, carry_in, carry_out); }
static inline u8  CarryAdd8 (u8  a, u8  b, u8  carry_in, u8*  carry_out) { return __builtin_addcb (a, b, carry_in, carry_out); }

static inline u64 CarrySub64(u64 a, u64 b, u64 carry_in, u64* carry_out) { return __builtin_subcll(a, b, carry_in, carry_out); }
static inline u32 CarrySub32(u32 a, u32 b, u32 carry_in, u32* carry_out) { return __builtin_subc  (a, b, carry_in, carry_out); }
static inline u16 CarrySub16(u16 a, u16 b, u16 carry_in, u16* carry_out) { return __builtin_subcs (a, b, carry_in, carry_out); }
static inline u8  CarrySub8 (u8  a, u8  b, u8  carry_in, u8*  carry_out) { return __builtin_subcb (a, b, carry_in, carry_out); }

#endif // MATH_H

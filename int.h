#pragma once

// -------------------------------------------- //

#define null nullptr
typedef decltype(null) Null;
typedef char byte;

static_assert(sizeof(byte) == 1);

// -------------------------------------------- //

typedef signed char      s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;
typedef signed __int128  s128;

static_assert(sizeof(s8 )  == 1);
static_assert(sizeof(s16)  == 2);
static_assert(sizeof(s32)  == 4);
static_assert(sizeof(s64)  == 8);
static_assert(sizeof(s128) == 16);

static const s64 INT8_MAX  = 0x7F;               // 127
static const s64 INT16_MAX = 0x7FFF;             // 32767
static const s64 INT32_MAX = 0x7FFFFFFF;         // 2147483647
static const s64 INT64_MAX = 0x7FFFFFFFFFFFFFFF; // 9223372036854775807

static const s64 INT8_MIN  = 0x80;                 // -128
static const s64 INT16_MIN = 0x8000;               // -32768
static const s64 INT32_MIN = 0x80000000;           // -2147483648
static const s64 INT64_MIN = 0x8000000000000000ll; // -9223372036854775808

static const s64 INT8_MAX_DIGITS  = 3;
static const s64 INT16_MAX_DIGITS = 5;
static const s64 INT32_MAX_DIGITS = 10;
static const s64 INT64_MAX_DIGITS = 19;

// -------------------------------------------- //

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;
typedef unsigned __int128  u128;

typedef unsigned long size_t;

static_assert(sizeof(u8 )  ==  1);
static_assert(sizeof(u16)  ==  2);
static_assert(sizeof(u32)  ==  4);
static_assert(sizeof(u64)  ==  8);
static_assert(sizeof(u128) == 16);

static const u64 UINT8_MIN  = 0;
static const u64 UINT16_MIN = 0;
static const u64 UINT32_MIN = 0;
static const u64 UINT64_MIN = 0;

static const u64 UINT8_MAX  = 0xFF;
static const u64 UINT16_MAX = 0xFFFF;
static const u64 UINT32_MAX = 0xFFFFFFFF;
static const u64 UINT64_MAX = 0xFFFFFFFFFFFFFFFFull;

static const s64 UINT8_MAX_DIGITS  = 3;
static const s64 UINT16_MAX_DIGITS = 5;
static const s64 UINT32_MAX_DIGITS = 10;
static const s64 UINT64_MAX_DIGITS = 20;

// -------------------------------------------- //

typedef __fp16 float16;
typedef float  float32;
typedef double float64;
static_assert(sizeof(float16) == 2);
static_assert(sizeof(float32) == 4);
static_assert(sizeof(float64) == 8);

// @todo: float32 and float64 min, max and epsilon.

// -------------------------------------------- //


#pragma once

// -------------------------------------------- //

#define null nullptr
typedef decltype(null) Null;
typedef char byte;

static_assert(sizeof(byte) == 1);

// -------------------------------------------- //

typedef signed char      int8;
typedef signed short     int16;
typedef signed int       int32;
typedef signed long long int64;

static_assert(sizeof(int8 ) == 1);
static_assert(sizeof(int16) == 2);
static_assert(sizeof(int32) == 4);
static_assert(sizeof(int64) == 8);

static const int64 INT8_MAX  = 0x7F;               // 127
static const int64 INT16_MAX = 0x7FFF;             // 32767
static const int64 INT32_MAX = 0x7FFFFFFF;         // 2147483647
static const int64 INT64_MAX = 0x7FFFFFFFFFFFFFFF; // 9223372036854775807

static const int64 INT8_MIN  = 0x80;                 // -128
static const int64 INT16_MIN = 0x8000;               // -32768
static const int64 INT32_MIN = 0x80000000;           // -2147483648
static const int64 INT64_MIN = 0x8000000000000000ll; // -9223372036854775808

static const int64 INT8_MAX_DIGITS  = 3;
static const int64 INT16_MAX_DIGITS = 5;
static const int64 INT32_MAX_DIGITS = 10;
static const int64 INT64_MAX_DIGITS = 19;

// -------------------------------------------- //

typedef unsigned char      uint8;
typedef unsigned short     uint16;
typedef unsigned int       uint32;
typedef unsigned long long uint64;

static_assert(sizeof(uint8 ) == 1);
static_assert(sizeof(uint16) == 2);
static_assert(sizeof(uint32) == 4);
static_assert(sizeof(uint64) == 8);

static const uint64 UINT8_MIN  = 0;
static const uint64 UINT16_MIN = 0;
static const uint64 UINT32_MIN = 0;
static const uint64 UINT64_MIN = 0;

static const uint64 UINT8_MAX  = 0xFF;
static const uint64 UINT16_MAX = 0xFFFF;
static const uint64 UINT32_MAX = 0xFFFFFFFF;
static const uint64 UINT64_MAX = 0xFFFFFFFFFFFFFFFFull;

static const int64 UINT8_MAX_DIGITS  = 3;
static const int64 UINT16_MAX_DIGITS = 5;
static const int64 UINT32_MAX_DIGITS = 10;
static const int64 UINT64_MAX_DIGITS = 20;

// -------------------------------------------- //

typedef __fp16 float16;
typedef float  float32;
typedef double float64;
static_assert(sizeof(float16) == 2);
static_assert(sizeof(float32) == 4);
static_assert(sizeof(float64) == 8);

// @Todo: float32 and float64 min, max and epsilon.

// -------------------------------------------- //


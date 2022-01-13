#pragma once

// -------------------------------------------- //

#define null nullptr
using Null = decltype(null);

// -------------------------------------------- //

using u8  = unsigned char;
using u16 = unsigned short;
using u32 = unsigned int;
using u64 = unsigned long long;

static_assert(sizeof(u8 ) == 1);
static_assert(sizeof(u16) == 2);
static_assert(sizeof(u32) == 4);
static_assert(sizeof(u64) == 8);

static const u64 U8_MIN  = 0x0;
static const u64 U16_MIN = 0x0;
static const u64 U32_MIN = 0x0;
static const u64 U64_MIN = 0x0;

static const u64 U8_MAX  = 0xFF;
static const u64 U16_MAX = 0xFFFF;
static const u64 U32_MAX = 0xFFFFFFFF;
static const u64 U64_MAX = 0xFFFFFFFFFFFFFFFF;

// -------------------------------------------- //

using s8  = signed char;
using s16 = signed short;
using s32 = signed int;
using s64 = signed long long;

static_assert(sizeof(s8 ) == 1);
static_assert(sizeof(s16) == 2);
static_assert(sizeof(s32) == 4);
static_assert(sizeof(s64) == 8);

static const u64 S8_MAX  = 0x7F;
static const u64 S16_MAX = 0x7FFF;
static const u64 S32_MAX = 0x7FFFFFFF;
static const u64 S64_MAX = 0x7FFFFFFFFFFFFFFF;

static const u64 S8_MIN  = 0x80;
static const u64 S16_MIN = 0x8000;
static const u64 S32_MIN = 0x80000000;
static const u64 S64_MIN = 0x8000000000000000;

// -------------------------------------------- //

using f16 = __fp16;
using f32 = float;
using f64 = double;
static_assert(sizeof(f32) == 4);
static_assert(sizeof(f64) == 8);

// @Todo: f32 and f64 min, max and epsilon.

// -------------------------------------------- //


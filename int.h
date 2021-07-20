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

static constexpr u64 u8_min  = 0x0;
static constexpr u64 u16_min = 0x0;
static constexpr u64 u32_min = 0x0;
static constexpr u64 u64_min = 0x0;

static constexpr u64 u8_max  = 0xFF;
static constexpr u64 u16_max = 0xFFFF;
static constexpr u64 u32_max = 0xFFFFFFFF;
static constexpr u64 u64_max = 0xFFFFFFFFFFFFFFFF;

// -------------------------------------------- //

using s8  = signed char;
using s16 = signed short;
using s32 = signed int;
using s64 = signed long long;

static_assert(sizeof(s8 ) == 1);
static_assert(sizeof(s16) == 2);
static_assert(sizeof(s32) == 4);
static_assert(sizeof(s64) == 8);

static constexpr u64 s8_max  = 0x7F;
static constexpr u64 s16_max = 0x7FFF;
static constexpr u64 s32_max = 0x7FFFFFFF;
static constexpr u64 s64_max = 0x7FFFFFFFFFFFFFFF;

static constexpr u64 s8_min  = 0x80;
static constexpr u64 s16_min = 0x8000;
static constexpr u64 s32_min = 0x80000000;
static constexpr u64 s64_min = 0x8000000000000000;

// -------------------------------------------- //

using f16 = __fp16;
using f32 = float;
using f64 = double;
static_assert(sizeof(f32) == 4);
static_assert(sizeof(f64) == 8);

// @Todo: f32 and f64 min, max and epsilon.

// -------------------------------------------- //


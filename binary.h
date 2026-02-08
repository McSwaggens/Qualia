#pragma once

#include "int.h"
#include "general.h"
#include "alloc.h"
#include "math.h"
#include "stack.h"
#include "assert.h"

namespace Bitwise {
	static u64 SafeLeftShift64(u64 a, u64 b) {
		if (b >= 64) COLD return 0;
		return a << b;
	}

	static u64 Apply64(u64 a, u64 b, u32 index, u32 length) {
		Assert(index + length <= 64);
		u64 mask = SafeLeftShift64(-1llu, index) ^ SafeLeftShift64(-1llu, (index + length));
		return (a & ~mask) | (b & mask);
	}

	static u32 Apply32(u32 a, u32 b, u32 index, u32 length) {
		u64 mask = (-1llu << index) ^ (-1llu << (index + length));
		return (a & ~mask) | (b & mask);
	}

	static u16 Apply16(u16 a, u16 b, u32 index, u32 length) { return Apply32(a, b, index, length); }
	static u8  Apply8 (u8  a, u8  b, u32 index, u32 length) { return Apply32(a, b, index, length); }
}

namespace Integer {
	static u32 Add64(u64* a, u64* b, u64* r, u64 count) {
		u64 carry = 0;

		for (u64 i = 0; i < count; i++) r[i] = CarryAdd64(a[i], b[i], carry, &carry);

		return carry;
	}

	static u32 Add32(u32* a, u32* b, u32* r, u64 count) {
		u64 carry = 0;

		if (count & 1) {
			*(u32*)r = CarryAdd32(*(u32*)a, *(u32*)b, carry, (u32*)&carry);
			a += 1;
			b += 1;
		}

		for (u32 words = count >> 2, i = 0; i < words; i++)
			((u64*)r)[i] = CarryAdd64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static u32 Add16(u16* a, u16* b, u16* r, u64 count) {
		u64 carry = 0;

		if (count & 1) *(u16*)r = CarryAdd16(*(u16*)(a + (count & 0)), *(u16*)(b + (count & 0)), carry, (u16*)&carry);
		if (count & 2) *(u32*)r = CarryAdd32(*(u32*)(a + (count & 1)), *(u32*)(b + (count & 1)), carry, (u32*)&carry);

		a += (count & 3);
		b += (count & 3);
		for (u32 words = count >> 2, i = 0; i < words; i++)
			((u64*)r)[i] = CarryAdd64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static u32 Add8(byte* a, byte* b, byte* r, u64 size) {
		u64 carry = 0;

		if (size & 1) *(u8 *)r = CarryAdd8 (*(u8 *)(a + (size & 0)), *(u8 *)(b + (size & 0)), carry, (u8 *)&carry);
		if (size & 2) *(u16*)r = CarryAdd16(*(u16*)(a + (size & 1)), *(u16*)(b + (size & 1)), carry, (u16*)&carry);
		if (size & 4) *(u32*)r = CarryAdd32(*(u32*)(a + (size & 3)), *(u32*)(b + (size & 3)), carry, (u32*)&carry);

		a += (size & 7);
		b += (size & 7);
		for (u32 words = size >> 3, i = 0; i < words; i++)
			((u64*)r)[i] = CarryAdd64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static u32 Sub64(u64* a, u64* b, u64* r, u64 count) {
		u64 carry = 0;

		for (u64 i = 0; i < count; i++)
			r[i] = CarrySub64(a[i], b[i], carry, &carry);

		return carry;
	}

	static u32 Sub32(u32* a, u32* b, u32* r, u64 count) {
		u64 carry = 0;

		if (count & 1) {
			*(u32*)r = CarrySub32(*(u32*)a, *(u32*)b, carry, (u32*)&carry);
			a += 1;
			b += 1;
		}

		for (u32 words = count >> 2, i = 0; i < words; i++)
			((u64*)r)[i] = CarrySub64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static u32 Sub16(u16* a, u16* b, u16* r, u64 count) {
		u64 carry = 0;

		if (count & 1) *(u16*)r = CarrySub16(*(u16*)(a + (count & 0)), *(u16*)(b + (count & 0)), carry, (u16*)&carry);
		if (count & 2) *(u32*)r = CarrySub32(*(u32*)(a + (count & 1)), *(u32*)(b + (count & 1)), carry, (u32*)&carry);

		a += (count & 3);
		b += (count & 3);
		for (u32 words = count >> 2, i = 0; i < words; i++)
			((u64*)r)[i] = CarrySub64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static u32 Sub8(byte* a, byte* b, byte* r, u64 size) {
		u64 carry = 0;

		if (size & 1) *(u8 *)r = CarrySub8 (*(u8 *)(a + (size & 0)), *(u8 *)(b + (size & 0)), carry, (u8 *)&carry);
		if (size & 2) *(u16*)r = CarrySub16(*(u16*)(a + (size & 1)), *(u16*)(b + (size & 1)), carry, (u16*)&carry);
		if (size & 4) *(u32*)r = CarrySub32(*(u32*)(a + (size & 3)), *(u32*)(b + (size & 3)), carry, (u32*)&carry);

		a += (size & 7);
		b += (size & 7);
		for (u32 words = size >> 3, i = 0; i < words; i++)
			((u64*)r)[i] = CarrySub64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	static void And64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] & b[i]; }
	static void And32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void And16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void And8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	static void Nand64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] & b[i]); }
	static void Nand32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void Nand16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void Nand8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	static void Or64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] | b[i]; }
	static void Or32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void Or16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void Or8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	static void Xor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] ^ b[i]; }
	static void Xor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void Xor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void Xor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	static void Nor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] | b[i]); }
	static void Nor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void Nor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void Nor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	static void Xnor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] ^ b[i]); }
	static void Xnor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	static void Xnor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	static void Xnor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }
	static void Xnor1 (u8*  a, u8*  b, u8*  r, u64 count) {
		Xnor8((u8*)a, b, (u8*)r, count >> 3);
		if (count & 7) {
			u8 mask = (1 << (count & 7)) - 1;
			r[count>>3] = ~(a[count>>3] ^ b[count>>3]) & mask;
		}
	}

	static void Not64(u64* a, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~a[i]; }
	static void Not32(u32* a, u32* r, u64 count) { if (count & 1) *r++ = ~*a++; Not64((u64*)a, (u64*)r, count >> 1); }
	static void Not16(u16* a, u16* r, u64 count) { if (count & 1) *r++ = ~*a++; Not32((u32*)a, (u32*)r, count >> 1); }
	static void Not8 (u8*  a, u8*  r, u64 count) { if (count & 1) *r++ = ~*a++; Not16((u16*)a, (u16*)r, count >> 1); }
	static void Not1 (u8*  a, u8*  r, u64 index, u64 count) {
		a += index >> 3;
		r += index >> 3;

		if (index & 7) *r++ =  *a++ ^ (-1 << (index & 7));
		Not8(a, r, count >> 3);
		if (count & 7) r[index>>3] ^= 255 >> (count & 7);
	}
}

static u64 BitToByteCount(u64 bitcount) { return (bitcount + 7) >> 3; }
static const byte BIG_ARRAY_OF_ZEROES[1024] = { };

struct Binary {
	static constexpr u64 INLINE_BITS = 64;

	u64 bitcount = 0;
	union {
		u64   inlined64 = 0;
		byte* data;
	};

	static Binary LARGE_ZERO;

	static Binary Create(Array<const byte> data);

	template<typename T>
	static Binary Create(T value) {
		return Create(Array<const byte>((const byte*)&value, sizeof(T)));
	}

	constexpr Binary() = default;
	constexpr Binary(u64 n, u64 bitcount) : inlined64(n),  bitcount(bitcount) { Assert(bitcount <= INLINE_BITS); }
	constexpr Binary(byte* data, u64 bitcount) : data(data), bitcount(bitcount) { }
	constexpr Binary(u8  n)  : inlined64(n), bitcount( 8) { }
	constexpr Binary(u16 n)  : inlined64(n), bitcount(16) { }
	constexpr Binary(u32 n)  : inlined64(n), bitcount(32) { }
	constexpr Binary(u64 n)  : inlined64(n), bitcount(64) { }
	constexpr Binary(s8  n)  : inlined64(n), bitcount( 8) { }
	constexpr Binary(s16 n)  : inlined64(n), bitcount(16) { }
	constexpr Binary(s32 n)  : inlined64(n), bitcount(32) { }
	constexpr Binary(s64 n)  : inlined64(n), bitcount(64) { }

	static Binary Create(u64 bitcount) {
		bitcount = RaisePow2(bitcount);
		u64 byte_count = BitToByteCount(bitcount);

		if (bitcount <= INLINE_BITS)
			return Binary(0ull, bitcount);

		if (bitcount <= LARGE_ZERO.bitcount)
			return Binary(LARGE_ZERO.data, bitcount);

		// Gross!
		byte* xxx = (byte*)AllocMemory(byte_count);
		ZeroMemory(xxx, byte_count);
		Binary result = Create(Array<byte>(xxx, byte_count));
		FreeMemory(xxx, byte_count);
		return result;
	}


	bool IsInlined() { return bitcount <= INLINE_BITS; }
	u64  ByteCount() { return BitToByteCount(bitcount); }

	byte* GetData() {
		if (IsInlined())
			return (byte*)&inlined64;

		return data;
	}
};

struct IntegerOperationResult {
	Binary value;
	bool overflow;
	bool underflow;
};

static void InitEvalSystem();

namespace Integer {
	static Binary Add(Binary a, Binary b) {
		u64 result_bitcount = Max(a.bitcount, b.bitcount);
		Binary result = Binary::Create(result_bitcount);

		// Perform addition based on the element size
		if      (result_bitcount <=  8) Integer::Add8(a.GetData(), b.GetData(), result.GetData(), 1);
		else if (result_bitcount <= 16) Integer::Add16((u16*)a.GetData(), (u16*)b.GetData(), (u16*)result.GetData(), 1);
		else if (result_bitcount <= 32) Integer::Add32((u32*)a.GetData(), (u32*)b.GetData(), (u32*)result.GetData(), 1);
		else if (result_bitcount <= 64) Integer::Add64((u64*)a.GetData(), (u64*)b.GetData(), (u64*)result.GetData(), 1);
		else {
			// For larger sizes, work with 64-bit chunks
			u64 word_count = (result_bitcount + 63) / 64;
			Integer::Add64((u64*)a.GetData(), (u64*)b.GetData(), (u64*)result.GetData(), word_count);
		}

		return result;
	}

	static Binary Sub(Binary a, Binary b) {
		u64 result_bitcount = Max(a.bitcount, b.bitcount);
		Binary result = Binary::Create(result_bitcount);

		// Perform subtraction based on the element size
		if      (result_bitcount <=  8) Integer::Sub8(a.GetData(), b.GetData(), result.GetData(), 1);
		else if (result_bitcount <= 16) Integer::Sub16((u16*)a.GetData(), (u16*)b.GetData(), (u16*)result.GetData(), 1);
		else if (result_bitcount <= 32) Integer::Sub32((u32*)a.GetData(), (u32*)b.GetData(), (u32*)result.GetData(), 1);
		else if (result_bitcount <= 64) Integer::Sub64((u64*)a.GetData(), (u64*)b.GetData(), (u64*)result.GetData(), 1);
		else {
			// For larger sizes, work with 64-bit chunks
			u64 word_count = (result_bitcount + 63) / 64;
			Integer::Sub64((u64*)a.GetData(), (u64*)b.GetData(), (u64*)result.GetData(), word_count);
		}

		return result;
	}
}

#ifndef LARGE_VALUE_H
#define LARGE_VALUE_H

#include "int.h"
#include "general.h"
#include "memory.h"
#include "math.h"
#include "stack.h"

namespace Integer {
	u32 Add64(u64* a, u64* b, u64* r, u64 count) {
		u64 carry = 0;

		for (u64 i = 0; i < count; i++) r[i] = CarryAdd64(a[i], b[i], carry, &carry);

		return carry;
	}

	u32 Add32(u32* a, u32* b, u32* r, u64 count) {
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

	u32 Add16(u16* a, u16* b, u16* r, u64 count) {
		u64 carry = 0;

		if (count & 1) *(u16*)r = CarryAdd16(*(u16*)(a + (count & 0)), *(u16*)(b + (count & 0)), carry, (u16*)&carry);
		if (count & 2) *(u32*)r = CarryAdd32(*(u32*)(a + (count & 1)), *(u32*)(b + (count & 1)), carry, (u32*)&carry);

		a += (count & 3);
		b += (count & 3);
		for (u32 words = count >> 2, i = 0; i < words; i++)
			((u64*)r)[i] = CarryAdd64(((u64*)a)[i], ((u64*)b)[i], carry, &carry);

		return carry;
	}

	u32 Add8(byte* a, byte* b, byte* r, u64 size) {
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
	void And64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] & b[i]; }
	void And32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void And16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void And8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ & *b++; And16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Nand64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] & b[i]); }
	void Nand32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void Nand16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void Nand8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ & *b++); Nand16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Or64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] | b[i]; }
	void Or32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void Or16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void Or8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ | *b++; Or16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Xor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = a[i] ^ b[i]; }
	void Xor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void Xor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void Xor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = *a++ ^ *b++; Xor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Nor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] | b[i]); }
	void Nor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void Nor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void Nor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ | *b++); Nor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Xnor64(u64* a, u64* b, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~(a[i] ^ b[i]); }
	void Xnor32(u32* a, u32* b, u32* r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor64((u64*)a, (u64*)b, (u64*)r, count >> 1); }
	void Xnor16(u16* a, u16* b, u16* r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor32((u32*)a, (u32*)b, (u32*)r, count >> 1); }
	void Xnor8 (u8*  a, u8*  b, u8*  r, u64 count) { if (count & 1) *r++ = ~(*a++ ^ *b++); Xnor16((u16*)a, (u16*)b, (u16*)r, count >> 1); }

	void Not64(u64* a, u64* r, u64 count) { for (u64 i = 0; i < count; i++) r[i] = ~a[i]; }
	void Not32(u32* a, u32* r, u64 count) { if (count & 1) *r++ = ~*a++; Not64((u64*)a, (u64*)r, count >> 1); }
	void Not16(u16* a, u16* r, u64 count) { if (count & 1) *r++ = ~*a++; Not32((u32*)a, (u32*)r, count >> 1); }
	void Not8 (u8*  a, u8*  r, u64 count) { if (count & 1) *r++ = ~*a++; Not16((u16*)a, (u16*)r, count >> 1); }
}

struct Binary {
	u64 bitcount;
	union {
		u128 inlined_value;
		byte* data;
	};

	Binary(u8   n) : inlined_value(n), bitcount(8)  { }
	Binary(u16  n) : inlined_value(n), bitcount(16) { }
	Binary(u32  n) : inlined_value(n), bitcount(32) { }
	Binary(u64  n) : inlined_value(n), bitcount(64) { }
	Binary(u128 n) : inlined_value(n), bitcount(128) { }

	Binary(s8   n, u64 bitcount = 8)   : inlined_value(n), bitcount(bitcount) { }
	Binary(s16  n, u64 bitcount = 16)  : inlined_value(n), bitcount(bitcount) { }
	Binary(s32  n, u64 bitcount = 32)  : inlined_value(n), bitcount(bitcount) { }
	Binary(s64  n, u64 bitcount = 64)  : inlined_value(n), bitcount(bitcount) { }
	Binary(s128 n, u64 bitcount = 128) : inlined_value(n), bitcount(bitcount) { }

	Binary(byte* data, u64 bitcount) : data(data), bitcount(bitcount) {
		if (bitcount <= 128) {
			inlined_value = 0;
			CopyMemory((byte*)&inlined_value, data, (bitcount+7)>>3);
		}
	}

	bool IsInlined() { return bitcount <= 64; }

	byte* GetData() {
		if (IsInlined())
			return (byte*)&inlined_value;

		return data;
	}

	Binary IntegerAdd(Binary o) {
		u64 max_out_bitcount = (Max(bitcount, o->bitcount) + 1);
		if (!out) out = Binary(AllocateMemory(max_out_bitcount << 3));

		return out;
	}
};

#endif // LARGE_VALUE_H

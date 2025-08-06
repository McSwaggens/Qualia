#ifndef LARGE_VALUE_H
#define LARGE_VALUE_H

#include "general.h"
#include "math.h"
#include "stack.h"

static u64 Boi(Array<byte> data) {
	u64 n = 0;
	u64* words = (u64*)&data[0];
	for (u64 i = 0; i < (data.length & -64); i++)
}


struct LargeValue {
	u64  bits = 64;

	union {
		byte bytes[];
		u64  words[];
	};

	static u64 GetAlignedSize(u64 bitcount) {
		return (bitcount + 7llu) >> 3llu;
	}

	static LargeValue* Alloc(Stack* stack, u64 bitcount) {
		return (LargeValue*)stack->AllocateMemory(sizeof(LargeValue) + GetAlignedSize(bitcount));
	}

	static LargeValue* CreateLargeValue(Stack* stack, Array<byte> data) {
		return (LargeValue*)stack->AllocateMemory(sizeof(LargeValue) + LargeValue::GetAlignedSize(Boi(data)));
	}

	u64 NumBytes() {
		return bits >> 3;
	}

	u64 NumWords() {
		return bits >> 6;
	}

	LargeValue* IntegerAdd(LargeValue* v) {
		LargeValue* value = Create(Max(bits, v->bits));
		for (u64 i = 0; i < NumWords(); i++)
	}
};

#endif // LARGE_VALUE_H

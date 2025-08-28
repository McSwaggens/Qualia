#ifndef IR_H
#define IR_H

#include "general.h"
#include "stack.h"
#include "math.h"
#include "assert.h"
#include "memory.h"
#include "fixed_buffer.h"

#include "large_value.h"

namespace IR {
	static Stack stack;

	struct ValueData {
	};

	static FixedBuffer<ValueData, 1l << 32> value_buffer;

	struct Value {
		u32 handle;

		constexpr Value(u32 index) : handle(index) { }

		bool IsInline() { return handle < UINT32_MAX; }
		u32 GetInlinedValue() { Assert(IsInline()); return ((s32)handle << 1) >> 1; }

		constexpr operator bool() { return handle != 0; }

		ValueData* operator ->() { return &value_buffer[handle]; }
	};

	static void Init();

	static Value NewValue() { return value_buffer.AllocIndex(); }
	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + n;

		// Check if constant already exists..

		Value value = NewValue();
		return value;
	}
};

static void Write(struct OutputBuffer* buffer, IR::Value value);

#endif // IR_H

#ifndef IR_H
#define IR_H

#include "general.h"
#include "stack.h"
#include "math.h"
#include "assert.h"

#include "large_value.h"

namespace IR {
	static Stack stack;

	struct Value {
		u32 handle;

		bool IsInline() { return handle < UINT32_MAX; }
		u32 GetInlinedValue() { Assert(IsInline()); return ((s32)handle << 1) >> 1; }

		LargeValue* GetLargeValue() { }
	};

	static void Init() {
		stack = CreateStack(1<<30llu);
	}
};

#endif // IR_H

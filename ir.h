#ifndef IR_H
#define IR_H

#include "general.h"
#include "stack.h"
#include "math.h"

namespace IR {
	static Stack stack;

	struct Value {
		u32 handle;
	};

	// struct Value {
	// 	union {
	// 		u64 i;
	// 		Float32 fp32;
	// 		Float64 fp64;
	// 	};
	// };

	static LargeValue* CreateLargeValue(u64 bitcount) {
	}

	static void Init() {
		stack = CreateStack(1<<30llu);
	}
};

#endif // IR_H

#ifndef IR_H
#define IR_H

#include "general.h"
#include "stack.h"
#include "math.h"
#include "assert.h"
#include "memory.h"
#include "fixed_buffer.h"
#include "set.h"

#include "large_value.h"
#include "map.h"

namespace IR {
	static Stack stack;

	struct Handle;
	struct Context;
	struct Relation;
	struct Key;
	struct ValueData;

	enum class RelationKind {
		Equal,
		NotEqual,
		Less,
		LessOrEqual,
		Greater,
		GreaterOrEqual,
		Distance,
	};

	struct ValueData {
	};

	static FixedBuffer<ValueData, 1l << 32> value_buffer;

	struct Value {
		u32 handle = 0;

		constexpr Value() = default;
		constexpr Value(u32 index) : handle(index) { }

		bool IsValid() { return handle != 0; }
		constexpr operator bool() { return handle != 0; }
		ValueData* operator ->() { return &value_buffer[handle]; }
	};

	struct Key {
	};

	struct Context {
	};

	struct Relation {
		Key key;
		Value value;
		Value to;
	};

	static Map<u64, Value> small_constants;

	static void Init();

	static Value NewValue() { return value_buffer.AllocIndex(); }
	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + n;

		auto[inserted, value] = small_constants.GetOrAdd(n);

		if (!inserted) {
			*value = NewValue();
		}

		return *value;
	}
};

static void Write(struct OutputBuffer* buffer, IR::Value value);

#endif // IR_H

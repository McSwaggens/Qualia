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

		UnsignedLess,
		UnsignedLessOrEqual,
		UnsignedGreater,
		UnsignedGreaterOrEqual,

		SignedLess,
		SignedLessOrEqual,
		SignedGreater,
		SignedGreaterOrEqual,

		Distance,
	};

	enum ValueFlag : u16 {
		VALUE_CONSTANT      = 0x01,
		VALUE_LONG_CONSTANT = 0x02,
	};

	struct ValueData {
		u16 flags = 0;
		Set<Relation> relations;
		Binary constant;

		bool IsConstant() { return flags & VALUE_CONSTANT; }
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
		RelationKind kind;
		Value pivot;
		Value to;
	};

	struct Context {
		Set<Key> keys;
	};

	struct Relation {
		RelationKind kind;
		Value value;
		Value to;
		Context context;
	};

	static Map<u64, Value> small_constants;

	static void Init();

	static Value NewValue() { return value_buffer.AllocIndex(); }

	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + n;

		auto[inserted, value] = small_constants.GetOrAdd(n);

		if (!inserted) {
			Value new_value = NewValue();
			new_value->constant = n;
			*value = new_value;
		}

		return *value;
	}

	static Value Constant(Array<byte> data) {
		Assert(data.length);

		if (data.length <= 8) {
			union { u64 n; byte bytes[8]; } u = { 0 };
			CopyMemory(u.bytes, data, data.length);
			return small_constants.TryGet(u.n).Or(Constant(u.n));
		}

		Value new_value = NewValue();
		new_value->flags = VALUE_CONSTANT | VALUE_LONG_CONSTANT;
		new_value->constant = Binary(data);

		return new_value;
	}
};

static void Write(struct OutputBuffer* buffer, IR::Value value);

#endif // IR_H

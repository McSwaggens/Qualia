#pragma once

#include "general.h"
#include "stack.h"
#include "math.h"
#include "assert.h"
#include "alloc.h"
#include "fixed_buffer.h"
#include "set.h"

#include "binary.h"
#include "map.h"

namespace IR {
	static Stack stack;

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

	// A value is the set of relations it has to other values.
	struct Value {
		u32 handle = 0;

		constexpr Value() = default;
		constexpr Value(u32 index) : handle(index) { }

		bool IsValid() { return handle != 0; }
		constexpr operator bool() { return handle != 0; }
		ValueData* operator ->() { return &value_buffer[handle]; }

		bool operator ==(Value o) const { return handle == o.handle; }
		bool operator !=(Value o) const { return handle != o.handle; }
		bool operator <(Value o) const { return handle < o.handle; }
		bool operator >(Value o) const { return handle > o.handle; }
	};

	struct Key {
		RelationKind kind;
		Value pivot;
		Value to;

		bool operator ==(Key o) const { return kind == o.kind && pivot == o.pivot && to == o.to; }
		bool operator !=(Key o) const { return !(*this == o); }
		bool operator <(Key o) const {
			if (kind != o.kind) return kind < o.kind;
			if (pivot != o.pivot) return pivot < o.pivot;
			return to < o.to;
		}
		bool operator >(Key o) const { return o < *this; }
	};

	// Set of things that we know.
	struct Context {
		Context* parent;
		Map<Key, Context*> children;

		Set<Key> keys; // Things we know to be true.

		explicit Context() = default;

		Context(Context* parent, Key key) : parent(parent) {
			keys = parent->keys.Copy();
			Assert(keys.GetBinaryIndex(key) == keys.Count());
			keys.elements.Add(key);
		}

		Context* Get(Key key) {
			auto [inserted, child] = children.GetOrAdd(key);
			if (inserted) {
				Context* new_context = stack.Allocate<Context>();
				*new_context = Context(this, key);
				*child = new_context;
			}
			return *child;
		}

	};

	static Context empty_context = Context();

	static Context* FindContext(Set<Key> keys) {
		Context* c = &empty_context;
		for (auto& key : keys)
			c = c->Get(key);
		return c;
	}


	struct Relation {
		RelationKind kind;
		Value value;
		Value to;
		Context* context;
	};

	static void Init();

	static Value NewValue() { return value_buffer.AddIndex(); }

	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + n;

		Value new_value = NewValue();
		new_value->constant = n;
		return new_value;
	}

	static Value Constant(Array<byte> data) {
		Assert(data.length);

		if (data.length <= 8) {
			union { u64 n; byte bytes[8]; } u = { 0 };
			CopyMemory(u.bytes, data, data.length);
			return Constant(u.n);
		}

		Value new_value = NewValue();
		new_value->flags = VALUE_CONSTANT | VALUE_LONG_CONSTANT;
		new_value->constant = Binary(data);

		return new_value;
	}

	template <typename T>
	static Value Constant(T value) {
		return Constant(Array<byte>((byte*)&value, sizeof(T)));
	}
};

static void Write(struct OutputBuffer* buffer, IR::Value value);

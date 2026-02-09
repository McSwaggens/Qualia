#pragma once

// IR Architecture: Two-Graph System
//
// Relation Graph (stateless):
//   - Forward-chaining inference engine for value constraints
//   - Adding one relation triggers cascading inferences throughout the graph
//   - Example: a < b, b < c => infer a < c
//   - Example: Distance(a,b,4), Distance(b,c,8) => infer Distance(a,c,12)
//   - Design: fewer relation types with richer inference rules
//
// State Graph (stateful):
//   - Loops, stack allocations, memory operation ordering
//   - Load/store ordering, memory aliasing, control flow
//   - References stateless values from relation graph

#include "general.h"
#include "stack.h"
#include "math.h"
#include "assert.h"
#include "alloc.h"
#include "fixed_buffer.h"
#include "set.h"

#include "binary.h"
#include "map.h"

namespace Ast { struct Expression; };

namespace IR {
	static Stack stack;

	struct Context;
	struct Relation;
	struct ValueData;

	enum class RelationKind {
		Equal,
		NotEqual,

		Less,
		LessOrEqual,
		Greater,
		GreaterOrEqual,

		Distance,
		Remainder,
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

		ValueData& Get() const { return value_buffer[handle]; }

		bool IsValid() { return handle != 0; }
		constexpr operator bool() { return handle != 0; }
		ValueData* operator ->() { return &Get(); }

		bool operator ==(Value o) const { return handle == o.handle; }
		bool operator !=(Value o) const { return handle != o.handle; }
		bool operator  <(Value o) const { return handle  < o.handle; }
		bool operator  >(Value o) const { return handle  > o.handle; }
	};

	struct Relation {
		RelationKind kind;
		Value to;            // Column
		Value value;         // Cell
		Context* context;    // Conditions required for this to be true.

		bool operator ==(Relation o) const {
			return kind == o.kind && to == o.to && value == o.value && context == o.context;
		}
		bool operator !=(Relation o) const { return !(*this == o); }
		bool operator <(Relation o) const {
			if (kind    != o.kind)    return kind    < o.kind;
			if (context != o.context) return context < o.context;
			if (to      != o.to)      return to      < o.to;
			return value < o.value;
		}
		bool operator >(Relation o) const { return o < *this; }
	};

	// Set of things that we know.
	struct Context {
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

		Context* parent = null;
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

		bool CanSee(Relation relation) {
			if (this > relation.context)
				return false;

			Context* ctx = relation.context;
			while (ctx && ctx != this)
				ctx = ctx->parent;

			return ctx != null;
		}

		void Equal(Value a, Value b) {
			if (a == b)
				return; // Prevent self-loops

			if (!a->relations.Add({ .context = this, .kind = RelationKind::Equal, .to = b, }))
				return; // Already exists!

			Equal(b, a); // Symmetry

			// if a == b && a == c then b == c
			// if a == b && a != c then b != c
			u32 count = a->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (r.kind == RelationKind::Equal    && CanSee(r))    Equal(b, r.to);
				if (r.kind == RelationKind::NotEqual && CanSee(r)) NotEqual(b, r.to);
			}
		}

		void NotEqual(Value a, Value b) {
			if (a == b)
				return; // Can't be not-equal to itself

			if (!a->relations.Add({ .context = this, .kind = RelationKind::NotEqual, .to = b, }))
				return; // Already exists

			NotEqual(b, a); // Symmetry

			// if a != b && a == x then x != b
			u32 count = a->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (r.kind == RelationKind::Equal && CanSee(r)) NotEqual(r.to, b);
			}
		}

		void Less(Value a, Value b) {
			if (a == b)
				return; // Can't be less than itself

			if (!a->relations.Add({ .context = this, .kind = RelationKind::Less, .to = b, }))
				return; // Already exists

			// if a < b then a != b
			NotEqual(a, b);

			// if a < b && b < c then a < c
			// if a < b && b <= c then a < c
			// if a < b && b == y then a < y
			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Less)         Less(a, r.to);
				else if (r.kind == RelationKind::LessOrEqual)  Less(a, r.to);
				else if (r.kind == RelationKind::Equal)        Less(a, r.to);
			}

			// if x == a && a < b then x < b
			// if c < a && a < b then c < b
			// if c <= a && a < b then c < b
			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Equal)           Less(r.to, b);
				else if (r.kind == RelationKind::Greater)         Less(r.to, b);
				else if (r.kind == RelationKind::GreaterOrEqual)  Less(r.to, b);
			}
		}

		void LessOrEqual(Value a, Value b) {
			if (a == b)
				return; // a <= a is trivially true, skip

			if (!a->relations.Add({ .context = this, .kind = RelationKind::LessOrEqual, .to = b, }))
				return; // Already exists

			// if a <= b && b < c then a < c
			// if a <= b && b <= c then a <= c
			// if a <= b && b == y then a <= y
			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Less)         Less(a, r.to);
				else if (r.kind == RelationKind::LessOrEqual)  LessOrEqual(a, r.to);
				else if (r.kind == RelationKind::Equal)        LessOrEqual(a, r.to);
			}

			// if x == a && a <= b then x <= b
			// if c < a && a <= b then c < b
			// if c <= a && a <= b then c <= b
			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Equal)           LessOrEqual(r.to, b);
				else if (r.kind == RelationKind::Greater)         Less(r.to, b);
				else if (r.kind == RelationKind::GreaterOrEqual)  LessOrEqual(r.to, b);
			}
		}

		void Greater(Value a, Value b) {
			if (a == b)
				return; // Can't be greater than itself

			if (!a->relations.Add({ .context = this, .kind = RelationKind::Greater, .to = b, }))
				return; // Already exists

			// if a > b then a != b
			NotEqual(a, b);

			// if a > b && b > c then a > c
			// if a > b && b >= c then a > c
			// if a > b && b == y then a > y
			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Greater)         Greater(a, r.to);
				else if (r.kind == RelationKind::GreaterOrEqual)  Greater(a, r.to);
				else if (r.kind == RelationKind::Equal)           Greater(a, r.to);
			}

			// if x == a && a > b then x > b
			// if c > a && a > b then c > b
			// if c >= a && a > b then c > b
			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == RelationKind::Equal)        Greater(r.to, b);
				else if (r.kind == RelationKind::Less)         Greater(r.to, b);
				else if (r.kind == RelationKind::LessOrEqual)  Greater(r.to, b);
			}
		}

		void GreaterOrEqual(Value a, Value b) {
			a->relations.Add({ .context = this, .kind = RelationKind::GreaterOrEqual, .to = b, });
		}

		void Distance(Value a, Value b, Value distance) {
			a->relations.Add({ .context = this, .kind = RelationKind::Distance, .to = b, .value = distance, });
		}

		void Remainder(Value a, Value b, Value remainder) {
			a->relations.Add({ .context = this, .kind = RelationKind::Remainder, .to = b, .value = remainder, });
		}
	};

	static Context empty_context = Context();

	static Context* FindContext(Set<Context::Key> keys) {
		Context* c = &empty_context;
		for (auto& key : keys)
			c = c->Get(key);
		return c;
	}

	static void Init();

	static Value NewValue() {
		Value v = value_buffer.AddIndex();
		v.Get() = (ValueData){ };
		return v;
	}

	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + n;

		Value new_value = NewValue();
		new_value->constant = n;
		return new_value;
	}

	static Value Constant(s64 n) {
		if (n >= 0 && n < 256)
			return 1 + n;

		Value new_value = NewValue();
		new_value->constant = n;
		return new_value;
	}

	static Value Constant(u8 n)  { return Constant((u64)n); }
	static Value Constant(u16 n) { return Constant((u64)n); }
	static Value Constant(u32 n) { return Constant((u64)n); }

	static Value Constant(s8 n)  { return Constant((s64)n); }
	static Value Constant(s16 n) { return Constant((s64)n); }
	static Value Constant(s32 n) { return Constant((s64)n); }

	static Value Constant(Array<byte> data) {
		if (data.length == 0) {
			Value new_value = NewValue();
			new_value->flags = VALUE_CONSTANT;
			new_value->constant = Binary((u64)0, 0);
			return new_value;
		}

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

	struct Load {
		IR::Value address;
	};

	struct Store {
		IR::Value address;
		IR::Value value;
	};

	struct Branch {
		IR::Value condition; // true or false
		struct State* left;
		struct State* right;
	};

	struct State {
		Context* context;
		List<Load>  loads;
		List<Store> stores;
		Branch branch;
	};

	// State object, used to traverse Blocks.
	struct State {
		Context* prev_state_common_ancestor;
		Block* block;
		List<Pair<List<Context::Key>, Block*>> parents;

		// Going to a previous state means dropping Keys and Adding new keys.
		State GotoPrevState()
	};

	static void PrintState();
};

static void Write(struct OutputBuffer* buffer, IR::Value value);
static void Write(struct OutputBuffer* buffer, IR::Relation relation);
static void Write(struct OutputBuffer* buffer, Array<IR::Relation> relations);
static void Write(struct OutputBuffer* buffer, IR::RelationKind kind);
static void Write(struct OutputBuffer* buffer, IR::Context context);
static void Write(struct OutputBuffer* buffer, IR::ValueFlag);

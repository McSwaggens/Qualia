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
//
// Trigger-Based Semantic Analysis:
//   - Types are IR::Values (not properties of values)
//   - Triggers track which contexts each dependency value is known in
//   - When all dependencies are known in a common context, trigger fires
//   - Example for `a + b`:
//       Stage 1: [ a_type = { }, b_type = { } ]
//       Stage 2: a_type known in c0 -> [ a_type = {c0}, b_type = { } ]
//       Stage 3: b_type known in c1 -> [ a_type = {c0}, b_type = {c1} ]
//       Stage 4: b_type known in c0 -> [ a_type = {c0}, b_type = {c0,c1} ]
//                TRIGGER in c0! (both known in c0)
//
//   - Usage:
//       // 1. Define Trigger method on AST node
//       struct Expression {
//           void Trigger(Context* ctx, Trigger* t) { /* type-check */ }
//       };
//
//       // 2. Create trigger at parse time
//       Expression* expr = ...;
//       auto callback = (void(*)(void*, Context*, Trigger*))&Expression::Trigger;
//       CreateTrigger({type_of_a, type_of_b}, callback, expr);
//
//       // 3. Notify triggers when values become known
//       ctx->Equal(type_of_a, int_type);
//       NotifyValueKnown(type_of_a, ctx);  // Fires triggers if all deps ready

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
	struct Trigger;

	enum ValueFlag : u16 {
		VALUE_CONSTANT      = 0x01,
		VALUE_LONG_CONSTANT = 0x02,
	};

	struct ValueData {
		u16 flags = 0;
		Set<Relation> relations;
		Binary constant;
		List<Trigger*> waiting;  // Triggers waiting on this value

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
		enum Kind {
			// NotEqual(a, b): a != b
			// Use for contradiction checks and strict-order implications.
			NotEqual,

			// Less(a, b): a < b
			// Strict ordering relation.
			Less,

			// LessOrEqual(a, b): a <= b
			// Non-strict ordering relation.
			LessOrEqual,

			// Greater(a, b): a > b
			// Strict ordering relation.
			Greater,

			// GreaterOrEqual(a, b): a >= b
			// Non-strict ordering relation.
			GreaterOrEqual,

			// Distance(a, b, c): b - a = c
			// Encodes: add, subtract, equality (b - a = 0), negative (0 - a = -a)
			Distance,

			// Remainder(a, b, c): a % b = c
			// Useful for divisibility, alignment and stride constraints.
			Remainder,
		};

		Kind kind;
		Value to;            // Column
		Value value;         // Cell
		Context* context;    // Conditions required for this to be true.

		Relation() = delete;

		constexpr Relation(Kind kind, Value to, Context* context, Value value = { })
			: kind(kind), to(to), value(value), context(context) {
			Assert(context);
		}

		static Relation CreateNotEqual(Value from, Value to, Context* context) {
			Assert(from);
			Assert(to);
			return Relation(NotEqual, to, context);
		}

		static Relation CreateLess(Value from, Value to, Context* context) {
			Assert(from);
			Assert(to);
			return Relation(Less, to, context);
		}

		static Relation CreateLessOrEqual(Value from, Value to, Context* context) {
			Assert(from);
			Assert(to);
			return Relation(LessOrEqual, to, context);
		}

		static Relation CreateGreater(Value from, Value to, Context* context) {
			Assert(from);
			Assert(to);
			return Relation(Greater, to, context);
		}

		static Relation CreateGreaterOrEqual(Value from, Value to, Context* context) {
			Assert(from);
			Assert(to);
			return Relation(GreaterOrEqual, to, context);
		}

		static Relation CreateDistance(Value from, Value to, Value distance, Context* context) {
			Assert(from);
			Assert(to);
			Assert(distance);
			return Relation(Distance, to, context, distance);
		}

		static Relation CreateRemainder(Value from, Value to, Value remainder, Context* context) {
			Assert(from);
			Assert(to);
			Assert(remainder);
			return Relation(Remainder, to, context, remainder);
		}

		bool operator ==(Relation o) const {
			return kind == o.kind && to == o.to && value == o.value && context == o.context;
		}
		bool operator !=(Relation o) const { return !(*this == o); }
		bool operator <(Relation o) const {
			if (to      != o.to)      return to      < o.to;
			if (kind    != o.kind)    return kind    < o.kind;
			if (context != o.context) return context < o.context;
			return value < o.value;
		}
		bool operator >(Relation o) const { return o < *this; }
	};

	static void Init();

	static Value NewValue() {
		Value v = value_buffer.AddIndex();
		v.Get() = (ValueData){ };
		return v;
	}

	static Value Constant(u64 n) {
		if (n < 256)
			return 1 + (u32)n;

		Value new_value = NewValue();
		new_value->flags = VALUE_CONSTANT;
		new_value->constant = n;
		return new_value;
	}

	static Value Constant(s64 n) {
		return Constant((u64)n);
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

	// Set of things that we know.
	struct Context {
		struct Key {
			Relation::Kind kind;
			Value from;
			Value to;
			Value value;

			Key() = delete;

			constexpr Key(Relation::Kind kind, Value from, Value to, Value value = { })
				: kind(kind), from(from), to(to), value(value) {
				Assert(from);
				Assert(to);
			}

			bool operator ==(Key o) const {
				return kind == o.kind && from == o.from && to == o.to && value == o.value;
			}
			bool operator !=(Key o) const { return !(*this == o); }
			bool operator <(Key o) const {
				if (kind != o.kind) return kind < o.kind;
				if (from != o.from) return from < o.from;
				if (to != o.to) return to < o.to;
				return value < o.value;
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
				Assert(new_context > this);
				*new_context = Context(this, key);
				*child = new_context;
			}
			return *child;
		}

		Context* Intersect(Context* other);

		bool CanSee(Relation relation) {
			// A relation is visible if all context keys required to establish it
			// are present in the current context.
			Assert(relation.context);

			if (this == relation.context)
				return true;

			// Root/empty-context relations have no prerequisites.
			if (relation.context->keys.IsEmpty())
				return true;

			return keys.Contains(relation.context->keys);
		}

		struct PairFacts {
			bool eq = false;
			bool ne = false;
			bool lt = false;
			bool le = false;
			bool gt = false;
			bool ge = false;
		};

		struct RelationRange {
			u32 begin = 0;
			u32 end = 0;
		};

		RelationRange GetRelationRangeTo(Value from, Value to) {
			u32 count = from->relations.Count();
			auto& relations = from->relations.elements;

			u32 lo = 0;
			u32 hi = count;
			while (lo < hi) {
				u32 mid = lo + ((hi - lo) >> 1);
				if (relations[mid].to < to) lo = mid + 1;
				else                        hi = mid;
			}
			u32 begin = lo;

			hi = count;
			while (lo < hi) {
				u32 mid = lo + ((hi - lo) >> 1);
				if (relations[mid].to > to) hi = mid;
				else                        lo = mid + 1;
			}

			return { .begin = begin, .end = lo };
		}

		PairFacts CollectPairFacts(Value a, Value b) {
			PairFacts facts = { };

			if (a == b) {
				facts.eq = true;
				facts.le = true;
				facts.ge = true;
			}

			Value zero = Constant(0);
			RelationRange range = GetRelationRangeTo(a, b);
			for (u32 i = range.begin; i < range.end; i++) {
				Relation& r = a->relations.elements[i];
				if (!CanSee(r)) continue;

				if      (r.kind == Relation::Kind::NotEqual)                              facts.ne = true;
				else if (r.kind == Relation::Kind::Less)                                  facts.lt = true;
				else if (r.kind == Relation::Kind::LessOrEqual)                           facts.le = true;
				else if (r.kind == Relation::Kind::Greater)                               facts.gt = true;
				else if (r.kind == Relation::Kind::GreaterOrEqual)                        facts.ge = true;
				else if (r.kind == Relation::Kind::Distance && r.value == zero)           facts.eq = true;
			}

			range = GetRelationRangeTo(b, a);
			for (u32 i = range.begin; i < range.end; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;

				if      (r.kind == Relation::Kind::NotEqual)                              facts.ne = true;
				else if (r.kind == Relation::Kind::Less)                                  facts.gt = true;
				else if (r.kind == Relation::Kind::LessOrEqual)                           facts.ge = true;
				else if (r.kind == Relation::Kind::Greater)                               facts.lt = true;
				else if (r.kind == Relation::Kind::GreaterOrEqual)                        facts.le = true;
				else if (r.kind == Relation::Kind::Distance && r.value == zero)           facts.eq = true;
			}

			Assert(!(facts.lt && facts.gt)); // strict ordering: a < b and a > b are mutually exclusive
			Assert(!facts.eq || !facts.ne);  // equality: a == b and a != b are mutually exclusive

			if (facts.eq) {
				facts.le = true;
				facts.ge = true;
			}
			if (facts.lt) {
				facts.le = true;  // a < b implies a <= b
				facts.ne = true;
			}
			if (facts.gt) {
				facts.ge = true;  // a > b implies a >= b
				facts.ne = true;
			}

			return facts;
		}

		Optional<bool> IsEqual(Value a, Value b) {
			PairFacts facts = CollectPairFacts(a, b);
			if (facts.eq)
				return true;

			if (facts.ne || facts.lt || facts.gt)
				return false;

			return OptNone;
		}

		Optional<bool> IsNotEqual(Value a, Value b) {
			PairFacts facts = CollectPairFacts(a, b);
			if (facts.ne || facts.lt || facts.gt)
				return true;

			if (facts.eq)
				return false;

			return OptNone;
		}

		Optional<bool> IsLess(Value a, Value b) {
			PairFacts facts = CollectPairFacts(a, b);
			if (facts.lt)
				return true;

			if (facts.gt || facts.ge || facts.eq)
				return false;

			if (facts.le && facts.ne)
				return true;

			return OptNone;
		}

		Optional<bool> IsLessOrEqual(Value a, Value b) {
			PairFacts facts = CollectPairFacts(a, b);
			if (facts.lt || facts.le || facts.eq)
				return true;

			if (facts.gt)
				return false;

			if (facts.ge) {
				if (facts.ne)
					return false;
				if (facts.eq)
					return true;
			}

			return OptNone;
		}

		Optional<bool> IsGreater(Value a, Value b) {
			return IsLess(b, a);
		}

		Optional<bool> IsGreaterOrEqual(Value a, Value b) {
			return IsLessOrEqual(b, a);
		}

		Value Negative(Value value) {
			Value zero = Constant(0);
			if (value == zero)
				return value;

			// Primary form: if Distance(value, 0, n) then -value = n
			RelationRange range = GetRelationRangeTo(value, zero);
			Value result = { };
			for (u32 i = range.begin; i < range.end; i++) {
				Relation& r = value->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind != Relation::Kind::Distance) continue;

				if (!result) result = r.value;
				else Assert(result == r.value);
			}
			if (result)
				return result;

			// Involution witness: if Distance(x, 0, value) then -value = x
			for (u32 handle = 1; handle < value_buffer.head; handle++) {
				Value candidate = handle;
				range = GetRelationRangeTo(candidate, zero);

				for (u32 i = range.begin; i < range.end; i++) {
					Relation& r = candidate->relations.elements[i];
					if (!CanSee(r)) continue;
					if (r.kind != Relation::Kind::Distance || r.value != value) continue;

					if (!result) result = candidate;
					else Assert(result == candidate);
				}
			}
			if (result)
				return result;

			// No known negation yet, create symbolic one.
			Value negative = NewValue();
			Distance(value, zero, negative);
			return negative;
		}

		void Equal(Value a, Value b) {
			// Equal(a, b) is just Distance(a, b, 0)
			Distance(a, b, Constant(0));
		}

		void NotEqual(Value a, Value b) {
			if (a == b)
				return; // Can't be not-equal to itself

			if (!a->relations.Add(Relation::CreateNotEqual(a, b, this)))
				return; // Already exists

			NotEqual(b, a); // Symmetry

			// if a != b && Distance(a, x, 0) then x != b
			u32 count = a->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0) && CanSee(r)) NotEqual(r.to, b);
			}

			// if a != b && Distance(a, b, d) then d != 0
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to != b) continue;
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance) {
					NotEqual(r.value, Constant(0));
				}
			}
		}

		void Less(Value a, Value b) {
			if (a == b)
				return; // Can't be less than itself

			if (!a->relations.Add(Relation::CreateLess(a, b, this)))
				return; // Already exists

			Greater(b, a); // Strict-order duality: a < b <=> b > a

			// if a < b then a != b
			NotEqual(a, b);

			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Less)                               Less(a, r.to); // if a < b && b < c then a < c
				else if (r.kind == Relation::Kind::LessOrEqual)                        Less(a, r.to); // if a < b && b <= c then a < c
				else if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Less(a, r.to); // if a < b && Distance(b, y, 0) then a < y
			}

			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Less(r.to, b); // if Distance(x, a, 0) && a < b then x < b
				else if (r.kind == Relation::Kind::Greater)                             Less(r.to, b); // if c < a && a < b then c < b
				else if (r.kind == Relation::Kind::GreaterOrEqual)                      Less(r.to, b); // if c <= a && a < b then c < b
			}
		}

		void LessOrEqual(Value a, Value b) {
			if (a == b)
				return; // a <= a is trivially true, skip

			if (!a->relations.Add(Relation::CreateLessOrEqual(a, b, this)))
				return; // Already exists

			GreaterOrEqual(b, a); // Non-strict duality: a <= b <=> b >= a

			// Antisymmetry: if a <= b && b <= a then a == b
			RelationRange range_ba = GetRelationRangeTo(b, a);
			for (u32 i = range_ba.begin; i < range_ba.end; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::LessOrEqual) {
					Equal(a, b); // a <= b && b <= a implies a == b
					break;
				}
			}

			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Less)                               Less(a, r.to); // if a <= b && b < c then a < c
				else if (r.kind == Relation::Kind::LessOrEqual)                        LessOrEqual(a, r.to); // if a <= b && b <= c then a <= c
				else if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) LessOrEqual(a, r.to); // if a <= b && Distance(b, y, 0) then a <= y
			}

			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Distance && r.value == Constant(0)) LessOrEqual(r.to, b); // if Distance(x, a, 0) && a <= b then x <= b
				else if (r.kind == Relation::Kind::Greater)                             Less(r.to, b); // if c < a && a <= b then c < b
				else if (r.kind == Relation::Kind::GreaterOrEqual)                      LessOrEqual(r.to, b); // if c <= a && a <= b then c <= b
			}
		}

		void Greater(Value a, Value b) {
			if (a == b)
				return; // Can't be greater than itself

			if (!a->relations.Add(Relation::CreateGreater(a, b, this)))
				return; // Already exists

			Less(b, a); // Strict-order duality: a > b <=> b < a

			// if a > b then a != b
			NotEqual(a, b);

			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Greater)                            Greater(a, r.to); // if a > b && b > c then a > c
				else if (r.kind == Relation::Kind::GreaterOrEqual)                     Greater(a, r.to); // if a > b && b >= c then a > c
				else if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Greater(a, r.to); // if a > b && Distance(b, y, 0) then a > y
			}

			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Greater(r.to, b); // if Distance(x, a, 0) && a > b then x > b
				else if (r.kind == Relation::Kind::Less)                               Greater(r.to, b); // if c > a && a > b then c > b
				else if (r.kind == Relation::Kind::LessOrEqual)                        Greater(r.to, b); // if c >= a && a > b then c > b
			}
		}

		void GreaterOrEqual(Value a, Value b) {
			if (a == b)
				return; // a >= a is trivially true, skip

			if (!a->relations.Add(Relation::CreateGreaterOrEqual(a, b, this)))
				return; // Already exists

			LessOrEqual(b, a); // Non-strict duality: a >= b <=> b <= a

			// Antisymmetry: if a >= b && b >= a then a == b
			RelationRange range_ba = GetRelationRangeTo(b, a);
			for (u32 i = range_ba.begin; i < range_ba.end; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::GreaterOrEqual) {
					Equal(a, b); // a >= b && b >= a implies a == b
					break;
				}
			}

			u32 count_b = b->relations.Count();
			for (u32 i = 0; i < count_b; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Greater)                            Greater(a, r.to); // if a >= b && b > c then a > c
				else if (r.kind == Relation::Kind::GreaterOrEqual)                     GreaterOrEqual(a, r.to); // if a >= b && b >= c then a >= c
				else if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) GreaterOrEqual(a, r.to); // if a >= b && Distance(b, y, 0) then a >= y
			}

			u32 count_a = a->relations.Count();
			for (u32 i = 0; i < count_a; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if      (r.kind == Relation::Kind::Distance && r.value == Constant(0)) GreaterOrEqual(r.to, b); // if Distance(x, a, 0) && a >= b then x >= b
				else if (r.kind == Relation::Kind::Less)                               Greater(r.to, b); // if a < x && a >= b then x > b
				else if (r.kind == Relation::Kind::LessOrEqual)                        GreaterOrEqual(r.to, b); // if a <= x && a >= b then x >= b
			}
		}

		void Distance(Value a, Value b, Value distance) {
			// Distance(a, b, d) means: b - a = d, or b = a + d

			if (a == b)
				return; // Distance from a to itself is trivial

			if (!a->relations.Add(Relation::CreateDistance(a, b, distance, this)))
				return; // Already exists

			u32 count = a->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (r.to == b) continue;
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Distance(r.to, b, distance); // if Distance(a, x, 0) then Distance(x, b, distance)
			}

			count = b->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Distance(a, r.to, distance); // if Distance(b, y, 0) then Distance(a, y, distance)
			}

			count = distance->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = distance->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Distance(a, b, r.to); // if Distance(distance, z, 0) then Distance(a, b, z)
			}

			// Transitivity: if Distance(a, b, d1) && Distance(b, c, d2) then Distance(a, c, d1+d2)
			count = b->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance) {
					// Have: Distance(a, b, distance) and Distance(b, r.to, r.value)
					// Want: Distance(a, r.to, distance + r.value)
					// TODO: Need arithmetic Add(distance, r.value) -> sum
				}
			}

			// Symmetry: if Distance(a, b, d) then Distance(b, a, -d)
			Distance(b, a, Negative(distance));
		}

		void Remainder(Value a, Value b, Value remainder) {
			// Remainder(a, b, r) means: a % b = r

			if (!a->relations.Add(Relation::CreateRemainder(a, b, remainder, this)))
				return; // Already exists

			u32 count = a->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = a->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Remainder(r.to, b, remainder); // if Distance(a, x, 0) then Remainder(x, b, remainder)
			}

			count = b->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = b->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Remainder(a, r.to, remainder); // if Distance(b, y, 0) then Remainder(a, y, remainder)
			}

			count = remainder->relations.Count();
			for (u32 i = 0; i < count; i++) {
				Relation& r = remainder->relations.elements[i];
				if (!CanSee(r)) continue;
				if (r.kind == Relation::Kind::Distance && r.value == Constant(0)) Remainder(a, b, r.to); // if Distance(remainder, z, 0) then Remainder(a, b, z)
			}

			// Note: Additional inferences like "if remainder == 0 then b divides a"
			// can be added later for alignment/stride analysis
		}
	};

	// Trigger: fires when all dependencies are satisfied in a common context
	struct Trigger {
		Map<Context*, u16> ready_count;     // How many dependencies satisfied per context
		u16 dependency_count;               // Total number of dependencies
		void (*callback)(void*, Context*, Trigger*);  // Member function: (this, context, trigger)
		void* ast_node = null;              // Expression*, Statement*, Function*, etc.

		Trigger(Array<Value> deps, void (*cb)(void*, Context*, Trigger*), void* node = null) {
			dependency_count = deps.length;
			callback = cb;
			ast_node = node;

			// Register this trigger with each dependency value
			for (Value v : deps) {
				v->waiting.Add(this);
			}
		}

		// Called when a value becomes known in a context
		void ValueKnownInContext(Value v, Context* ctx) {
			auto [inserted, count_ptr] = ready_count.GetOrAdd(ctx);
			if (inserted) *count_ptr = 0;

			(*count_ptr)++;

			// All dependencies satisfied in this context?
			if (*count_ptr == dependency_count) {
				callback(ast_node, ctx, this);  // Calls member function with this as first arg
			}
		}
	};

	// Notify all triggers waiting on a value that it's now known in a context
	// Call this after adding an Equal relation to notify type inference triggers
	static void NotifyValueKnown(Value v, Context* ctx) {
		for (Trigger* t : v->waiting) {
			t->ValueKnownInContext(v, ctx);
		}
	}

	static Context empty_context = Context();

	static Context* FindContext(Set<Context::Key> keys) {
		Context* c = &empty_context;
		for (auto& key : keys)
			c = c->Get(key);
		return c;
	}

	inline Context* Context::Intersect(Context* other) {
		Assert(other);

		Context* c = &empty_context;
		u32 i = 0;
		u32 j = 0;

		// Inline version of Set::Intersect. Avoids allocation.
		while (i < keys.Count() && j < other->keys.Count()) {
			Key& a = keys.elements[i];
			Key& b = other->keys.elements[j];
			if (a == b) {
				c = c->Get(a);
				i++;
				j++;
			}
			else if (a < b) {
				i++;
			}
			else {
				j++;
			}
		}

		return c;
	}

	// Helper to create a trigger (allocates from stack)
	static Trigger* CreateTrigger(Array<Value> deps, void (*callback)(void*, Context*, Trigger*), void* node = null) {
		Trigger* t = stack.New<Trigger>(deps, callback, node);
		return t;
	}

	struct Load {
		Value address;
		Value size;    // Bytes loaded
		Value value;   // Value loaded // Maybe store bits of information (BOI) as a Value inside Value?
	};

	struct Store {
		Value address;
		Value size;
		Value value; // Value to store // Maybe store bits of information (BOI) as a Value inside Value?
	};

	struct Touch {
		Value begin;
		Value end;
	};

	struct ExtCall {
		Value address;
		List<Value> input;
		List<Value> output;
	};

	struct Fence {
		Fence* parent;
		Context* context;
		List<Load>    loads;
		List<Store>   stores;
		List<Touch>   touches;
		List<ExtCall> extcalls;
		List<Fence*> children;
		// Closure?
	};

	static FixedBuffer<Fence, 1<<20> fences;

	static void PrintState();
};

static void Write(struct OutputBuffer* buffer, IR::Value value);
static void Write(struct OutputBuffer* buffer, IR::Value from, IR::Relation relation);
static void Write(struct OutputBuffer* buffer, IR::Value from, Array<IR::Relation> relations);
static void Write(struct OutputBuffer* buffer, IR::Relation::Kind kind);
static void Write(struct OutputBuffer* buffer, IR::Context context);
static void Write(struct OutputBuffer* buffer, IR::ValueFlag);

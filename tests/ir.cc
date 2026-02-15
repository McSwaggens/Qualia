#include "../file_system.h"
#include "../int.h"
#include "../ir.h"
#include "../list.h"

namespace Ast { struct Module; }
static List<Ast::Module*> modules = null;

#include "../general.cc"
#include "../thread.cc"
#include "../stacktrace.cc"
#include "../assert.cc"
#include "../file_system.cc"
#include "../stack.cc"
#include "../error.cc"
#include "../type_system.cc"
#include "../print.cc"
#include "../ir.cc"
#include "../alloc.cc"

static IR::Context* ctx;

// Distance(a, b, 0) means equality (via Equal helper)
// if Equal(a, b) then IsEqual(a, b) == true
static void Test_Equal() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Equal(a, b);

    Assert(ctx->IsEqual(a, b) == true, "Equal(a, b) should make IsEqual(a, b) true");
}

// Less transitivity
// if a < b && b < c then a < c
static void Test_LessTransitivity() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Less(a, b);
    ctx->Less(b, c);

    Assert(ctx->IsLess(a, c) == true, "Less transitivity: a < b && b < c should infer a < c");
}

// Less implies LessOrEqual
// if a < b then a <= b
static void Test_LessImpliesLessOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsLess(a, b) == true, "a < b should be recorded");
    Assert(ctx->IsLessOrEqual(a, b) == true, "a < b should imply a <= b");
}

// Less implies NotEqual
// if a < b then a != b
static void Test_LessImpliesNotEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsNotEqual(a, b) == true, "a < b should imply a != b");
}

// Duality - Less and Greater
// if a < b then b > a
static void Test_DualityLessGreater() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsLess(a, b) == true, "a < b should be recorded");
    Assert(ctx->IsGreater(b, a) == true, "a < b should imply b > a (duality)");
}

// LessOrEqual transitivity
// if a <= b && b <= c then a <= c
static void Test_LessOrEqualTransitivity() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->LessOrEqual(a, b);
    ctx->LessOrEqual(b, c);

    Assert(ctx->IsLessOrEqual(a, c) == true, "LessOrEqual transitivity: a <= b && b <= c should infer a <= c");
}

// Greater implies GreaterOrEqual
// if a > b then a >= b
static void Test_GreaterImpliesGreaterOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Greater(a, b);

    Assert(ctx->IsGreater(a, b) == true, "a > b should be recorded");
    Assert(ctx->IsGreaterOrEqual(a, b) == true, "a > b should imply a >= b");
}

// NotEqual is symmetric
// if a != b then b != a
static void Test_NotEqualSymmetric() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->NotEqual(a, b);

    Assert(ctx->IsNotEqual(a, b) == true, "a != b should be recorded");
    Assert(ctx->IsNotEqual(b, a) == true, "a != b should imply b != a");
}

// Greater transitivity
// if a > b && b > c then a > c
static void Test_GreaterTransitivity() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Greater(a, b);
    ctx->Greater(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Greater transitivity: a > b && b > c should infer a > c");
}

// GreaterOrEqual transitivity
// if a >= b && b >= c then a >= c
static void Test_GreaterOrEqualTransitivity() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->GreaterOrEqual(b, c);

    Assert(ctx->IsGreaterOrEqual(a, c) == true, "GreaterOrEqual transitivity: a >= b && b >= c should infer a >= c");
}

// Mixed transitivity (Less + LessOrEqual)
// if a < b && b <= c then a < c
static void Test_MixedTransitivity_Less_LessOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Less(a, b);
    ctx->LessOrEqual(b, c);

    Assert(ctx->IsLess(a, c) == true, "Mixed transitivity: a < b && b <= c should infer a < c");
}

// Mixed transitivity (LessOrEqual + Less)
// if a <= b && b < c then a < c
static void Test_MixedTransitivity_LessOrEqual_Less() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->LessOrEqual(a, b);
    ctx->Less(b, c);

    Assert(ctx->IsLess(a, c) == true, "Mixed transitivity: a <= b && b < c should infer a < c");
}

// Mixed transitivity (Greater + GreaterOrEqual)
// if a > b && b >= c then a > c
static void Test_MixedTransitivity_Greater_GreaterOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Greater(a, b);
    ctx->GreaterOrEqual(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Mixed transitivity: a > b && b >= c should infer a > c");
}

// Mixed transitivity (GreaterOrEqual + Greater)
// if a >= b && b > c then a > c
static void Test_MixedTransitivity_GreaterOrEqual_Greater() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->Greater(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Mixed transitivity: a >= b && b > c should infer a > c");
}

// Antisymmetry (LessOrEqual)
// if a <= b && b <= a then a == b
static void Test_AntisymmetryLessOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->LessOrEqual(a, b);
    ctx->LessOrEqual(b, a);

    Assert(ctx->IsEqual(a, b) == true, "Antisymmetry: a <= b && b <= a should infer a == b");
}

// Antisymmetry (GreaterOrEqual)
// if a >= b && b >= a then a == b
static void Test_AntisymmetryGreaterOrEqual() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->GreaterOrEqual(b, a);

    Assert(ctx->IsEqual(a, b) == true, "Antisymmetry: a >= b && b >= a should infer a == b");
}

// Distance equality propagation (Less)
// if Distance(a, b, 0) && a < c then b < c
static void Test_DistanceEqualityPropagation_Less() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Equal(a, b);
    ctx->Less(a, c);

    Assert(ctx->IsLess(b, c) == true, "Distance equality: a == b && a < c should infer b < c");
}

// Distance equality propagation (Less, reversed input)
// if Distance(a, b, 0) && c < a then c < b
static void Test_DistanceEqualityPropagation_LessReversedInput() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Equal(a, b);
    ctx->Less(c, a);

    Assert(ctx->IsLess(c, b) == true, "Distance equality: a == b && c < a should infer c < b");
}

// Remainder basic storage
// Remainder(a, b, r) should be retrievable
static void Test_RemainderBasicStorage() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value r = IR::Constant(3);

    ctx->Remainder(a, b, r);

    // Check that the relation was added
    IR::Context::RelationRange range = ctx->GetRelationRangeTo(a, b);
    bool found = false;
    for (u32 i = range.begin; i < range.end; i++) {
        IR::Relation& rel = a->relations.elements[i];
        if (rel.kind == IR::RelationKind::Remainder && rel.value == r && ctx->CanSee(rel)) {
            found = true;
            break;
        }
    }
    Assert(found, "Remainder(a, b, r) should be stored");
}

// Remainder propagation through equality (first arg)
// if Distance(a, x, 0) && Remainder(a, b, r) then Remainder(x, b, r)
static void Test_RemainderPropagationEquality() {
    IR::Value a = IR::NewValue();
    IR::Value x = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value r = IR::Constant(5);

    ctx->Equal(a, x);
    ctx->Remainder(a, b, r);

    IR::Context::RelationRange range = ctx->GetRelationRangeTo(x, b);
    bool found = false;
    for (u32 i = range.begin; i < range.end; i++) {
        IR::Relation& rel = x->relations.elements[i];
        if (rel.kind == IR::RelationKind::Remainder && rel.value == r && ctx->CanSee(rel)) {
            found = true;
            break;
        }
    }
    Assert(found, "Remainder equality propagation: a == x && Remainder(a,b,r) should infer Remainder(x,b,r)");
}

// Context child sees parent relations
static void Test_ContextChildSeesParent() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Less(a, b);

    // Create child context
    IR::Context* child = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });

    Assert(child->IsLess(a, b) == true, "Child context should see parent's Less(a, b)");
}

// Context child can add relations
static void Test_ContextChildCanAddRelations() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Less(a, b);

    // Create child context and add relation
    IR::Context* child = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    child->Less(b, c);

    // Child should see both
    Assert(child->IsLess(a, b) == true, "Child sees parent relation a < b");
    Assert(child->IsLess(b, c) == true, "Child sees own relation b < c");

    // Child should infer transitivity
    Assert(child->IsLess(a, c) == true, "Child should infer a < c from transitivity");
}

// Parent cannot see child relations
static void Test_ParentCannotSeeChild() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    // Create child context and add relation
    IR::Context* child = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    child->Less(a, b);

    // Parent should not see child's relation
    Assert(ctx->IsLess(a, b) == OptNone, "Parent should not see child's Less(a, b)");
}

// Constant deduplication
static void Test_ConstantDeduplication() {
    IR::Value c1 = IR::Constant(42);
    IR::Value c2 = IR::Constant(42);
    IR::Value c3 = IR::Constant(99);

    Assert(c1 == c2, "Constant(42) should be deduplicated");
    Assert(c1 != c3, "Constant(42) != Constant(99)");
}

// NewValue creates distinct values
static void Test_NewValueDistinct() {
    IR::Value v1 = IR::NewValue();
    IR::Value v2 = IR::NewValue();

    Assert(v1 != v2, "NewValue() should create distinct values");
}

// Self-relation (Less)
// Less(a, a) is false (not unknown)
static void Test_SelfRelation_Less() {
    IR::Value a = IR::NewValue();

    ctx->Less(a, a);

    Assert(ctx->IsLess(a, a) == false, "IsLess(a, a) should be false");
}

// Self-relation (Equal)
// Equal(a, a) is trivially true
static void Test_SelfRelation_Equal() {
    IR::Value a = IR::NewValue();

    Assert(ctx->IsEqual(a, a) == true, "IsEqual(a, a) should be trivially true");
}

// Self-relation (NotEqual)
// NotEqual(a, a) is false (not unknown)
static void Test_SelfRelation_NotEqual() {
    IR::Value a = IR::NewValue();

    ctx->NotEqual(a, a);

    Assert(ctx->IsNotEqual(a, a) == false, "IsNotEqual(a, a) should be false");
}

// Long transitive chain
// a0 < a1 < a2 < a3 < a4 should infer a0 < a4
static void Test_LongTransitiveChain() {
    IR::Value a0 = IR::NewValue();
    IR::Value a1 = IR::NewValue();
    IR::Value a2 = IR::NewValue();
    IR::Value a3 = IR::NewValue();
    IR::Value a4 = IR::NewValue();

    ctx->Less(a0, a1);
    ctx->Less(a1, a2);
    ctx->Less(a2, a3);
    ctx->Less(a3, a4);

    Assert(ctx->IsLess(a0, a4) == true, "Long chain: a0 < a1 < a2 < a3 < a4 should infer a0 < a4");
}

// Negative involution
// Negative(Negative(a)) should equal a
static void Test_NegativeInvolution() {
    IR::Value a = IR::NewValue();
    IR::Value neg_a = ctx->Negative(a);
    IR::Value neg_neg_a = ctx->Negative(neg_a);

    Assert(ctx->IsEqual(a, neg_neg_a) == true, "Negative involution: -(-a) == a");
}

// Negative of constant
// Negative(Constant(0)) should be Constant(0)
static void Test_NegativeOfZero() {
    IR::Value zero = IR::Constant(0);
    IR::Value neg_zero = ctx->Negative(zero);

    Assert(zero == neg_zero, "Negative(0) should be 0");
}

// Equal via Distance establishes bidirectional equality
// if Equal(a, b) then IsEqual(b, a)
static void Test_EqualBidirectional() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();

    ctx->Equal(a, b);

    Assert(ctx->IsEqual(b, a) == true, "Equal(a, b) should make IsEqual(b, a) true");
}

// LessOrEqual with self is trivially true
static void Test_LessOrEqualWithSelf() {
    IR::Value a = IR::NewValue();

    ctx->LessOrEqual(a, a);

    Assert(ctx->IsLessOrEqual(a, a) == true, "a <= a should be trivially true");
}

// GreaterOrEqual with self is trivially true
static void Test_GreaterOrEqualWithSelf() {
    IR::Value a = IR::NewValue();

    ctx->GreaterOrEqual(a, a);

    Assert(ctx->IsGreaterOrEqual(a, a) == true, "a >= a should be trivially true");
}

// Equality transitivity
// if a == b && b == c then a == c
static void Test_EqualityTransitivity() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();

    ctx->Equal(a, b);
    ctx->Equal(b, c);

    Assert(ctx->IsEqual(a, c) == true, "Equality transitivity: a == b && b == c should infer a == c");
}

// Nested conditional contexts (realistic control flow)
// IR equivalent of:
//   Foo(a: int, b: int) -> int:
//       c := 1
//       r := 0
//       // Context A
//       if a < b:
//           c = 42
//           // Context B
//       if c = 42:
//           // Context C
//           if a < b:
//               r = 1
//               // Context D
//       if a < b:
//           // Context E
//       // Context F
// Tests context hierarchy, inheritance, and sibling isolation
static void Test_NestedConditionalContexts() {
    IR::Value a = IR::NewValue();
    IR::Value b = IR::NewValue();
    IR::Value c = IR::NewValue();
    IR::Value c_cond = IR::NewValue();  // Use separate value for condition to avoid conflicts
    IR::Value r = IR::NewValue();
    IR::Value one = IR::Constant(1);
    IR::Value forty_two = IR::Constant(42);
    IR::Value zero = IR::Constant(0);

    // Context A: root context, nothing known about relationships
    Assert(ctx->IsLess(a, b) == OptNone, "Context A: don't know if a < b");

    // Context B: if (a < b) { c = 42; }
    IR::Context* ctxB = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    ctxB->Less(a, b);  // Add the actual relation we're testing
    Assert(ctxB->IsLess(a, b) == true, "Context B: knows a < b from condition");
    // Inside B, we assign c = 42
    ctxB->Equal(c, forty_two);
    Assert(ctxB->IsEqual(c, forty_two) == true, "Context B: knows c == 42 after assignment");

    // Back to Context A - parent shouldn't see child's assignments
    Assert(ctx->IsEqual(c, forty_two) == OptNone, "Context A: doesn't see child B's c == 42");
    Assert(ctx->IsLess(a, b) == OptNone, "Context A: doesn't know a < b outside branch");

    // Context C: if (c == 42) at root level (sibling to B)
    IR::Context* ctxC = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    ctxC->Equal(c, forty_two);  // Add the actual relation we're testing
    Assert(ctxC->IsEqual(c, forty_two) == true, "Context C: knows c == 42 from condition");
    Assert(ctxC->IsLess(a, b) == OptNone, "Context C: doesn't know a < b (that was only in sibling B)");

    // Context D: nested inside C, if (a < b) { r = 1; }
    IR::Context* ctxD = ctxC->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    ctxD->Less(a, b);  // Add the actual relation we're testing
    Assert(ctxD->IsEqual(c, forty_two) == true, "Context D: inherits c == 42 from parent C");
    Assert(ctxD->IsLess(a, b) == true, "Context D: knows a < b from own condition");
    // Key test: D knows BOTH c == 42 (inherited) AND a < b (own condition)
    // This models the case where we're inside nested ifs with different conditions
    ctxD->Equal(r, one);
    Assert(ctxD->IsEqual(r, one) == true, "Context D: knows r == 1 after assignment");

    // Context E: back to root level, another if (a < b)
    // This creates a sibling context (different key from B because different values)
    IR::Context* ctxE = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    ctxE->Less(a, b);  // Add the actual relation we're testing
    Assert(ctxE->IsLess(a, b) == true, "Context E: knows a < b from condition");
    Assert(ctxE->IsEqual(c, forty_two) == OptNone, "Context E: doesn't know c == 42 (was in sibling branch C/D)");
    Assert(ctxE->IsEqual(r, one) == OptNone, "Context E: doesn't know r == 1 (was in sibling branch D)");

    // Context F: back to root after all branches
    Assert(ctx->IsLess(a, b) == OptNone, "Context F (root): still doesn't know a < b");
    Assert(ctx->IsEqual(c, forty_two) == OptNone, "Context F (root): still doesn't know c == 42");
    Assert(ctx->IsEqual(r, one) == OptNone, "Context F (root): still doesn't know r == 1");

    // Verify complete isolation: siblings don't see each other's relations
    Assert(ctxB->IsEqual(r, one) == OptNone, "Context B: doesn't see sibling D's r == 1");
    Assert(ctxE->IsEqual(c, forty_two) == OptNone, "Context E: doesn't see sibling B's c == 42");
}

// Stress test: many values/relations across many contexts
// Verifies long-distance inference and strict context isolation.
static void Test_StressManyValuesManyContexts() {
    constexpr u32 CHAIN_COUNT = 128;
    constexpr u32 NOISE_COUNT = 64;
    constexpr u32 ALIAS_COUNT = 32;

    // Long root chain: v0 < v1 < ... < v127
    IR::Value chain[CHAIN_COUNT];
    for (u32 i = 0; i < CHAIN_COUNT; i++)
        chain[i] = IR::NewValue();
    for (u32 i = 0; i + 1 < CHAIN_COUNT; i++)
        ctx->Less(chain[i], chain[i + 1]);

    // Add many different relation kinds in root context.
    IR::Value aliases[ALIAS_COUNT];
    for (u32 i = 0; i < ALIAS_COUNT; i++) {
        aliases[i] = IR::NewValue();
        ctx->Equal(chain[i], aliases[i]); // Distance(..., 0)
    }

    for (u32 i = 0; i < NOISE_COUNT; i++) {
        IR::Value a = IR::NewValue();
        IR::Value b = IR::NewValue();
        IR::Value c = IR::NewValue();
        IR::Value d = IR::NewValue();

        ctx->LessOrEqual(a, b);
        ctx->Greater(c, d);
        ctx->NotEqual(a, d);
        ctx->Remainder(b, c, IR::Constant(i % 11));
    }

    Assert(ctx->IsLess(chain[0], chain[CHAIN_COUNT - 1]) == true, "Root should infer long-chain Less across many hops");
    Assert(ctx->IsEqual(chain[0], aliases[0]) == true, "Root should preserve equality aliases under load");

    // Build sibling context trees.
    IR::Context* left = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    IR::Context* right = ctx->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    IR::Context* left_deep = left->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    IR::Context* left_far = left_deep->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });
    IR::Context* right_deep = right->Get({ .kind = IR::RelationKind::NotEqual, .from = IR::NewValue(), .to = IR::NewValue() });

    // Left branch extends the long root chain.
    IR::Value left_tail = IR::NewValue();
    IR::Value left_mid = IR::NewValue();
    IR::Value left_goal = IR::NewValue();
    left->Less(chain[CHAIN_COUNT - 1], left_tail);
    left_deep->LessOrEqual(left_tail, left_mid);
    left_far->Less(left_mid, left_goal);

    Assert(left_far->IsLess(chain[0], left_goal) == true, "Deep context should infer through long root chain and branch-local relations");
    Assert(left_far->IsLess(aliases[0], left_goal) == true, "Deep context should use equality alias plus long-distance transitivity");

    // Right branch adds unrelated constraints that must not leak to left.
    IR::Value right_x = IR::NewValue();
    IR::Value right_y = IR::NewValue();
    right_deep->Greater(right_x, right_y);

    Assert(right_deep->IsLess(chain[0], chain[CHAIN_COUNT - 1]) == true, "Sibling branch should still inherit distant root inference");
    Assert(right_deep->IsGreater(right_x, right_y) == true, "Right branch should see its own relations");

    Assert(ctx->IsLess(chain[CHAIN_COUNT - 1], left_tail) == OptNone, "Root should not see child-only relations");
    Assert(right_deep->IsLess(chain[CHAIN_COUNT - 1], left_tail) == OptNone, "Sibling branch should not see left-branch relations");
    Assert(left_far->IsGreater(right_x, right_y) == OptNone, "Left branch should not see right-branch relations");
}

int main() {
    InitCrashHandler();
    InitGlobalAllocator();
    Thread::Init();
    InitTypeSystem();
    IR::Init();

    ctx = &IR::empty_context;

    Print("Running IR tests...\n");

    Test_Equal();
    Test_LessTransitivity();
    Test_LessImpliesLessOrEqual();
    Test_LessImpliesNotEqual();
    Test_DualityLessGreater();
    Test_LessOrEqualTransitivity();
    Test_GreaterImpliesGreaterOrEqual();
    Test_NotEqualSymmetric();
    Test_GreaterTransitivity();
    Test_GreaterOrEqualTransitivity();
    Test_MixedTransitivity_Less_LessOrEqual();
    Test_MixedTransitivity_LessOrEqual_Less();
    Test_MixedTransitivity_Greater_GreaterOrEqual();
    Test_MixedTransitivity_GreaterOrEqual_Greater();
    Test_AntisymmetryLessOrEqual();
    Test_AntisymmetryGreaterOrEqual();
    Test_DistanceEqualityPropagation_Less();
    Test_DistanceEqualityPropagation_LessReversedInput();
    Test_RemainderBasicStorage();
    Test_RemainderPropagationEquality();
    Test_ContextChildSeesParent();
    Test_ContextChildCanAddRelations();
    Test_ParentCannotSeeChild();
    Test_ConstantDeduplication();
    Test_NewValueDistinct();
    Test_SelfRelation_Less();
    Test_SelfRelation_Equal();
    Test_SelfRelation_NotEqual();
    Test_LongTransitiveChain();
    Test_NegativeInvolution();
    Test_NegativeOfZero();
    Test_EqualBidirectional();
    Test_LessOrEqualWithSelf();
    Test_GreaterOrEqualWithSelf();
    Test_EqualityTransitivity();
    Test_NestedConditionalContexts();
    Test_StressManyValuesManyContexts();

    Print("All IR tests passed!\n");
    output_buffer.Flush();
    OS::Terminate(true);
}

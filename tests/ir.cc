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

    Print("All IR tests passed!\n");
    output_buffer.Flush();
    OS::Terminate(true);
}

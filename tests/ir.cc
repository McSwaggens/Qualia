#include "../file_system.h"
#include "../int.h"
#include "../ir.h"
#include "../list.h"

namespace Ast { struct Module; }
static List<Ast::Module*> modules = null;

#include "assert.h"

#include "../general.cc"
#include "../thread.cc"
#include "../stacktrace.cc"
#include "../assert.cc"
#include "../file_system.cc"
#include "../stack.cc"
#include "../error.cc"
#include "../type_system.cc"
#include "../lexer.cc"
#include "../array_buffer.cc"
#include "../print.cc"
#include "../semantic.cc"
#include "../parser.cc"
#include "../ir.cc"
#include "../alloc.cc"

#include "../ast.cc"

using namespace IR;

static void ParseModule(Ast::Module* module) {
	Parser parser = Parser(module);
	parser.ParseGlobalScope();
	SemanticParse(module);
}

static Ast::Module* CompileString(String code_string) {
	Ast::Module* module = new Ast::Module("<string>", "<string>");
	modules.Add(module);

	Lexer lexer = Lexer(module, code_string, "<string>");
	lexer.Parse();

	ParseModule(module);

	return module;
}

static Context* ctx;

// Distance(a, b, 0) means equality (via Equal helper)
// if Equal(a, b) then IsEqual(a, b) == true
static void Test_Equal() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Equal(a, b);

    Assert(ctx->IsEqual(a, b) == true, "Equal(a, b) should make IsEqual(a, b) true");
}

// Less transitivity
// if a < b && b < c then a < c
static void Test_LessTransitivity() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Less(a, b);
    ctx->Less(b, c);

    Assert(ctx->IsLess(a, c) == true, "Less transitivity: a < b && b < c should infer a < c");
}

// Less implies LessOrEqual
// if a < b then a <= b
static void Test_LessImpliesLessOrEqual() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsLess(a, b) == true, "a < b should be recorded");
    Assert(ctx->IsLessOrEqual(a, b) == true, "a < b should imply a <= b");
}

// Less implies NotEqual
// if a < b then a != b
static void Test_LessImpliesNotEqual() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsNotEqual(a, b) == true, "a < b should imply a != b");
}

// Duality - Less and Greater
// if a < b then b > a
static void Test_DualityLessGreater() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Less(a, b);

    Assert(ctx->IsLess(a, b) == true, "a < b should be recorded");
    Assert(ctx->IsGreater(b, a) == true, "a < b should imply b > a (duality)");
}

// LessOrEqual transitivity
// if a <= b && b <= c then a <= c
static void Test_LessOrEqualTransitivity() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->LessOrEqual(a, b);
    ctx->LessOrEqual(b, c);

    Assert(ctx->IsLessOrEqual(a, c) == true, "LessOrEqual transitivity: a <= b && b <= c should infer a <= c");
}

// Greater implies GreaterOrEqual
// if a > b then a >= b
static void Test_GreaterImpliesGreaterOrEqual() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Greater(a, b);

    Assert(ctx->IsGreater(a, b) == true, "a > b should be recorded");
    Assert(ctx->IsGreaterOrEqual(a, b) == true, "a > b should imply a >= b");
}

// NotEqual is symmetric
// if a != b then b != a
static void Test_NotEqualSymmetric() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->NotEqual(a, b);

    Assert(ctx->IsNotEqual(a, b) == true, "a != b should be recorded");
    Assert(ctx->IsNotEqual(b, a) == true, "a != b should imply b != a");
}

// Greater transitivity
// if a > b && b > c then a > c
static void Test_GreaterTransitivity() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Greater(a, b);
    ctx->Greater(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Greater transitivity: a > b && b > c should infer a > c");
}

// GreaterOrEqual transitivity
// if a >= b && b >= c then a >= c
static void Test_GreaterOrEqualTransitivity() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->GreaterOrEqual(b, c);

    Assert(ctx->IsGreaterOrEqual(a, c) == true, "GreaterOrEqual transitivity: a >= b && b >= c should infer a >= c");
}

// Mixed transitivity (Less + LessOrEqual)
// if a < b && b <= c then a < c
static void Test_MixedTransitivity_Less_LessOrEqual() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Less(a, b);
    ctx->LessOrEqual(b, c);

    Assert(ctx->IsLess(a, c) == true, "Mixed transitivity: a < b && b <= c should infer a < c");
}

// Mixed transitivity (LessOrEqual + Less)
// if a <= b && b < c then a < c
static void Test_MixedTransitivity_LessOrEqual_Less() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->LessOrEqual(a, b);
    ctx->Less(b, c);

    Assert(ctx->IsLess(a, c) == true, "Mixed transitivity: a <= b && b < c should infer a < c");
}

// Mixed transitivity (Greater + GreaterOrEqual)
// if a > b && b >= c then a > c
static void Test_MixedTransitivity_Greater_GreaterOrEqual() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Greater(a, b);
    ctx->GreaterOrEqual(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Mixed transitivity: a > b && b >= c should infer a > c");
}

// Mixed transitivity (GreaterOrEqual + Greater)
// if a >= b && b > c then a > c
static void Test_MixedTransitivity_GreaterOrEqual_Greater() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->Greater(b, c);

    Assert(ctx->IsGreater(a, c) == true, "Mixed transitivity: a >= b && b > c should infer a > c");
}

// Antisymmetry (LessOrEqual)
// if a <= b && b <= a then a == b
static void Test_AntisymmetryLessOrEqual() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->LessOrEqual(a, b);
    ctx->LessOrEqual(b, a);

    Assert(ctx->IsEqual(a, b) == true, "Antisymmetry: a <= b && b <= a should infer a == b");
}

// Antisymmetry (GreaterOrEqual)
// if a >= b && b >= a then a == b
static void Test_AntisymmetryGreaterOrEqual() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->GreaterOrEqual(a, b);
    ctx->GreaterOrEqual(b, a);

    Assert(ctx->IsEqual(a, b) == true, "Antisymmetry: a >= b && b >= a should infer a == b");
}

// Distance equality propagation (Less)
// if Distance(a, b, 0) && a < c then b < c
static void Test_DistanceEqualityPropagation_Less() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Equal(a, b);
    ctx->Less(a, c);

    Assert(ctx->IsLess(b, c) == true, "Distance equality: a == b && a < c should infer b < c");
}

// Distance equality propagation (Less, reversed input)
// if Distance(a, b, 0) && c < a then c < b
static void Test_DistanceEqualityPropagation_LessReversedInput() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Equal(a, b);
    ctx->Less(c, a);

    Assert(ctx->IsLess(c, b) == true, "Distance equality: a == b && c < a should infer c < b");
}

// Remainder basic storage
// Remainder(a, b, r) should be retrievable
static void Test_RemainderBasicStorage() {
    Value a = NewValue();
    Value b = NewValue();
    Value r = Constant(3);

    ctx->Remainder(a, b, r);

    // Check that the relation was added
    Context::RelationRange range = ctx->GetRelationRangeTo(a, b);
    bool found = false;
    for (u32 i = range.begin; i < range.end; i++) {
        Relation& rel = a->relations.elements[i];
        if (rel.kind == Relation::Remainder && rel.value == r && ctx->CanSee(rel)) {
            found = true;
            break;
        }
    }
    Assert(found, "Remainder(a, b, r) should be stored");
}

// Remainder propagation through equality (first arg)
// if Distance(a, x, 0) && Remainder(a, b, r) then Remainder(x, b, r)
static void Test_RemainderPropagationEquality() {
    Value a = NewValue();
    Value x = NewValue();
    Value b = NewValue();
    Value r = Constant(5);

    ctx->Equal(a, x);
    ctx->Remainder(a, b, r);

    Context::RelationRange range = ctx->GetRelationRangeTo(x, b);
    bool found = false;
    for (u32 i = range.begin; i < range.end; i++) {
        Relation& rel = x->relations.elements[i];
        if (rel.kind == Relation::Remainder && rel.value == r && ctx->CanSee(rel)) {
            found = true;
            break;
        }
    }
    Assert(found, "Remainder equality propagation: a == x && Remainder(a,b,r) should infer Remainder(x,b,r)");
}

// Context child sees parent relations
static void Test_ContextChildSeesParent() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Less(a, b);

    // Create child context
    Context* child = ctx->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));

    Assert(child->IsLess(a, b) == true, "Child context should see parent's Less(a, b)");
}

// Context child can add relations
static void Test_ContextChildCanAddRelations() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Less(a, b);

    // Create child context and add relation
    Context* child = ctx->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    child->Less(b, c);

    // Child should see both
    Assert(child->IsLess(a, b) == true, "Child sees parent relation a < b");
    Assert(child->IsLess(b, c) == true, "Child sees own relation b < c");

    // Child should infer transitivity
    Assert(child->IsLess(a, c) == true, "Child should infer a < c from transitivity");
}

// Parent cannot see child relations
static void Test_ParentCannotSeeChild() {
    Value a = NewValue();
    Value b = NewValue();

    // Create child context and add relation
    Context* child = ctx->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    child->Less(a, b);

    // Parent should not see child's relation
    Assert(ctx->IsLess(a, b) == OptNone, "Parent should not see child's Less(a, b)");
}

// Constant deduplication
static void Test_ConstantDeduplication() {
    Value c1 = Constant(42);
    Value c2 = Constant(42);
    Value c3 = Constant(99);

    Assert(c1 == c2, "Constant(42) should be deduplicated");
    Assert(c1 != c3, "Constant(42) != Constant(99)");
}

// NewValue creates distinct values
static void Test_NewValueDistinct() {
    Value v1 = NewValue();
    Value v2 = NewValue();

    Assert(v1 != v2, "NewValue() should create distinct values");
}

// Self-relation (Less)
// Less(a, a) is false (not unknown)
static void Test_SelfRelation_Less() {
    Value a = NewValue();

    ctx->Less(a, a);

    Assert(ctx->IsLess(a, a) == false, "IsLess(a, a) should be false");
}

// Self-relation (Equal)
// Equal(a, a) is trivially true
static void Test_SelfRelation_Equal() {
    Value a = NewValue();

    Assert(ctx->IsEqual(a, a) == true, "IsEqual(a, a) should be trivially true");
}

// Self-relation (NotEqual)
// NotEqual(a, a) is false (not unknown)
static void Test_SelfRelation_NotEqual() {
    Value a = NewValue();

    ctx->NotEqual(a, a);

    Assert(ctx->IsNotEqual(a, a) == false, "IsNotEqual(a, a) should be false");
}

// Long transitive chain
// a0 < a1 < a2 < a3 < a4 should infer a0 < a4
static void Test_LongTransitiveChain() {
    Value a0 = NewValue();
    Value a1 = NewValue();
    Value a2 = NewValue();
    Value a3 = NewValue();
    Value a4 = NewValue();

    ctx->Less(a0, a1);
    ctx->Less(a1, a2);
    ctx->Less(a2, a3);
    ctx->Less(a3, a4);

    Assert(ctx->IsLess(a0, a4) == true, "Long chain: a0 < a1 < a2 < a3 < a4 should infer a0 < a4");
}

// Negative involution
// Negative(Negative(a)) should equal a
static void Test_NegativeInvolution() {
    Value a = NewValue();
    Value neg_a = ctx->Negative(a);
    Value neg_neg_a = ctx->Negative(neg_a);

    Assert(ctx->IsEqual(a, neg_neg_a) == true, "Negative involution: -(-a) == a");
}

// Negative of constant
// Negative(Constant(0)) should be Constant(0)
static void Test_NegativeOfZero() {
    Value zero = Constant(0);
    Value neg_zero = ctx->Negative(zero);

    Assert(zero == neg_zero, "Negative(0) should be 0");
}

// Equal via Distance establishes bidirectional equality
// if Equal(a, b) then IsEqual(b, a)
static void Test_EqualBidirectional() {
    Value a = NewValue();
    Value b = NewValue();

    ctx->Equal(a, b);

    Assert(ctx->IsEqual(b, a) == true, "Equal(a, b) should make IsEqual(b, a) true");
}

// LessOrEqual with self is trivially true
static void Test_LessOrEqualWithSelf() {
    Value a = NewValue();

    ctx->LessOrEqual(a, a);

    Assert(ctx->IsLessOrEqual(a, a) == true, "a <= a should be trivially true");
}

// GreaterOrEqual with self is trivially true
static void Test_GreaterOrEqualWithSelf() {
    Value a = NewValue();

    ctx->GreaterOrEqual(a, a);

    Assert(ctx->IsGreaterOrEqual(a, a) == true, "a >= a should be trivially true");
}

// Equality transitivity
// if a == b && b == c then a == c
static void Test_EqualityTransitivity() {
    Value a = NewValue();
    Value b = NewValue();
    Value c = NewValue();

    ctx->Equal(a, b);
    ctx->Equal(b, c);

    Assert(ctx->IsEqual(a, c) == true, "Equality transitivity: a == b && b == c should infer a == c");
}

// Context inference.
static void Test_ContextInference() {
	return; // This test is disabled because lowering isn't implemented yet.

	Ast::Module* module = CompileString(R"(
Foo(a: int, b: int);
	c : int = 0
	if a < b: c = 42
	else:     c = 1

	if c = 42:
		harness0 := 0
)");
}

// Stress test: many values/relations across many contexts
// Verifies long-distance inference and strict context isolation.
static void Test_StressManyValuesManyContexts() {
    constexpr u32 CHAIN_COUNT = 128;
    constexpr u32 NOISE_COUNT = 64;
    constexpr u32 ALIAS_COUNT = 32;

    // Long root chain: v0 < v1 < ... < v127
    Value chain[CHAIN_COUNT];
    for (u32 i = 0; i < CHAIN_COUNT; i++)
        chain[i] = NewValue();
    for (u32 i = 0; i + 1 < CHAIN_COUNT; i++)
        ctx->Less(chain[i], chain[i + 1]);

    // Add many different relation kinds in root context.
    Value aliases[ALIAS_COUNT];
    for (u32 i = 0; i < ALIAS_COUNT; i++) {
        aliases[i] = NewValue();
        ctx->Equal(chain[i], aliases[i]); // Distance(..., 0)
    }

    for (u32 i = 0; i < NOISE_COUNT; i++) {
        Value a = NewValue();
        Value b = NewValue();
        Value c = NewValue();
        Value d = NewValue();

        ctx->LessOrEqual(a, b);
        ctx->Greater(c, d);
        ctx->NotEqual(a, d);
        ctx->Remainder(b, c, Constant(i % 11));
    }

    Assert(ctx->IsLess(chain[0], chain[CHAIN_COUNT - 1]) == true, "Root should infer long-chain Less across many hops");
    Assert(ctx->IsEqual(chain[0], aliases[0]) == true, "Root should preserve equality aliases under load");

    // Build sibling context trees.
    Context* left = ctx->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    Context* right = ctx->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    Context* left_deep = left->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    Context* left_far = left_deep->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));
    Context* right_deep = right->Get(Context::Key(Relation::NotEqual, NewValue(), NewValue()));

    // Left branch extends the long root chain.
    Value left_tail = NewValue();
    Value left_mid = NewValue();
    Value left_goal = NewValue();
    left->Less(chain[CHAIN_COUNT - 1], left_tail);
    left_deep->LessOrEqual(left_tail, left_mid);
    left_far->Less(left_mid, left_goal);

    Assert(left_far->IsLess(chain[0], left_goal) == true, "Deep context should infer through long root chain and branch-local relations");
    Assert(left_far->IsLess(aliases[0], left_goal) == true, "Deep context should use equality alias plus long-distance transitivity");

    // Right branch adds unrelated constraints that must not leak to left.
    Value right_x = NewValue();
    Value right_y = NewValue();
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
    Init();

    ctx = &empty_context;

    Print("Running IR tests...\n");

    Test_ContextInference();
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
    Test_StressManyValuesManyContexts();

    Print("All IR tests passed!\n");
    output_buffer.Flush();
    OS::Terminate(true);
}

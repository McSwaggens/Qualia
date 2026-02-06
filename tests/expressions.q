// Expression syntax tests

TestArithmetic():
	a := 10
	b := 3

	// Arithmetic operators
	c0 := a + b
	c1 := a - b
	c2 := a * b
	c3 := a / b
	c4 := a % b     // BUG: '%' (TOKEN_MOD) has no case in lexer - never lexed

	// Nested arithmetic
	c5 := (a + b) * (a - b)
	c6 := a + b * 2
	c7 := (a + b) / 2

TestComparison():
	a := 10
	b := 20

	// Comparison operators
	c0 := a = b
	c1 := a != b
	c2 := a < b
	c3 := a <= b
	c4 := a > b
	c5 := a >= b

TestLogical():
	a := true
	b := false

	// Logical operators
	c0 := a && b
	c1 := a and b   // BUG: 'and' keyword missing from lexer case 'a:' handler (only && works)
	c2 := a or b    // BUG: 'or' keyword broken - ToString(TOKEN_OR) returns "||", so TestKeyword compares cursor against "||" instead of "or"
	c3 := not a
	c4 := !a

TestBitwise():
	a := 15
	b := 3

	// Bitwise operators
	c0 := a & b
	c1 := a | b
	c2 := a ^ b
	c3 := ~a        // BUG: '~' (TOKEN_TILDE) has no case in lexer - never lexed
	c4 := a << b
	c5 := a >> b

TestUnary():
	a := 42
	b := -a
	c := +a
	d := ~a         // BUG: '~' not in lexer (same as above)

TestTernary():
	a := 10
	b := 20

	// Ternary if-else expression
	c := a if a < b else b
	d := 1 if true else 0

TestCast():
	a := 42
	b := a as uint32

TestFixedArrayLiteral():
	a := {1, 2, 3}
	b := {10, 20, 30, 40, 50}

TestTupleExpression():
	a := (1, 2)
	b := (1, 2, 3)

TestPrecedence():
	a := 2 + 3 * 4
	b := (2 + 3) * 4
	c := 1 + 2 + 3
	d := 10 - 2 - 3
	e := 1 < 2 and 3 < 4        // BUG: 'and' keyword not lexed
	f := true or false and true  // BUG: both 'or' and 'and' keywords broken/missing in lexer

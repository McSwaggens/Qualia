// Expression syntax tests

TestArithmetic():
	a := 10
	b := 3

	// Arithmetic operators
	c0 := a + b
	c1 := a - b
	c2 := a * b
	c3 := a / b
	c4 := a % b

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
	c1 := a || b
	c3 := not a
	c4 := !a

TestBitwise():
	a := 15
	b := 3

	// Bitwise operators
	c0 := a & b
	c1 := a | b
	c2 := a ^ b
	c3 := ~a
	c4 := a << b
	c5 := a >> b

TestUnary():
	a := 42
	b := -a
	c := +a
	d := ~a

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
	// Standard precedence (uniform spacing)
	a := 2 + 3 * 4        // 2 + (3 * 4) = 14
	b := (2 + 3) * 4      // 20
	c := 1 + 2 + 3        // (1 + 2) + 3 = 6, left-associative
	d := 10 - 2 - 3       // (10 - 2) - 3 = 5, left-associative
	e := 1 < 2 && 3 < 4   // (1 < 2) && (3 < 4)
	f := true || false && true  // true || (false && true)

TestSpacingPrecedence():
	a := 2
	b := 3
	c := 4

	// Unspaced binds tighter than spaced
	d := a+b * c           // (a+b) * c = 20
	e := a * b+c           // a * (b+c) = 14
	f := a+b * c+d         // (a+b) * (c+d) = 30

	// Unspaced vs spaced with same operator
	g := a+b - c           // (a+b) - c = 1
	h := a - b+c           // a - (b+c) = -5

	// Standard precedence preserved when all spaced
	i := a + b * c         // a + (b * c) = 14
	j := a * b + c         // (a * b) + c = 10

	// Standard precedence preserved when all unspaced
	k := a+b*c             // a+(b*c) = 14
	l := a*b+c             // (a*b)+c = 10

TestPointerOps():
	x: int = 42
	// Address-of
	ptr := &x
	// Dereference
	y := *ptr
	// Dereference in expression
	z := *ptr + 5
	w := *ptr * 2

TestArrayRange(p: *int, count: uint64):
	// Array range from pointer and count
	arr := [p..count]
	n := arr.length

TestArrayRangeFromPtrs(begin: *int, end: *int):
	// Array range from two pointers
	arr := [begin..end]
	n := arr.length

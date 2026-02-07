// Literal syntax tests

TestIntegerLiterals():
	// Decimal integers
	a0 := 0
	a1 := 1
	a2 := 42
	a3 := 100
	a4 := 2_5_5
	a5 := 1000000
	a6 := 1_000_000

	// Signed integer type qualifiers (s prefix + size)
	b0 := 0s8
	b1 := 127s8
	b2 := 0s16
	b3 := 32000s16
	b4 := 0s32
	b5 := 2000000000s32
	b6 := 0s64
	b7 := 100s64

	// Unsigned integer type qualifiers (u prefix + size)
	c0 := 0u8
	c1 := 255u8
	c2 := 0u16
	c3 := 65535u16
	c4 := 0u32
	c5 := 4000000000u32
	c6 := 0u64
	c7 := 100u64

	// Binary literals (suffix b)
	d0 := 0b
	d1 := 1b
	d2 := 1010b
	d3 := 1111_0000b
	d4 := 11111111b
	d5 := 10101010b32

	// Scaler qualifiers
	e0 := 4k
	e1 := 2m
	e2 := 1g

TestHexLiterals():
	// Hexadecimal literals (suffix h)
	f0 := 0h
	f1 := FFh
	f2 := ffh
	f3 := DEAD_BEEFh
	f4 := cafe_babeh
	f5 := FFh32
	f6 := FFFFh16

TestFloatLiterals():
	g0 := 3.14
	g1 := 0.5
	g2 := 1.0
	g3 := 3.14f32
	g4 := 3.14f64
	g5 := 0.0f32
	g6 := 100.001f64

TestBoolLiterals():
	// Boolean literals
	h0 := true
	h1 := false

TestNullLiteral():
	// Null literal
	i0 := null

TestStringLiterals():
	// BUG: string literals crash - CreateValueFromLiteralToken has Assert() for TOKEN_LITERAL_STRING
	j0 := "hello"
	j1 := "world"
	j2 := "hello world"
	j3 := ""
	j4 := "tab\there"
	j5 := "newline\nhere"
	j6 := "escaped\\backslash"
	j7 := "escaped\"quote"
	j8 := "null\0char"

// Statement syntax tests

struct TestStruct:
	x: int
	y: int

TestVariableDecl():
	// Typed declaration
	a: int
	// Typed with initializer
	b: int = 42
	// Type inference
	c := 100

TestAssignment():
	a: int = 0
	// Simple assignment
	a = 10
	a = 20 + 5

TestCompoundAssignment():
	a: int = 10
	// Compound assignment operators
	a += 5
	a -= 3
	a *= 2
	a /= 4
	a ^= 1

TestIncDec():
	a: int = 10
	// Increment and decrement
	inc a
	dec a
	inc a
	dec a

TestReturn() -> int:
	return 42

TestReturnVoid():
	return

TestDefer():
	a: int = 0
	defer:
		a = 1
	a = 2

TestClaim():
	a := 10
	claim a > 0
	claim a = 10

TestBreakInLoop():
	x := 0
	while true:
		if x = 5:
			break
		inc x

TestMemberAccess():
	s: TestStruct
	a := s.x
	b := s.y

TestSubscript(arr: []int):
	a := arr[0]
	b := arr[1]

TestArrayProperties(arr: []int):
	n := arr.length
	c := arr.count
	b := arr.begin
	d := arr.data
	e := arr.end

TestMultipleDefers():
	a: int = 0
	defer:
		a = 1
	defer:
		a = 2
	a = 3

TestNestedScopes():
	a: int = 0
	if true:
		b: int = 1
		if true:
			c: int = 2

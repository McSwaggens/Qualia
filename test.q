FooBar():
	a := 1

	if 123:
		b := 2
	else:
		c := 3
	then if 456:
		c := 4

// CopyMemory(dest: *byte, src: *byte, size: uint):
// 	for i := 0, i < size:
// 		dest[i] = src[i]

// WhileLoop(n : int):
	// while n: n = 111

	// if n = -1: if n: n = 222; n = 333;

	// while n: n = 444 then: n = 555
	// while n: n = 666 else: n = 777 then: n = 888

// VerboseForLoops(n : int) -> int:
	// for i := 0, i < n, 1:
	// for i := 0, i < n:
	// for i : int, i < n:
	// return 0

// Foo(begin : *int, end : *int) -> int:
	// for p := begin, p < end, 1:
	// for p := begin, p < end:
	// for p := begin, p < end:

	// for p in [begin .. end], 1:
	// for p in [begin .. end]:
	// for p in [begin .. 10], 1:
	// for p in [begin .. 10]:

	// return 0

// Foo(nums : []int) -> int:
// 	for n in nums:
// 	for nums:
// 	for x: 

// FooBar():
// 	n : int = 42
// 	m : int

// 	if n = 1:
// 		m = 111
// 	else if n != 2:
// 		m = 222
// 	else if n < 3:
// 		m = 333
// 	else if n <= 4:
// 		m = 444
// 	else if n > 5:
// 		m = 555
// 	else if n >= 6:
// 		m = 666
// 	else:
// 		m = 777

// Variable_Declarations_And_Assignment():
// 	a := 0
// 	b := 1
// 	c := 2
// 	j := a + b - c

// Foo(a: int, b: int, c: int, d: int, e: int) -> int:
// 	x := a + b * c / d MOD e
// 	return x

// LogicalAnd(a: int, b: int, c: int, d: int) -> bool:
// 	return a < 1 and b > 2 and c != 3 and d = 4

// LogicalOr(a: int, b: int, c: int, d: int) -> bool:
// 	return a < 1 or b > 2 or c != 3 or d = 4

// UnaryPlus(n: int) -> int:
// 	return +n

// AllPathsReturnTest() -> int:
// 	if true:
// 	else: x := 123
// 	then if 1: return 1
// 	else: return 1


// Increment_Decrement_Integer():
// 	n : int = 0
// 	inc n
// 	dec n

// Increment_Decrement_Float():
// 	n : float32 = 0
// 	inc n
// 	dec n

// Increment_Decrement_Pointer():
// 	n : *int = null
// 	inc n
// 	dec n


// ArithmeticAssignment_Int():
// 	n : int = 0
// 	n += 1
// 	n -= 2
// 	n *= 3
// 	n /= 4

// ArithmeticAssignment_Float():
// 	f : float32 = 1
// 	f += 1
// 	f -= 2
// 	f *= 3
// 	f /= 4

// ArithmeticAssignment_Pointer():
// 	p : *int = null
// 	p += 1
// 	p -= 2


// enum Phonetic:
// 	Alpha   = 0
// 	Bravo   = 1
// 	Charlie = 2
// 	Delta   = 3
// 	Echo    = 4
// 	Foxtrot = 5

// Fibonacci(n : int) -> int:
// 	a := 0
// 	b := 1

// 	while n:
// 		c := a
// 		a = b
// 		b = c
// 		dec n

// 	return b

// Fibonacci(n : int) -> int:
// 	a := 0
// 	b := 1

// 	while n:
// 		(a, b) = (b, a + b)
// 		dec n

// 	return b

// TupleAssignment():
// 	a : (int, int)
// 	b : int
// 	c : int
// 	d : (int, (int, int))
// 	e : *int
// 	f : *int

// 	(a, (b), c, d) = ((1, 2), 3, (4), (5, (6, 7)))
// 	(*e, (*f)) = (8, 9)

// SwapPointers(a : *int, b : *int):
// 	(*a, *b) = (*b, *a)

// TupleAssignmentWithConstants():
// 	a : int
// 	b : int
// 	(a, b) = (1, 2)

// TupleAssignmentIfElse():
// 	a : int
// 	b : int
// 	(a, b) = (a, b) if a <= b else (b, a)

// TupleSwap():
// 	a : int
// 	b : int
// 	(a, b) = (b, a)

// TupleAssignmentToTuplePointer(t : *(int, int)):
// 	a : int
// 	b : int
// 	(a, b) = *t

// TupleReferenceToTupleReference(a : *(int, int), b : *(int, int)):
// 	*a = *b

// struct Alpha:
// 	a : int
// 	b : int

// StructMemberAccess():
// 	alpha : *Alpha
// 	alpha.a = 123
// 	alpha.b = 456

// AndExpression(a : int, b : int):
// 	if a and b:
// 		inc a
// 		inc b

// OrExpression(a : int, b : int):
// 	if a or b:
// 		inc a
// 		inc b

// enum Foo:
// 	A = 0
// 	B = 1

// Enums():
// 	foo : Foo
// 	foo = Foo.A

// 	bar : Foo
// 	bar = Foo.B

// 	car : Foo

// 	if foo = bar:
// 		car = 0 as Foo

// PointerPlusPointer(a:  *int, b: *int): a+b
// PointerMinusPointer(a: *int, b: *int): a-b

// PointerPlusInt(a:  *int, b: int): a+b
// PointerMinusInt(a: *int, b: int): a-b

// CallA():
// 	CallB()

// CallB():
// 	CallA()


// BranchBlocks(a : int, b : int, p : *int, q : *bool):
// 	if a < b:
// 		*p = 1
// 	else if a > b:
// 		*p = 2
// 	else:
// 		*q = false
// 	then:
// 		*q = true


// Foo(a : int, b : int) -> int:
// 	return a + b

// Bar():
// 	1.Foo(2)


// ImplicitTypeCasting():
// 	p : *int
// 	i : int

// 	j := p + true
// 	k := i + true

// AllocateMemory(size: uint) -> *byte:
// 	return null

// PadLeft(str: []int8, c: int8, count: int) -> []int8:
// 	len := str.count+count
// 	p : *int8 = AllocateMemory(len)

// 	for i := 0, i < count:
// 		p[i] = c

// 	CopyMemory(p+count, str.data, str.length)
// 	return [p .. len]

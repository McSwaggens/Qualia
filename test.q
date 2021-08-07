enum Phonetic:
	Alpha   = 0
	Bravo   = 1
	Charlie = 2
	Delta   = 3
	Echo    = 4
	Foxtrot = 5

Fibonacci(n : int) -> int:
	a := 0
	b := 1

	while n:
		(a, b) = (b, a + b)
		dec n

	return b

TupleAssignment():
	a : (int, int)
	b : int
	c : int
	d : (int, (int, int))
	e : *int
	f : *int

	(a, (b), c, d) = ((1, 2), 3, (4), (5, (6, 7)))
	(*e, (*f)) = (8, 9)

SwapPointers(a : *int, b : *int):
	(*a, *b) = (*b, *a)

ArithmeticAssignment_Pointer():
	p : *int
	p += 1
	p -= 2

ArithmeticAssignment_Int():
	n : int
	n += 1
	n -= 2
	n *= 3
	n /= 4

ArithmeticAssignment_Float():
	f : float32
	f += 1
	f -= 2
	f *= 3
	f /= 4

TupleAssignmentWithConstants():
	a : int
	b : int
	(a, b) = (1, 2)

TupleAssignmentIfElse():
	a : int
	b : int
	(a, b) = (a, b) if a <= b else (b, a)

TupleSwap():
	a : int
	b : int
	(a, b) = (b, a)

TupleAssignmentToTuplePointer(t : *(int, int)):
	a : int
	b : int
	(a, b) = *t

TupleReferenceToTupleReference(a : *(int, int), b : *(int, int)):
	*a = *b

struct Alpha:
	a : int
	b : int

StructMemberAccess():
	alpha : *Alpha
	alpha.a = 123
	alpha.b = 456

AndExpression(a : int, b : int):
	if a and b:
		inc a
		inc b

OrExpression(a : int, b : int):
	if a or b:
		inc a
		inc b

enum Foo:
	A = 0
	B = 1

Enums():
	foo : Foo
	foo = Foo.A

	bar : Foo
	bar = Foo.B

	car : Foo

	if foo = bar:
		car = 0 as Foo

PointerPlusPointer(a:  *int, b: *int): a+b
PointerMinusPointer(a: *int, b: *int): a-b

PointerPlusInt(a:  *int, b: int): a+b
PointerMinusInt(a: *int, b: int): a-b

CallA():
	CallB()

CallB():
	CallA()


BranchBlocks(a : int, b : int, p : *int, q : *bool):
	if a < b:
		*p = 1
	else if a > b:
		*p = 2
	else:
		*q = false
	then:
		*q = true


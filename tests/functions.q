// Function syntax tests

// No params, no return
NoParams():
	a := 0

// Single param
SingleParam(x: int):
	a := x

// Multiple params
MultipleParams(x: int, y: int, z: int):
	a := x

// With return type
WithReturn(x: int) -> int:
	return x

// Multiple params with return type
AddTwo(a: int, b: int) -> int:
	return a + b

// Various parameter types
VariousParamTypes(a: int8, b: uint32, c: float64, d: bool, e: byte):
	x := a

// Pointer parameter
PointerParam(p: *int):
	x := p

// Optional parameter type
OptionalParam(p: ?int):
	x := p

// Array parameter type
ArrayParam(a: []int):
	x := a

// Fixed array parameter type
FixedArrayParam(a: [4]int):
	x := a

// Function with nested function
OuterFunction():
	InnerFunction(x: int) -> int:
		return x + 1
	a := InnerFunction(5)

// Function returning pointer
ReturnPointer(x: *int) -> *int:
	return x

// Function returning optional
ReturnOptional(x: ?int) -> ?int:
	return x

// Multiple nested functions
MultiNested():
	First() -> int:
		return 1
	Second() -> int:
		return 2
	a := First()
	b := Second()

// Nested function calls (single-arg)
TestNestedCalls():
	Double(x: int) -> int:
		return x + x
	a := Double(Double(3))

// Function call in larger expression
TestCallInExpression():
	Square(x: int) -> int:
		return x * x
	a := Square(5) + 10
	b := Square(3) * Square(2)
	c := 100 - Square(4)

// Multi-arg function calls
TestMultiArgCall():
	result := AddTwo(10, 20)

// Nested multi-arg calls
TestNestedMultiArgCalls():
	a := AddTwo(AddTwo(1, 2), 3)
	b := AddTwo(AddTwo(1, 2), AddTwo(3, 4))

// Multi-arg call in larger expression
TestMultiArgCallInExpression():
	a := AddTwo(2, 3) + 10
	b := AddTwo(1, 2) * AddTwo(3, 4)
	c := 100 - AddTwo(10, 20)

// Three-param function call
AddThree(a: int, b: int, c: int) -> int:
	return a + b + c

TestThreeArgCall():
	a := AddThree(1, 2, 3)
	b := AddThree(AddTwo(1, 2), 3, 4)

// Call with boolean argument
PassBool(x: bool) -> bool:
	return x

TestBoolCall():
	a := PassBool(true)
	b := PassBool(false)

// Call with pointer argument
TestPointerCall():
	x: int = 42
	p := &x
	q := ReturnPointer(p)

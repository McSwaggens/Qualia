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

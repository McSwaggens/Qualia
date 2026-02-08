// Comprehensive pointer and reference tests

// Basic pointer read
TestBasicPointerRead():
	x: int = 42
	ptr := &x
	y := *ptr
	z := *ptr + 10

// Modifying through pointer
TestPointerWrite():
	x: int = 42
	ptr := &x
	*ptr = 100
	y := x

// Multiple modifications through pointer
TestMultiplePointerWrites():
	x: int = 0
	ptr := &x
	*ptr = 10
	*ptr = 20
	*ptr = 30

// Pointer arithmetic in assignment
TestPointerArithmeticWrite():
	x: int = 10
	ptr := &x
	*ptr = *ptr + 5
	*ptr = *ptr * 2

// Function modifying through pointer parameter
ModifyThroughPointer(ptr: *int):
	*ptr = 999

TestFunctionModifyPointer():
	x: int = 42
	ModifyThroughPointer(&x)
	y := x

// Function with multiple pointer parameters
Swap(a: *int, b: *int):
	temp := *a
	*a = *b
	*b = temp

TestSwapFunction():
	x: int = 10
	y: int = 20
	Swap(&x, &y)

// Increment through pointer
Increment(ptr: *int):
	*ptr = *ptr + 1

TestIncrementPointer():
	x: int = 5
	Increment(&x)
	Increment(&x)
	Increment(&x)

// Multiple pointers to same variable
TestMultiplePointersToSame():
	x: int = 100
	ptr1 := &x
	ptr2 := &x
	*ptr1 = 200
	y := *ptr2

// Pointer reassignment
TestPointerReassignment():
	x: int = 10
	y: int = 20
	ptr := &x
	a := *ptr
	ptr = &y
	b := *ptr

// Modifying through reassigned pointer
TestModifyReassignedPointer():
	x: int = 10
	y: int = 20
	ptr := &x
	*ptr = 15
	ptr = &y
	*ptr = 25

// Null pointer
TestNullPointer():
	ptr: *int = null as *int
	is_null := ptr = (null as *int)

// Pointer comparison
TestPointerComparison():
	x: int = 42
	y: int = 100
	ptr1 := &x
	ptr2 := &x
	ptr3 := &y
	same := ptr1 = ptr2
	different := ptr1 = ptr3

// Pointer to different types
TestDifferentPointerTypes():
	a: int8 = 10
	b: int32 = 20
	c: int64 = 30
	d: uint64 = 40
	ptr_a := &a
	ptr_b := &b
	ptr_c := &c
	ptr_d := &d
	*ptr_a = 11
	*ptr_b = 22
	*ptr_c = 33
	*ptr_d = 44

// Pointer to bool
TestPointerToBool():
	flag: bool = false
	ptr := &flag
	*ptr = true
	result := *ptr

// Pointer to float
TestPointerToFloat():
	x: float64 = 3.14
	ptr := &x
	*ptr = 2.718
	y := *ptr

// Double indirection
TestDoubleIndirection():
	x: int = 42
	ptr := &x
	ptr_ptr := &ptr
	y := **ptr_ptr

// Modifying through double indirection
TestModifyDoubleIndirection():
	x: int = 10
	ptr := &x
	ptr_ptr := &ptr
	**ptr_ptr = 100

// Function returning modified pointer
GetAndIncrement(ptr: *int) -> *int:
	*ptr = *ptr + 1
	return ptr

TestFunctionReturnModifiedPointer():
	x: int = 5
	ptr := GetAndIncrement(&x)
	y := *ptr

// Pointer in conditional
TestPointerConditional():
	x: int = 10
	y: int = 20
	condition: bool = true
	ptr := &x if condition else &y
	val := *ptr

// Array element pointer
TestArrayElementPointer():
	arr := {10, 20, 30, 40, 50}
	ptr := &arr[2]
	*ptr = 99
	val := arr[2]

// Pointer in loop
TestPointerInLoop():
	x: int = 0
	ptr := &x
	i: int = 0
	while i < 10:
		*ptr = *ptr + 1
		i = i + 1

// Accumulator function with pointer
Accumulate(sum: *int, value: int):
	*sum = *sum + value

TestAccumulator():
	total: int = 0
	Accumulate(&total, 10)
	Accumulate(&total, 20)
	Accumulate(&total, 30)

// Pointer validity check
TestPointerValidity():
	x: int = 42
	ptr := &x
	is_not_null := ptr != (null as *int)

// Comparing pointer to null
TestComparePointerToNull():
	x: int = 42
	ptr1: *int = &x
	ptr2: *int = null as *int

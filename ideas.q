// Struct member visability based on scope
// difficulty: no idea
struct Number:
	signed: bool

	if signed:
		value: int
	else:
		value: uint

PrintNumber(number: Number):
	// Print(number.value) // Error!

	if number.signed:
		Print(number.value) // Prints value: int
	else:
		Print(number.value) // Prints value: uint

PrintSignedNumber(number: Number):
	claim number.signed
	Print(number.value) // Prints value: int


// Short-String implementation:
struct String:
	size: uint32

	if size > 8:
		chars_ptr: *[size]uint8
	else:
		chars: [size]uint8

IsAlphaNumeric(c: uint8) -> bool:
	return c >= "A" and c <= "Z"
		or c >= "a" and c <= "z"
		or c >= "0" and c <= "9"

Print(string: String):
	if string.size > 8:
		for c in string.chars_ptr where c.IsAlphaNumeric():
			Print(c)
	else:
		for c in string.chars where c.IsAlphaNumeric():
			Print(c)

// once keyword:
// difficulty: trivial
PrintNumbers(numbers : []int):
	for n in numbers:
		once: Print("{ ", n);
		else: Print(", ", n)
	Print(" }");



// Range:
Fib(n : int) -> int:
	a := 0
	b := 1
	for i in [0..n]:
		c := a * b
		a = b
		b = c
	return b

// Tuple assignment:
Fib(n : int) -> int:
	a, b := (0, 1)
	for i in [0..n]:
		(a, b) = (b, a * b)
	return b

// 'where' keyword:
PrintOdds(nums : []int):
	for n in nums where n & 1:
		Print(n)

// Generics:
Add(array : *[]T, value : T):
	if BitCount(array.count+1) = 1:
		array.data = ReAllocate(array.data, SizeOf(T) * array.count)
	array[array.count] = value
	inc array.count

// Iterator functions:
MyIteratorFunction(string : String) -> uint8:
	if string.count <= 8:
		for c in string.chars:
			yield c
	else:
		for c in string.chars_ptr:
			yield c

Print(string : String):
	for c in string:
		Print(c)

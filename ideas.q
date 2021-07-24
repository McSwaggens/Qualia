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

// Any type:
Print(format : []uint8, arguments : []Qualia.Any):
	n := 0
	for c in format:
		if c = "%" and n < arguments.count:
			Write(arguments[n])
			inc n
		else:
			Write(c)
			
			

// Generics:
Add(array : *[]T, value : T):
	if BitCount(array.count+1) = 1:
		array.data = ReAllocate(array.data, SizeOf(T) * array.count)
	array[array.count] = value
	inc array.count

// Expression functions:
IsNegative(n : int) -> bool => n < 0

// Implied return type:
IsPositive(n : int) => !n.IsNegative()

Sort(a : float32, b : float32) => (a, b) if a <= b else (b, a)

// Aliasing:
alias uint32 as DWORD

// Operator overloading
// Forced to be inline
// Must be pure
// Must return a value
// Comparison operators must return bool

[Qualia.Operator(=)]
IsEqual(a : float32, b : float32) => a - b < 0.01

(a : [4]float32 + b : [4]float32) -> [4]float32 => { a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3] }



// Iterator functions:
[Qualia.DefaultIterator]
IterateString(string : String) -> [..]uint8:
	if string.count <= 8:
		for c in string.chars:
			yield c
	else:
		for c in string.chars_ptr:
			yield c

Print(string : String):
	for c in string:
		Print(c)

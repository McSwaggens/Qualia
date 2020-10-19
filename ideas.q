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


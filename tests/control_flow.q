// Control flow syntax tests

TestIf():
	x := 10

	// Simple if
	if x = 10:
		y := 1

	// If-else
	if x = 10:
		y := 1
	else:
		y := 2

	// If-else-then
	if x = 10:
		y := 1
	else:
		y := 2
	then:
		z := 3

	// If-then (no else)
	if x = 10:
		y := 1
	then:
		z := 2

	// Nested if
	if x = 10:
		if x > 5:
			y := 1
		else:
			y := 2

TestWhile():
	x := 10

	// Simple while
	while x > 0:
		dec x

	// While with else
	while x > 0:
		dec x
	else:
		y := 0

	// While with then
	while x > 0:
		dec x
	then:
		y := 0

	// While with else and then
	while x > 0:
		dec x
	else:
		y := 0
	then:
		z := 0

	// Nested while
	while x > 0:
		y := 10
		while y > 0:
			dec y
		dec x

// BUG: ScanRangeFor never sets the iterator variable's type from the range element type,
// so `x := item` crashes during semantic analysis
TestForRange(arr: []int):
	// Simple for-in
	for item in arr:
		x := item

	// For-in with where filter
	for item in arr where item > 0:
		x := item

TestForVerbose():
	// Verbose for loop with explicit type
	for i: int = 0, i < 10, i + 1:
		x := i

	// Verbose for with inferred type
	for j: = 0, j < 10, j + 1:
		x := j

	// Verbose for with else
	for i: int = 0, i < 10, i + 1:
		x := i
	else:
		y := 0

	// Verbose for with then
	for i: int = 0, i < 10, i + 1:
		x := i
	then:
		y := 0

TestBreak():
	x := 10
	while x > 0:
		if x = 5:
			break
		dec x

TestNestedLoops():
	for i: int = 0, i < 5, i + 1:
		for j: int = 0, j < 5, j + 1:
			x := i + j

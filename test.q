

enum Color:
	Red   = 0 + 0
	Green = 1 << 0
	Blue  = 2^1

struct Foo:
	a : int
	b : int
	c : int
	d : int

Fib(n : int) -> int:
	a := 0
	b := 1

	while n:
		dec n
		c := a + b
		a = b
		b = c

	return b



ProgramStart():
	while true:


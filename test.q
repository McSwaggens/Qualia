
Foo(p: *int):
	*p = 1337

Add(a: *int, b: *int) -> int:
	return *a + *b

Test():
	n : int
	Foo(&n)
	n + 0

	n = 1
	j := 2
	k := Add(&n, &j)
	k + 0


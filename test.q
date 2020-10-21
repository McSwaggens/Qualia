
struct Foo:
	a : int
	b : int
	c : int
	d : int


A() -> int:
	foo : Foo
	foo.a = 1
	foo.b = 2
	foo.c = 3
	foo.d = 4
	return foo.a + foo.b + foo.c + foo.d

Test():
	n := A()

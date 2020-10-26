
struct Foo:
	a : int
	b : Bar
	c : int

struct Bar:
	c : int

A() -> int:
	foo : Foo
	bar : Bar
	foo.a = 1
	foo.c = 2
	bar.c = 3
	foo.b.c = 4
	foo.b = bar
	return foo.a + bar.c + foo.b.c + foo.c

Test():
	n := A()

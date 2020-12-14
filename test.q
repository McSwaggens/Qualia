
Test():
	struct Foo:
		a : int
		b : int

	struct Bar:
		c : *uint8
		d : float32
		e : *Bar

	foo0 : Foo
	foo1 : Foo

	bar0 : Bar
	bar1 : Bar

	bar0.d = 123.456

	foo1 = foo0
	bar1 = bar0

	foo0.a = bar0.d

	bar0.d + 0
	foo0.a + 0

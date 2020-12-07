Test():

	struct Foo:
		x : int
		y : int

	struct Bar:
		a : int
		b : Foo

	bar0 : Bar
	bar1 : Bar
	bar2 : Bar

	bar0.a   = 123
	bar0.b.x = 456
	bar0.b.y = 789

	bar1.a   = 123
	bar1.b.x = 456
	bar1.b.y = 789

	bar2.a   = 42
	bar2.b.x = 42
	bar2.b.y = 42

	b0_1 := bar0 = bar1
	b0_2 := bar0 = bar2

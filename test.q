Test():

	struct Foo:
		a : int
		b : int

	foo : Foo

	p0 := &foo
	p1 := &p0

	foo.a = 1
	foo.b = 2

	(&foo).a = 123
	(&foo).a+0

	p1.b = 456
	p1.a+p0.b

	(p0+0).a = 789
	(p0+0).a+0

	Bar(f : **Foo) -> Foo:
		return **f

	Bar(p1).a+0

	a : [256]Foo
	i := 0
	while i < a.length:
		a[i].a = a.length - i - 1
		inc i

	d : []Foo
	d.data = &a[0]
	d.length = a.length

	i = 0
	while i < d.length:
		d[i].a+0
		inc i


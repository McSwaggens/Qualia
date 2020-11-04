
Foo(): return

Test():
	n := -10
	j := -20
	b := true

	defer:
		n+j

	while true:
		defer: j = 666+0
		inc n
		b = n < 10

		if not b:
			break
	then: Foo()

	Foo()

	p : *int = null

	if p = null:
		p = &n
		*p = 456
	else:
		*p = 123


struct N0: v : N1
struct N1: v : N2
struct N2: v : N3
struct N3: v : N4
struct N4: v : int; q : int

Add(a: int, b: int) -> N0:
	num : N0
	num.v.v.v.v.q = a + b
	return num

Test():
	n := Add(1, 2).v.v.v.v.q + 0

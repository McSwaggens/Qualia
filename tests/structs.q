// Struct syntax tests

// Single member struct
struct Single:
	x: int

// Multiple members
struct Point:
	x: int
	y: int

// Various primitive member types
struct AllPrimitives:
	a: byte
	b: bool
	c: int8
	d: int16
	e: int32
	f: int64
	g: uint8
	h: uint16
	i: uint32
	j: uint64
	k: float32
	l: float64

// Pointer members
struct WithPointers:
	p: *int
	q: **int

// Optional members
struct WithOptionals:
	a: ?int
	b: ?*int

// Array members
struct WithArrays:
	a: []int
	b: [8]int

// Complex specifiers
struct Complex:
	a: *[]int
	b: [4]*int
	c: ?*int

// Nested struct reference
struct Rect:
	origin: Point
	size: Point

// Tuple member
struct WithTuple:
	pair: (int, int)

// Function pointer member
// BUG: CalculateStructSize crashes for structs with function pointer type members
struct WithFnPtr:
	callback: (int) -> int

// Type annotation tests

TestTypes():
	v0  : byte
	v1  : int
	v2  : int8
	v3  : int16
	v4  : int32
	v5  : int64
	v6  : uint
	v7  : uint8
	v8  : uint16
	v9  : uint32
	v10 : uint64
	v11 : float32
	v12 : float64
	v13 : bool

	v14 : *int
	v15 : ?int
	v16 : []int
	v17 : [42]int

	v18 : ********************int
	v19 : ????????????????????int
	v20 : [][][][][][][][][][]int
	v21 : [42][42][42][42][42]int    // BUG: GetTypeSize crashes for deeply nested fixed arrays
	v22 : *?**[][42][42]*?****int   // BUG: same GetTypeSize crash due to nested fixed array specifiers

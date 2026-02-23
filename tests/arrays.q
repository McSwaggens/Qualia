TestArrayFields(arr: []int):
	n := arr.length
	c := arr.count
	b := arr.begin
	d := arr.data
	e := arr.end

TestArraySubscript(arr: []int):
	a := arr[0]
	b := arr[1]

TestArrayRange(p: *int, count: uint64):
	arr := [p..count]
	n := arr.length

TestArrayRangeFromPtrs(begin: *int, end: *int):
	arr := [begin..end]
	n := arr.length

TestFixedArray():
	arr: [5]int
	a := arr[0]
	n := arr.length
	c := arr.count
	b := arr.begin
	d := arr.data
	e := arr.end

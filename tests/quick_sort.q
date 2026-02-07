// Quick sort (iterative): Lomuto partition with explicit stack
// O(n log n) average time, O(1) extra space (bounded stack)

QuickSort(arr: [10]int):
	n := 10
	// Stack to store sub-range bounds (low, high pairs)
	stack: [20]int
	top := -1

	// Push initial range
	inc top
	stack[top] = 0
	inc top
	stack[top] = n - 1

	while top >= 0:
		// Pop high and low
		high := stack[top]
		dec top
		low := stack[top]
		dec top

		// Lomuto partition: pivot = arr[high]
		pivot := arr[high]
		i := low - 1
		for j: int = low, j < high, j + 1:
			if arr[j] <= pivot:
				inc i
				tmp := arr[i]
				arr[i] = arr[j]
				arr[j] = tmp
		// Place pivot in correct position
		inc i
		tmp := arr[i]
		arr[i] = arr[high]
		arr[high] = tmp
		// i is now the pivot index

		// Push left sub-array if it has more than one element
		if i - 1 > low:
			inc top
			stack[top] = low
			inc top
			stack[top] = i - 1

		// Push right sub-array if it has more than one element
		if i + 1 < high:
			inc top
			stack[top] = i + 1
			inc top
			stack[top] = high

TestQuickSort():
	arr := {9, 4, 7, 2, 8, 1, 5, 3, 6, 0}
	QuickSort(arr)

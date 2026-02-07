// Merge sort (bottom-up iterative): merge adjacent pairs, doubling width each pass
// O(n log n) time, O(n) space

MergeSort(arr: [10]int):
	n := 10
	tmp: [10]int

	width := 1
	while width < n:
		left := 0
		while left < n:
			// Compute mid and right boundaries
			mid := left + width
			right := left + width + width

			// Clamp to array bounds
			if mid > n:
				mid = n
			if right > n:
				right = n

			// Merge arr[left..mid) and arr[mid..right) into tmp
			i := left
			j := mid
			k := left

			// Merge while both halves have elements
			while i < mid:
				if j >= right:
					break
				if arr[i] <= arr[j]:
					tmp[k] = arr[i]
					inc i
				else:
					tmp[k] = arr[j]
					inc j
				inc k

			// Copy remaining from left half
			while i < mid:
				tmp[k] = arr[i]
				inc i
				inc k

			// Copy remaining from right half
			while j < right:
				tmp[k] = arr[j]
				inc j
				inc k

			// Copy merged result back to arr
			for c: int = left, c < right, c + 1:
				arr[c] = tmp[c]

			left = left + width + width
		width = width + width

TestMergeSort():
	arr := {9, 4, 7, 2, 8, 1, 5, 3, 6, 0}
	MergeSort(arr)

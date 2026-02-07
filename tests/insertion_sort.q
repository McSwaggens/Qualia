// Insertion sort: shift each element left into correct position in sorted prefix
// O(n^2) time, O(1) space

InsertionSort(arr: [10]int):
	n := 10
	for i: int = 1, i < n, i + 1:
		key := arr[i]
		j := i - 1
		while j >= 0:
			if arr[j] > key:
				arr[j + 1] = arr[j]
				dec j
			else:
				break
		arr[j + 1] = key

TestInsertionSort():
	arr := {9, 4, 7, 2, 8, 1, 5, 3, 6, 0}
	InsertionSort(arr)

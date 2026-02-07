// Selection sort: find minimum in unsorted portion, swap into position
// O(n^2) time, O(1) space

SelectionSort(arr: [10]int):
	n := 10
	for i: int = 0, i < n - 1, i + 1:
		min_idx := i
		for j: int = i + 1, j < n, j + 1:
			if arr[j] < arr[min_idx]:
				min_idx = j
		tmp := arr[i]
		arr[i] = arr[min_idx]
		arr[min_idx] = tmp

TestSelectionSort():
	arr := {9, 4, 7, 2, 8, 1, 5, 3, 6, 0}
	SelectionSort(arr)

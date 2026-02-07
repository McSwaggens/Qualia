// Bubble sort: repeatedly swap adjacent elements if out of order
// O(n^2) time, O(1) space

BubbleSort(arr: [10]int):
	n := 10
	for i: int = 0, i < n - 1, i + 1:
		for j: int = 0, j < n - 1 - i, j + 1:
			if arr[j] > arr[j + 1]:
				tmp := arr[j]
				arr[j] = arr[j + 1]
				arr[j + 1] = tmp

TestBubbleSort():
	arr := {9, 4, 7, 2, 8, 1, 5, 3, 6, 0}
	BubbleSort(arr)

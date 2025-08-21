#include "general.h"


template<typename T>
static T* Partition(T* begin, T* end, T value) {
	// Print("Partition(%, %)\n", Array(begin, end-begin), value);
	end++;
	begin--;
	while (true) {
		while (*++begin < value);
		while (*--end   > value);
		// Print("begin = %, end = %\n", *begin, *end);
		if (begin >= end) break;
		Swap(begin, end);
	}

	return end;
}

template<typename T>
static void QuickSort(T* begin, T* end) {
	if (begin >= end) return;
	u64 length = end - begin;
	// Print("QuickSort(%)\n", Array(begin, end-begin));

	T* div = Partition(begin, end, begin[length/2]);
	QuickSort(begin, div);
	QuickSort(div + 1, end);
}

template<typename T>
static bool IsSorted(T* begin, T* end) {
	for (T* p = begin; p < end-1; p++)
		if (p[0] > p[1])
			return false;
	return true;
}



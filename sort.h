#include "general.h"
#include "array.h"
#include "memory.h"
#include "concepts.h"
#include "print.h"

// -------------------------------------------------- //

template<typename T>
static T* Partition(T* begin, T* end, T value) {
	while (true) {
		while (*begin < value) begin++;
		while (*end   > value) end--;
		if (begin >= end) break;
		Swap(*begin, *end);
	}

	return end;
}

template<typename T>
static void QuickSort(T* begin, T* end) {
	if (begin >= end) return;
	u64 length = end - begin;

	T* div = Partition(begin, end, begin[length/2]);
	QuickSort(begin, div);
	QuickSort(div + 1, end);
}

template<typename T>
static void QuickSort(Array<T> array) {
	QuickSort(array.Begin(), array.End());
}

// -------------------------------------------------- //

template<typename T>
static void RadixSortGenerateIndices(Array<T> numbers) {
	u64 n = 0;
	for (u64 i = 0; i < numbers.length; i++) {
		u64 old = numbers[i];
		numbers[i] = n;
		n += old;
	}
}

template<typename T>
static void RadixSort(Array<T> array) {
	u64 rounds = sizeof(T);
	u64 counters[256];

	T* a = array.Begin();
	T* b = Alloc<T>(array.length);

	for (u64 r = 0; r < rounds; r++) {
		Zero(counters, 256);

		for (u64 i = 0; i < array.length; i++)
			counters[((u8*)(a+i))[r]]++;

		RadixSortGenerateIndices(Array<u64>(counters, 256));

		for (u64 i = 0; i < array.length; i++)
			b[counters[((u8*)(a+i))[r]]++] = a[i];

		Swap(a, b);
	}

	if (rounds & 1) {
		Copy(b, a, array.length);
		Swap(a, b);
	}

	Free(b, array.length);
}

// -------------------------------------------------- //

template<typename T>
static bool IsSorted(T* begin, T* end) {
	for (T* p = begin; p < end-1; p++)
		if (p[0] > p[1])
			return false;
	return true;
}

template<typename T>
static T* BinarySearch(Array<T> array, T value) {
	if (array.length < 2)
		return array.Begin();

	T* middle = array + (array.length / 2);

	if (value < middle) return BinarySearch(Array<T>(array.Begin(), (array.length / 2)), value);
	if (value > middle) return BinarySearch(Array<T>(middle, array.End() - middle), value);

	return middle;
}


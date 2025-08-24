#include "list.h"
#include "sort.h"

template<typename T>
struct Set {
	List<T> elements;

	constexpr Set() = default;

	constexpr Set(List<T> list) {
		elements = list.Copy();
		QuickSort(elements.ToArray());
	}

	constexpr       T* Begin()       { return elements.Begin(); }
	constexpr const T* Begin() const { return elements.Begin(); }
	constexpr       T* begin()       { return elements.Begin(); }
	constexpr const T* begin() const { return elements.Begin(); }

	constexpr       T* End()       { return elements.End(); }
	constexpr const T* End() const { return elements.End(); }
	constexpr       T* end()       { return elements.End(); }
	constexpr const T* end() const { return elements.End(); }

	constexpr operator bool() { return elements; }
	constexpr u32  Count()   { return elements.count; }
	constexpr bool IsEmpty() { return elements.IsEmpty(); }

	constexpr bool operator ==(Set<T> o) { return elements == o.elements; }
	constexpr bool operator !=(Set<T> o) { return !(*this == o); }

	constexpr T& Last()  { elements.Last();  }
	constexpr T& First() { elements.First(); }

	u32 GetBinaryIndex(T item) {
		return elements.End() - BinarySearch(elements.ToArray(), item);
	}

	u32 GetBinaryPtr(T item) {
		return BinarySearch(elements.ToArray(), item);
	}

	void Add(T item) {
		u32 index = GetBinaryIndex(item);

		if (index == Count())
			elements.Add(item);

		elements.Insert(item, index);
	}
};


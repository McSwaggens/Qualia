#pragma once

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

	constexpr Set(const Set<T>& set) {
		elements = set.elements.Copy();
	}

	constexpr auto* Begin(this auto& self) { return self.elements.Begin(); }
	constexpr auto* begin(this auto& self) { return self.elements.Begin(); }

	constexpr auto* End(this auto& self) { return self.elements.End(); }
	constexpr auto* end(this auto& self) { return self.elements.End(); }

	constexpr operator bool() { return elements; }
	constexpr u32  Count()   { return elements.count; }
	constexpr bool IsEmpty() { return elements.IsEmpty(); }

	constexpr bool operator ==(Set<T> o) { return elements == o.elements; }
	constexpr bool operator !=(Set<T> o) { return !(*this == o); }

	constexpr T& Last()  { return elements.Last();  }
	constexpr T& First() { return elements.First(); }

	u32 GetBinaryIndex(T item) {
		return BinarySearch(elements.ToArray(), item) - elements.Begin();
	}

	u32 GetBinaryPtr(T item) {
		return BinarySearch(elements.ToArray(), item);
	}

	void Add(T item) {
		u32 index = GetBinaryIndex(item);
		if (index < Count() && elements[index] == item)
			return;
		elements.Insert(item, index);
	}

	Set<T> Copy(u32 extra_capacity = 0) {
		Set<T> result;
		result.elements = elements.Copy(extra_capacity);
		return result;
	}
};


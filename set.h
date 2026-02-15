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

	bool Add(T item) {
		u32 index = GetBinaryIndex(item);
		if (index < Count() && elements[index] == item)
			return false;
		elements.Insert(item, index);
		return true;
	}

	Set<T> Copy(u32 extra_capacity = 0) {
		Set<T> result;
		result.elements = elements.Copy(extra_capacity);
		return result;
	}

	bool Contains(Set<T> other) {
		if (other.Count() > Count())
			return false;

		u32 i = 0; // Index in this set
		u32 j = 0; // Index in other set

		while (j < other.Count()) {
			if (i >= Count())
				return false; // Ran out of elements in this set

			if (elements[i] == other.elements[j]) {
				i++;
				j++;
			} else if (elements[i] < other.elements[j]) {
				i++;
			} else {
				return false; // other.elements[j] is not in this set
			}
		}

		return true;
	}

	Set<T> Intersect(Set<T> other) {
		Set<T> result;
		result.elements = AllocateList<T>(Min(Count(), other.Count()));

		u32 i = 0;
		u32 j = 0;

		while (i < Count() && j < other.Count()) {
			if (elements[i] == other.elements[j]) {
				result.elements.Add(elements[i]);
				i++;
				j++;
				continue;
			}

			if (elements[i] < other.elements[j]) {
				i++;
				continue;
			}

			j++;
		}

		return result;
	}
};

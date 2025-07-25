#pragma once

#include "memory.h"

template<typename T>
struct List {
	T*  data = null;
	u32 count = 0;
	u32 capacity = 0;

	List() = default;
	List(Null) : data(null), count(0), capacity(0) { }
	T& operator[](u32 n) { return data[n]; }
	T  operator[](u32 n) const { return data[n]; }

	constexpr T* Begin() { return data; }
	constexpr T* End()   { return data + count; }
	constexpr const T* Begin() const { return data; }
	constexpr const T* End()   const { return data + count; }

	constexpr T* begin() { return data; }
	constexpr T* end()   { return data + count; }
	constexpr const T* begin() const { return data; }
	constexpr const T* end()   const { return data + count; }

	constexpr bool operator ==(List<T> o) {
		if (count != o.count)
			return false;

		if (data == o.data)
			return true;

		for (u32 i = 0; i < count; i++)
			if (!Compare(data[i], o.data[i]))
				return false;

		return true;
	}

	constexpr bool operator !=(List<T> o) { return !(*this == o); }

	constexpr T& Last() { return data[count-1]; }

	T& Force(u32 n) {
		if (capacity <= n) {
			u32 new_capacity = NextPow2(n);
			data = ReAllocate(data, capacity, new_capacity);
			ZeroMemory(data+count, new_capacity - capacity);
			capacity = new_capacity;
			count = n;
		}
		else if (count < n) {
			count = n;
		}

		return data[n];
	}

	T* LinearFind(T value) {
		for (u32 i = 0; i < count; i++) {
			if (Compare(data+i, value)) return data + i;
		}

		return null;
	}

	void Add(T item) {
		if (count == capacity) {
			capacity = NextPow2(count+8);
			data = ReAllocate(data, count, capacity);
		}

		data[count++] = item;
	}

	bool AddIfUnique(T item) {
		for (u32 i = 0; i < count; i++) {
			if (data[i] == item) {
				return false;
			}
		}

		Add(item);
		return true;
	}

	void Add(T item, u32 amount) {
		if (count + amount >= capacity) {
			u32 old_capacity = capacity;
			capacity = (count + amount) * 2;
			data = ReAllocate(data, old_capacity, capacity);
		}

		FillMemory(data + count, amount, item);
		count += amount;
	}

	void Replace(T old_item, T new_item) {
		for (T* p = Begin(); p < End(); p++) {
			if (*p == old_item) {
				*p = new_item;
				return;
			}
		}
	}

	void ReplaceAll(T old_item, T new_item) {
		for (T* p = Begin(); p < End(); p++) {
			if (*p == old_item) {
				*p = new_item;
			}
		}
	}

	void Remove(T item) {
		T* p = Begin();
		while (p < End() && *p != item) p++;
		if (p != End()) {
			while (p+1 != End()) {
				*p = *++p;
			}

			count--;
		}
	}

	bool Contains(T value) {
		for (u32 i = 0; i < count; i++) {
			if (data[i] == value) {
				return true;
			}
		}

		return false;
	}

	void Pad(u32 amount) {
		if (count + amount >= capacity) {
			u32 old_capacity = capacity;
			capacity = count + amount;
			data = ReAllocate(data, old_capacity, capacity);
		}
	}

	void Pad(u32 amount, T value) {
		if (count + amount >= capacity) {
			u32 old_capacity = capacity;
			capacity = count + amount;
			data = ReAllocate(data, old_capacity, capacity);
		}

		FillMemory(data + count, amount, value);
	}

	void Clear() {
		count = 0;
	}

	Array<T> ToArray() {
		return Array<T>(data, count);
	}

	void Free() {
		DeAllocate(data, count);
	}
};

template<typename T>
static inline List<T> AllocateList(u32 capacity = 16) {
	List<T> list;
	list.data = Allocate<T>(capacity);
	list.count = 0;
	list.capacity = capacity;
	return list;
}

// ------------------------------------------- //

template<typename T>
static inline List<T> CopyList(List<T> src, u32 padding = 0) {
	List<T> list;
	list.count = src.count;
	list.capacity = src.count + padding;
	list.data = (T*)AllocateMemory(list.capacity * sizeof(T));
	CopyMemory(list.data, src.data, list.count);
	return list;
}

template<typename T>
static inline void AppendListToList(List<T>* list, const List<T> other) {
	if (list->count + other.count >= list->capacity) {
		list->capacity += other.count;
		list->data = ReAllocate(list->data, list->capacity);
	}

	CopyMemory(list->data + list->count, other.data, other.count);
	list->count += other.count;
}

template<typename T>
static inline void Free(List<T> list) {
	list.Free();
}


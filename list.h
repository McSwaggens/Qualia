#pragma once

#include "memory.h"
#include "assert.h"
#include "math.h"

template<typename T>
struct List {
	T*  data     = null;
	u32 count    = 0;
	u32 capacity = 0;

	constexpr List() = default;
	constexpr List(Null) : data(null), count(0), capacity(0) { }

	constexpr T& operator[](u32 n) { return data[n]; }
	constexpr T  operator[](u32 n) const { return data[n]; }

	constexpr       T* Begin()       { return data; }
	constexpr const T* Begin() const { return data; }
	constexpr       T* begin()       { return data; }
	constexpr const T* begin() const { return data; }

	constexpr       T* End()       { return data + count; }
	constexpr const T* End() const { return data + count; }
	constexpr       T* end()       { return data + count; }
	constexpr const T* end() const { return data + count; }

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

	constexpr operator bool() { return count != 0; }
	constexpr bool IsEmpty()  { return count == 0; }

	constexpr T& Last()  { Assert(!IsEmpty()); return data[count-1]; }
	constexpr T& First() { Assert(!IsEmpty()); return data[0]; }

	u32 NextCapacity(u32 n) {
		if (n == 0) return 0;
		if (n <= 8) return 8;
		return NextPow2(n);
	}

	void ChangeCapacity(u32 new_capacity) {
		new_capacity = NextCapacity(new_capacity);
		data = ReAlloc(data, capacity, new_capacity);
		capacity = new_capacity;
	}

	void AssureCapacity(u32 new_capacity) {
		if (capacity >= new_capacity)
			return;

		ChangeCapacity(new_capacity);
	}

	void Add(T item) {
		AssureCapacity(count + 1);
		data[count++] = item;
	}

	bool AddIfUnique(T item) {
		if (Contains(item))
			return false;

		Add(item);
		return true;
	}

	void Add(T item, u32 amount) {
		AssureCapacity(count + amount);
		FillMemory(End(), amount, item);
		count += amount;
	}

	void Add(List<T> items) {
		AssureCapacity(count + items.count);
		Copy(End(), items.Begin(), items.count);
		count += items.count;
	}

	void RemoveIndex(u32 index, u32 n = 1) {
		Assert(index + n <= count);

		if (index + n != count)
			Move(data + index, data + index + n, End() - (data + index + n));

		count -= n;
	}

	void Insert(T item, u32 index) {
		AssureCapacity(count + 1);
		index = Min(index, count);
		Move(data + index + 1, data + index, count - index);
		data[index] = item;
		count++;
	}

	void Insert(Array<T> items, u32 index) {
		AssureCapacity(count + items.length);
		index = Min(index, count);
		Move(data + index + items.length, data + index, count - index);
		Copy(data + index, items, items.length);
		count += items.length;
	}

	void Insert(List<T> items, u32 index) {
		Insert(items.ToArray());
	}

	void Remove(T item) {
		T* p = Begin();
		while (p < End() && *p != item) p++;
		if (p != End()) {
			while (p+1 != End())
				*p = *++p;

			count--;
		}
	}

	bool Contains(T value) {
		for (u32 i = 0; i < count; i++)
			if (data[i] == value)
				return true;

		return false;
	}

	void Clear() {
		count = 0;
	}

	Array<T> ToArray() {
		return Array<T>(data, count);
	}

	void Copy() {
		return List<T>{
			.capacity = capacity,
			.count = count,
			.data = CopyAllocate<T>(data, capacity)
		};
	}

	void Free() {
		DeAllocate(data, capacity);
	}
};

template<typename T>
static inline List<T> AllocateList(u32 capacity = 16) {
	List<T> list;
	list.AssureCapacity(capacity);
	return list;
}

template<typename T>
static inline void Free(List<T> list) {
	list.Free();
}


#pragma once

#include "memory.h"
#include "span.h"
// #include "initlist.h"

template<typename T>
struct List
{
	T*  data;
	uint32 count;
	uint32 capacity;

	constexpr List() = default;
	// constexpr List(InitList<T> initlist) : data(initlist.begin()), count(initlist.size()), capacity(0) { }
	constexpr List(Null) : data(null), count(0), capacity(0) { }
	// constexpr operator T*() { return data; }
	// constexpr operator bool() { return static_cast<bool>(data); }
	constexpr T& operator[](uint32 n) { return data[n]; }
	constexpr T  operator[](uint32 n) const { return data[n]; }

	constexpr T* Begin() { return data; }
	constexpr T* End() { return data + count; }

	constexpr T& Last() { return data[count-1]; }

	T& Force(uint32 n)
	{
		if (capacity <= n)
		{
			uint32 new_capacity = NextPow2(n+1);
			data = ReAllocate(data, capacity, new_capacity);
			ZeroMemory(data+count, new_capacity - capacity);
			capacity = new_capacity;
			count = n;
		}
		else if (count < n)
		{
			count = n;
		}

		return data[n];
	}

	T* LinearFind(T value)
	{
		for (uint32 i = 0; i < count; i++)
		{
			if (Compare(data+i, value)) return data + i;
		}

		return null;
	}

	void Add(T item)
	{
		uint32 amount = 1;
		if (count + amount >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity = (count + amount) * 2;
			data = ReAllocate(data, old_capacity, capacity);
		}

		FillMemory(data + count, amount, item);
		count += amount;
	}

	bool AddIfUnique(T item)
	{
		for (uint32 i = 0; i < count; i++)
		{
			if (data[i] == item)
			{
				return false;
			}
		}

		Add(item);
		return true;
	}

	void Add(T item, uint32 amount)
	{
		if (count + amount >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity = (count + amount) * 2;
			data = ReAllocate(data, old_capacity, capacity);
		}

		FillMemory(data + count, amount, item);
		count += amount;
	}

	void Replace(T old_item, T new_item)
	{
		for (T* p = Begin(); p < End(); p++)
		{
			if (*p == old_item)
			{
				*p = new_item;
				return;
			}
		}
	}

	void ReplaceAll(T old_item, T new_item)
	{
		for (T* p = Begin(); p < End(); p++)
		{
			if (*p == old_item)
			{
				*p = new_item;
			}
		}
	}

	void Remove(T item)
	{
		T* p = Begin();
		while (p < End() && *p != item) p++;
		if (p != End())
		{
			while (p+1 != End())
			{
				*p = *++p;
			}

			count--;
		}
	}

	bool Contains(T value)
	{
		for (uint32 i = 0; i < count; i++)
		{
			if (data[i] == value)
			{
				return true;
			}
		}

		return false;
	}

	void Pad(uint32 amount)
	{
		if (count + amount >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity = count + amount;
			data = ReAllocate(data, old_capacity, capacity);
		}
	}

	void Pad(uint32 amount, T value)
	{
		if (count + amount >= capacity)
		{
			uint32 old_capacity = capacity;
			capacity = count + amount;
			data = ReAllocate(data, old_capacity, capacity);
		}

		FillMemory(data + count, amount, value);
	}

	void Clear()
	{
		count = 0;
	}

	constexpr Array<T> ToArray()
	{
		return Array<T>(data, count);
	}
};

template<typename T>
static inline Array<T> ToArray(List<T> list)
{
	return Array<T>(list.data, list.count);
}

template<typename T>
static inline List<T> AllocateList(uint32 capacity = 16)
{
	List<T> list;
	list.data = Allocate<T>(capacity);
	list.count = 0;
	list.capacity = capacity;
	return list;
}

// ------------------------------------------- //

template<typename T>
static inline List<T> CopyList(List<T> src, uint32 padding = 0)
{
	List<T> list;
	list.count = src.count;
	list.capacity = src.count + padding;
	list.data = (T*)AllocateMemory(list.capacity * sizeof(T));
	CopyMemory(list.data, src.data, list.count);
	return list;
}

template<typename T>
static inline void AppendListToList(List<T>* list, const List<T> other)
{
	if (list->count + other.count >= list->capacity)
	{
		list->capacity += other.count;
		list->data = ReAllocate(list->data, list->capacity);
	}

	CopyMemory(list->data + list->count, other.data, other.count);
	list->count += other.count;
}

template<typename T>
static inline void FreeList(List<T> list)
{
	DeAllocate(list.data, list.capacity);
}

template<typename T>
static inline void Free(List<T> list)
{
	FreeList(list);
}


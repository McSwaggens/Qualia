#pragma once

#include "int.h"
#include "memory.h"
#include "span.h"
#include "initlist.h"

template<typename T>
struct List
{
	T*  data;
	u32 count;
	u32 capacity;

	constexpr List() = default;
	constexpr List(InitList<T> initlist) : data(initlist.begin()), count(initlist.size()), capacity(0) { }
	constexpr List(Null) : data(null), count(0), capacity(0) { }
	constexpr operator Span<T>() { return Span<T>(data, data + count); }
	constexpr operator T*() { return data; }
	constexpr operator const T*() const { return data; }
	constexpr operator bool() const { return static_cast<bool>(data); }
	constexpr T& operator[](u32 n) { return data[n]; }
	constexpr T  operator[](u32 n) const { return data[n]; }

	constexpr T* Begin() { return data; }
	constexpr T* End() { return data + count; }

	void Add(T item)
	{
		u32 amount = 1;
		if (count + amount >= capacity)
		{
			capacity = (count + amount) * 2;
			ReAllocate(data, capacity);
		}

		FillMemory(data + count, amount, item);
		count += amount;
	}

	bool AddIfUnique(T item)
	{
		for (u32 i = 0; i < count; i++)
		{
			if (data[i] == item)
			{
				return false;
			}
		}

		Add(item);
		return true;
	}

	void Add(T item, u32 amount)
	{
		if (count + amount >= capacity)
		{
			capacity = (count + amount) * 2;
			ReAllocate(data, capacity);
		}

		FillMemory(data + count, amount, item);
		count += amount;
	}

	bool Contains(T value)
	{
		for (u32 i = 0; i < count; i++)
		{
			if (data[i] == value)
			{
				return true;
			}
		}

		return false;
	}

	void Pad(u32 amount)
	{
		if (count + amount >= capacity)
		{
			capacity = count + amount;
			ReAllocate(data, capacity);
		}
	}

	void Pad(u32 amount, T value)
	{
		if (count + amount >= capacity)
		{
			capacity = count + amount;
			ReAllocate(data, capacity);
		}

		FillMemory(data + count, amount, value);
	}

	void Clear()
	{
		count = 0;
	}

	[[nodiscard]]
	constexpr Array<T> ToArray()
	{
		return Array<T>(data, count);
	}
};

template<typename T>
[[nodiscard]]
static constexpr List<T> ToArray(List<T> list)
{
	return Array<T>(list.data, list.count);
}

template<typename T>
[[nodiscard]]
static inline List<T> CreateList(u32 capacity = 16)
{
	List<T> list;
	list.data = Allocate<T>(capacity);
	list.count = 0;
	list.capacity = capacity;
	return list;
}

template<typename T>
[[nodiscard]]
static inline List<T> CopyList(List<T> src, u32 padding = 0)
{
	List<T> list;
	list.count = src.count;
	list.capacity = src.count + padding;
	list.data = Allocate<T>(list.capacity);
	CopyMemory(list.data, src.data, list.count);
	return list;
}


template<typename T>
static inline void AppendList(List<T>& target, const List<T> other)
{
	if (target.count + other.count >= target.capacity) // [[unlikely]]
	{
		target.capacity += other.count;
		ReAllocate(target.data, target.capacity);
	}

	CopyMemory(target.data + target.count, other.data, other.count);
	target.count += other.count;
}

// template<typename T>
// static inline void AppendList(List<T>& list, T item, u32 count = 1)
// {
// 	if (list.count + count >= list.capacity)
// 	{
// 		list.capacity = (list.count + count) * 2;
// 		ReAllocate(list.data, list.capacity);
// 	}
// 	FillMemory(list.data + list.count, item, count);
// 	list.count += count;
// }

template<typename T>
static inline void Free(List<T> list)
{
	Free(list.data);
}

template<typename T>
static constexpr T* begin(List<T> list)
{
	return list.data;
}

template<typename T>
static constexpr T* end(List<T> list)
{
	return list.data + list.count;
}



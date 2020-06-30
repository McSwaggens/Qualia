#pragma once

#include "int.h"

namespace std
{
	typedef unsigned long size_t;

	template<class T>
	class initializer_list {
	private:
		const T* array;
		size_t length;
		constexpr initializer_list(const T* array, size_t length) : array(array), length(length) { }
	public:
		constexpr initializer_list() noexcept : array(null), length(0) { }
		constexpr size_t size() const noexcept { return length; }
		constexpr const T* begin() const noexcept { return array; }
		constexpr const T* end() const noexcept { return begin() + size(); }
	};

	template<class T>
	constexpr const T* begin(initializer_list<T> il) noexcept
	{
		return il.begin();
	}

	template<class T>
	constexpr const T* end(initializer_list<T> il) noexcept
	{
		return il.end();
	}
}

template<typename T>
using InitList = std::initializer_list<T>;


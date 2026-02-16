#pragma once
#include "int.h"

namespace std {
	template<typename T>
	class initializer_list {
		const T* __begin;
		decltype(sizeof(0)) __size;

	public:
		using value_type = T;
		using reference = const T&;
		using const_reference = const T&;
		using size_type = decltype(sizeof(0));
		using iterator = const T*;
		using const_iterator = const T*;

		constexpr initializer_list() : __begin(nullptr), __size(0) { }

		constexpr size_type size() const { return __size; }
		constexpr const T* begin() const { return __begin; }
		constexpr const T* end() const { return __begin + __size; }
	};
}

template<typename T>
using InitList = std::initializer_list<T>;

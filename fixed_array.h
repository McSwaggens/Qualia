#pragma once

#include "int.h"
#include "array.h"
#include "assert.h"
#include "initlist.h"

template<typename T, u64 N>
struct FixedArray {
	static constexpr u64 COUNT = N;

	T data[COUNT] = { };

	constexpr FixedArray() = default;

	constexpr FixedArray(T (&a)[COUNT]) {
		Copy(data, a, COUNT);
	}

	template<typename U>
	constexpr FixedArray(FixedArray<U, COUNT> o) {
		Copy(data, o.data, COUNT);
	}

	constexpr FixedArray(InitList<T> list) {
		Assert(list.size() == COUNT);
		Copy(data, (T*)list.begin(), COUNT);
	}

	constexpr auto& operator[](this auto& self, u64 n) { Assert(n < COUNT); return self.data[n]; }

	constexpr auto* Begin(this auto& self) { return self.data; }
	constexpr auto* begin(this auto& self) { return self.data; }

	constexpr auto* End(this auto& self) { return self.data + COUNT; }
	constexpr auto* end(this auto& self) { return self.data + COUNT; }

	constexpr          Array<T> ToArray() { return { data, COUNT }; }
	constexpr operator Array<T>() { return ToArray(); }

	constexpr bool Contains(T value) {
		for (u64 i = 0; i < COUNT; i++)
			if (data[i] == value)
				return true;
		return false;
	}

	constexpr bool operator ==(FixedArray<T, COUNT> o) {
		for (u64 i = 0; i < COUNT; i++)
			if (data[i] != o.data[i])
				return false;

		return true;
	}

	constexpr bool operator !=(FixedArray<T, COUNT> o) { return !(*this == o); }
};


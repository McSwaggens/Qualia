#pragma once

#include "general.h"
#include "math.h"
#include "assert.h"
#include "os.h"

template<typename T, u64 N>
struct LargeFixedBuffer {
	static_assert(IsPow2(N));
	static_assert(N <= -1llu / sizeof(T));

	static constexpr u64 LENGTH = N;

	static T* Buffer() {
		static T* storage = null;
		if (!storage) {
			storage = (T*)OS::AllocateVirtualMemory(sizeof(T) * N);
			Assert(storage);
		}
		return storage;
	}

	u64 head = 0;

	T* Add(T value = { }) {
		Assert(head < LENGTH);
		T* result = Buffer() + head++;
		new (result) T(value);
		return result;
	}

	u64 AddIndex(T value = { }) {
		Assert(head < LENGTH);
		new (Buffer() + head) T(value);
		return head++;
	}

	bool DoesContain(T* p) {
		return p >= Buffer() && p < Buffer() + N;
	}

	u64 ToIndex(T* p) {
		Assert(DoesContain(p));
		return p - Buffer();
	}

	T& operator [](u64 n) {
		Assert(n < LENGTH);
		return Buffer()[n];
	}
};

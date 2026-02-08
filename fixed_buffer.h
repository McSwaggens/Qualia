#pragma once

#include "general.h"
#include "math.h"
#include "array.h"
#include "alloc.h"

template<typename T, u64 N>
struct FixedBuffer {
	static_assert(IsPow2(N));

	static constexpr u64 LENGTH = N;

	u64 head = 0;
	T buffer[N] = { };

	constexpr FixedBuffer() = default;

	Array<T> Add(u64 count, T value = { }) {
		Assert(head + count <= LENGTH);
		Array<T> result = { head, count };
		Fill(result.Begin(), result.End(), value);
		head += count;
		return result;
	}

	T* Add(T value = { }) {
		Assert(head < LENGTH);
		T* result = buffer + head++;
		*result = value;
		return result;
	}

	u64 AddIndex(T value = { }) {
		Assert(head < LENGTH);
		buffer[head] = value;
		return head++;
	}

	bool DoesContain(T* p) {
		return p >= buffer && p < buffer + N;
	}

	u64 ToIndex(T* p) {
		Assert(DoesContain(p));
		return p - buffer;
	}

	constexpr T& operator [](u64 n) {
		Assert(n < LENGTH);
		return buffer[n];
	}
};

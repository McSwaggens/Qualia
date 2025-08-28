#ifndef FIXED_BUFFER_H
#define FIXED_BUFFER_H

#include "general.h"
#include "math.h"
#include "array.h"
#include "memory.h"

template<typename T, u64 N>
struct FixedBuffer {
	static_assert(IsPow2(N));

	T* buffer = null;
	u64 head = 0;

	void Init() {
		buffer = ::Alloc<T>(N);
		head = 0;
	}

	Array<T> Alloc(u64 count, T value = { }) {
		Array<T> result = { head, count };
		Fill(result.Begin(), result.End(), value);
		head += count;
		return result;
	}

	T* Alloc(T value = { }) {
		T* result = buffer + head++;
		*result = value;
		return result;
	}

	u64 AllocIndex(T value = { }) {
		buffer[head] = value;
		return head++;
	}

	bool IsWithinBuffer(T* p) {
		return p >= buffer && p < buffer + N;
	}

	u64 ToIndex(T* p) { return p - buffer; }

	constexpr T& operator [](u64 n) { return buffer[n]; }
};

#endif // FIXED_BUFFER_H

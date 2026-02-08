#pragma once

#include "int.h"
#include "general.h"
#include "assert.h"

struct BitPointer {
	byte* data = null;
	u8    bit  = 0;

	constexpr BitPointer() = default;

	constexpr BitPointer(byte* data, u8 bit) : data(data), bit(bit) {
		Assert(bit < 8);
	}

	constexpr bool Get() const { return (*data >> bit) & 1; }

	constexpr void Set(bool value) {
		*data = (*data & ~(1 << bit)) | (value << bit);
	}

	template<typename T>
	T Load() const {
		if (bit == 0) return *(T*)data;

		T result;
		byte* dst = (byte*)&result;
		for (u64 i = 0; i < sizeof(T); i++)
			dst[i] = (byte)(((u8)data[i] >> bit) | ((u8)data[i + 1] << (8 - bit)));
		return result;
	}

	template<typename T>
	void Store(T value) {
		if (bit == 0) { *(T*)data = value; return; }

		byte* src = (byte*)&value;
		u8 mask = (1 << bit) - 1;
		data[0] &= mask;
		for (u64 i = 1; i < sizeof(T); i++) data[i] = 0;
		data[sizeof(T)] &= ~mask;
		for (u64 i = 0; i < sizeof(T); i++) {
			data[i]     |= (u8)src[i] << bit;
			data[i + 1] |= (u8)src[i] >> (8 - bit);
		}
	}

	constexpr operator bool() const { return data != null; }

	constexpr bool operator ==(BitPointer o) const { return data == o.data && bit == o.bit; }
	constexpr bool operator !=(BitPointer o) const { return data != o.data || bit != o.bit; }

	constexpr bool operator <(BitPointer o)  const { return data < o.data || (data == o.data && bit < o.bit); }
	constexpr bool operator <=(BitPointer o) const { return data < o.data || (data == o.data && bit <= o.bit); }
	constexpr bool operator >(BitPointer o)  const { return data > o.data || (data == o.data && bit > o.bit); }
	constexpr bool operator >=(BitPointer o) const { return data >= o.data || (data == o.data && bit >= o.bit); }

	constexpr BitPointer operator+(s64 bits) const {
		s64 total = (s64)bit + bits;
		return { data + (total >> 3), (u8)(total & 7) };
	}

	constexpr BitPointer operator-(s64 bits) const { return *this + (-bits); }

	constexpr s64 operator-(BitPointer o) const {
		return ((s64)(data - o.data)) * 8 + ((s64)bit - (s64)o.bit);
	}

	constexpr BitPointer& operator +=(s64 bits) { *this = *this + bits; return *this; }
	constexpr BitPointer& operator -=(s64 bits) { *this = *this - bits; return *this; }

	constexpr BitPointer& operator ++()    { *this += 1; return *this; }
	constexpr BitPointer  operator ++(int) { BitPointer old = *this; *this += 1; return old; }
	constexpr BitPointer& operator --()    { *this -= 1; return *this; }
	constexpr BitPointer  operator --(int) { BitPointer old = *this; *this -= 1; return old; }
};

struct BitSpan {
	BitPointer pointer;
	u64        length = 0;

	constexpr BitSpan() = default;
	constexpr BitSpan(BitPointer pointer, u64 length) : pointer(pointer), length(length) { }
	constexpr BitSpan(BitPointer begin, BitPointer end) : pointer(begin), length((u64)(end - begin)) {
		Assert(end >= begin);
	}

	constexpr BitPointer Begin() const { return pointer; }
	constexpr BitPointer begin() const { return pointer; }
	constexpr BitPointer End()   const { return pointer + (s64)length; }
	constexpr BitPointer end()   const { return pointer + (s64)length; }

	constexpr operator bool() const { return (bool)pointer; }

	bool operator[](u64 n) const { Assert(n < length); return (pointer + (s64)n).Get(); }
};

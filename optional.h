#pragma once

#include "assert.h"

struct OptNoneType { } static constexpr OptNone;

template<typename T>
struct Optional {
	T value;
	bool present;

	constexpr Optional() : value(T()), present(false) { }
	constexpr Optional(OptNoneType) : value(T()), present(false) { }
	constexpr Optional(T value) : value(value), present(true) { }

	constexpr bool operator ==(Optional o) const {
		if (present != o.present)
			return false;
		if (!present)
			return true;
		return value == o.value;
	}

	constexpr bool operator !=(Optional o) const { return !(*this == o); }

	constexpr bool operator ==(const T& v) const { return present && value == v; }
	constexpr bool operator !=(const T& v) const { return !(*this == v); }
	constexpr bool operator ==(OptNoneType) const { return !present; }
	constexpr bool operator !=(OptNoneType) const { return present; }

	constexpr explicit operator bool() const { return present; }

	template<typename U>
	constexpr auto Or(U alt) const { return present ? value : alt; }

	T& Get() {
		Assert(present);
		return value;
	}

	const T& Get() const {
		Assert(present);
		return value;
	}
};

template<typename T>
struct Optional<T&> {
	T* ptr;

	constexpr Optional() : ptr(null) { }
	constexpr Optional(OptNoneType) : ptr(null) { }
	constexpr Optional(T& value) : ptr(&value) { }

	constexpr bool operator ==(Optional o) const {
		if (ptr == null || o.ptr == null)
			return ptr == o.ptr;
		return *ptr == *o.ptr;
	}

	constexpr bool operator !=(Optional o) const { return !(*this == o); }

	constexpr bool operator ==(const T& v) const { return ptr && *ptr == v; }
	constexpr bool operator !=(const T& v) const { return !(*this == v); }
	constexpr bool operator ==(OptNoneType) const { return ptr == null; }
	constexpr bool operator !=(OptNoneType) const { return ptr != null; }

	constexpr explicit operator bool() const { return ptr != null; }

	template<typename U>
	constexpr auto Or(U alt) const { return ptr ? *ptr : alt; }

	T& Get() {
		Assert(ptr);
		return *ptr;
	}

	const T& Get() const {
		Assert(ptr);
		return *ptr;
	}
};

template<typename T>
constexpr bool operator ==(const T& v, Optional<T> o) { return o == v; }

template<typename T>
constexpr bool operator !=(const T& v, Optional<T> o) { return !(o == v); }

template<typename T>
constexpr bool operator ==(const T& v, Optional<T&> o) { return o == v; }

template<typename T>
constexpr bool operator !=(const T& v, Optional<T&> o) { return !(o == v); }

template<typename T>
constexpr bool operator ==(OptNoneType, Optional<T> o) { return o == OptNone; }

template<typename T>
constexpr bool operator !=(OptNoneType, Optional<T> o) { return o != OptNone; }

template<typename T>
constexpr bool operator ==(OptNoneType, Optional<T&> o) { return o == OptNone; }

template<typename T>
constexpr bool operator !=(OptNoneType, Optional<T&> o) { return o != OptNone; }

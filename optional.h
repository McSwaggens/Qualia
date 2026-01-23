#ifndef OPTIONAL_H
#define OPTIONAL_H

#include "assert.h"

struct OptNoneType { } static constexpr OptNone;

template<typename T>
struct Optional {
	T value;
	bool present;

	constexpr Optional() : value(T()), present(true) { }
	constexpr Optional(OptNoneType) : value(T()), present(false) { }
	constexpr Optional(T value) : value(value), present(true) { }

	constexpr operator bool() { return present; }

	template<typename U>
	constexpr auto Or(U alt) { return present ? value : alt; }

	T& Get() {
		Assert(present);
		return value;
	}
};

#endif // OPTIONAL_H

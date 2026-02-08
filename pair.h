#pragma once

template<typename U, typename V>
struct Pair {
	U first;
	V second;

	constexpr bool operator ==(Pair v) {
		return first == v.first && second == v.second;
	}

	constexpr bool operator <(Pair v) {
		if (first  <  v.first) return true;
		if (first  == v.first) return second <  v.second;
		return false;
	}
};


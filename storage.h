#ifndef STORAGE_H
#define STORAGE_H

#include "alloc.h"
#include "general.h"

template<u64 MinSize = 8, typename Size = u64>
struct Storage {
	Size size;
	static constexpr Size InlineSize = Max(InlineSize, MinSize);
	union {
		byte inline_buffer[InlineSize];
		byte* buffer;
	};

	Size Normal(Size n) {
		return Max(n, InlineSize);
	}

	bool IsInlined() {
		return size <= InlineSize;
	}

	void ReSize(u64 n) {
		u64 old_size = size;
		u64 new_size = Normal(n);

		if (new_size == size)
			return;

		if (IsInlined()) {
			return;
		}
	}
};

#endif // STORAGE_H

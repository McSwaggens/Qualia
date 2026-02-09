#pragma once

#include "general.h"
#include "array.h"
#include "alloc.h"

struct Stack_Block {
	Stack_Block* previous;
	u64 size;
	char data[];
};

struct Stack {
	Stack_Block* block = null;
	byte* head         = null;
	byte* end          = null;

	static Stack Create(u64 size);

	void* AllocateMemory(u64 size);

	void* CopyAllocMemory(const byte* ptr, u64 size) {
		byte* result = (byte*)AllocateMemory(size);
		CopyMemory(result, ptr, size);
		return result;
	}

	template<typename T>
	T* Allocate() {
		return (T*)AllocateMemory(sizeof(T));
	}

	template<typename T, typename... Args>
	T* New(Args&&... args) {
		T* ptr = (T*)AllocateMemory(sizeof(T));
		return new (ptr) T(static_cast<Args&&>(args)...);
	}

	template<typename T>
	Array<T> AllocateArray(u64 count) {
		if (!count)
			return Array<T>();

		return Array<T>(Allocate<T>(count), count);
	}

	void  Free();
};


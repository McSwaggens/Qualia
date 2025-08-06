#ifndef STACK_H
#define STACK_H

#include "general.h"
#include "array.h"

struct Stack_Block {
	Stack_Block* previous;
	u64 size;
	char data[];
};

struct Stack {
	Stack_Block* block = null;
	byte* head         = null;
	byte* end          = null;

	void* AllocateMemory(u64 size);

	template<typename T>
	T* Allocate(u64 count = 1) {
		return (T*)AllocateMemory(sizeof(T) * count);
	}

	template<typename T>
	Array<T> AllocateArray(u64 count) {
		if (!count)
			return Array<T>();

		return Array<T>(Allocate<T>(count), count);
	}

	void  Free();
};

static Stack CreateStack(u64 size);

#endif // STACK_H

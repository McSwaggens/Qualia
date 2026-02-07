#include "stack.h"
#include "assert.h"
#include "os.h"

Stack Stack::Create(u64 size) {
	Assert((size & 4095) == 0);

	// @cleanme
	Stack stack;
	stack.block = (Stack_Block*)OS::AllocateVirtualMemory(size);
	stack.block->previous = null;
	stack.block->size = size;
	stack.end = stack.block->data + size - sizeof(Stack_Block);
	stack.head = stack.block->data;
	return stack;
}

void* Stack::AllocateMemory(u64 size) {
	// Print("StackAllocate(%)\n", size);
	byte* result = head;
	head += size;

	if (head < end)
		return result;

	u64 old_block_size = block->size;
	u64 new_block_size = (old_block_size << 1) | (size & -old_block_size);

	Stack_Block* old_block = block;
	Stack_Block* new_block = (Stack_Block*)OS::AllocateVirtualMemory(new_block_size);

	new_block->previous = old_block;
	new_block->size = new_block_size;
	result = new_block->data;

	end = new_block->data + new_block_size - sizeof(Stack_Block);
	head = new_block->data + size;
	block = new_block;

	return result;
}

// Remove this functionality?
void Stack::Free() {
	while (block) {
		OS::FreeVirtualMemory((byte*)block, block->size);
		block = block->previous;
	}
}


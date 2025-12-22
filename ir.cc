#include "ir.h"
#include "print.h"

static void IR::Init() {
	stack = CreateStack(1llu<<32);
	value_buffer.Init();
	value_buffer.AllocIndex(); // Don't use index 0.

	for (int i = 0; i < 256; i++) {
		Value value = value_buffer.AllocIndex();
	}
}

static void Write(OutputBuffer* buffer, IR::Value value) {
	if (!value) {
		buffer->Write("null-value");
		return;
	}

	buffer->Write("Value(");
	buffer->Write(value.handle);
	buffer->Write(")");
}


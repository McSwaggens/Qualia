#include "ir.h"
#include "print.h"

static void IR::Init() {
	stack = Stack::Create(1llu<<32);
	value_buffer.AddIndex(); // Don't use index 0.

	for (u64 i = 0; i < 256; i++) {
		Value value = value_buffer.AddIndex();
		value.Get() = (ValueData){ };
		value->flags = VALUE_CONSTANT;
		value->constant = Binary(i);
	}
}

static void Write(OutputBuffer* buffer, IR::Value value) {
	if (!value) {
		buffer->Write("vNull");
		return;
	}

	buffer->Write("v");
	Write(buffer, value.handle);
}

static void Write(OutputBuffer* buffer, IR::Relation::Kind kind) {
	switch (kind) {
		case IR::Relation::NotEqual:         buffer->Write("NotEqual"); break;
		case IR::Relation::Less:             buffer->Write("Less"); break;
		case IR::Relation::LessOrEqual:      buffer->Write("LessOrEqual"); break;
		case IR::Relation::Greater:          buffer->Write("Greater"); break;
		case IR::Relation::GreaterOrEqual:   buffer->Write("GreaterOrEqual"); break;
		case IR::Relation::Distance:         buffer->Write("Distance"); break;
		case IR::Relation::Remainder:        buffer->Write("Remainder"); break;
	}
}

static void Write(OutputBuffer* buffer, IR::ValueFlag flag) {
	if (flag & IR::VALUE_CONSTANT)      buffer->Write("CONSTANT");
	if (flag & IR::VALUE_LONG_CONSTANT) buffer->Write("LONG_CONSTANT");
}

static void WriteContextKey(OutputBuffer* buffer, IR::Context::Key key) {
	Write(buffer, key.kind);
	buffer->Write("(");
	Write(buffer, key.from);
	buffer->Write(", ");
	Write(buffer, key.to);
	if (key.value) {
		buffer->Write(", ");
		Write(buffer, key.value);
	}
	buffer->Write(")");
}

static void Write(OutputBuffer* buffer, IR::Value from, IR::Relation relation) {
	Write(buffer, relation.kind);
	buffer->Write("(");
	Write(buffer, relation.to);
	if (relation.value.IsValid()) {
		buffer->Write(", ");
		Write(buffer, relation.value);
	}
	buffer->Write(")[");
	for (u32 i = 0; i < relation.context->keys.Count(); i++) {
		if (i > 0) buffer->Write(", ");
		WriteContextKey(buffer, relation.context->keys.elements[i]);
	}
	buffer->Write("]");
}

static void Write(OutputBuffer* buffer, IR::Value from, Array<IR::Relation> relations) {
	buffer->Write("[");
	for (u32 i = 0; i < relations.length; i++) {
		if (i > 0) buffer->Write(", ");
		Write(buffer, from, relations[i]);
	}
	buffer->Write("]");
}

static void Write(OutputBuffer* buffer, IR::Context context) {
	buffer->Write("Context@");
	Write(buffer, (void*)&context);
	if (context.parent) {
		buffer->Write(" parent=");
		Write(buffer, (void*)context.parent);
	}
	buffer->Write(" keys=[");
	for (u32 i = 0; i < context.keys.Count(); i++) {
		if (i > 0) buffer->Write(", ");
		WriteContextKey(buffer, context.keys.elements[i]);
	}
	buffer->Write("]");
}

struct ValueRelations {
	IR::Value value;
	Array<IR::Relation> relations;
};

static void Write(OutputBuffer* buffer, ValueRelations vr) {
	Write(buffer, vr.value, vr.relations);
}

static void IR::PrintState() {
	Print("IR Values:\n");
	for (u32 i = 1; i < value_buffer.head; i++) {
		Value value = i;
		Print("% = %\n", value, ValueRelations { value, value->relations.elements.ToArray() });
	}
}

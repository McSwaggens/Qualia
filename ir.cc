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

static void WriteRelationSymbol(OutputBuffer* buffer, IR::RelationKind kind) {
	switch (kind) {
		case IR::RelationKind::NotEqual:         buffer->Write("!="); break;
		case IR::RelationKind::Less:             buffer->Write("<"); break;
		case IR::RelationKind::LessOrEqual:      buffer->Write("<="); break;
		case IR::RelationKind::Greater:          buffer->Write(">"); break;
		case IR::RelationKind::GreaterOrEqual:   buffer->Write(">="); break;
		case IR::RelationKind::Distance:         buffer->Write("dist"); break;
		case IR::RelationKind::Remainder:        buffer->Write("rem"); break;
	}
}

static void Write(OutputBuffer* buffer, IR::RelationKind kind) {
	switch (kind) {
		case IR::RelationKind::NotEqual:         buffer->Write("NotEqual"); break;
		case IR::RelationKind::Less:             buffer->Write("Less"); break;
		case IR::RelationKind::LessOrEqual:      buffer->Write("LessOrEqual"); break;
		case IR::RelationKind::Greater:          buffer->Write("Greater"); break;
		case IR::RelationKind::GreaterOrEqual:   buffer->Write("GreaterOrEqual"); break;
		case IR::RelationKind::Distance:         buffer->Write("Distance"); break;
		case IR::RelationKind::Remainder:        buffer->Write("Remainder"); break;
	}
}

static void Write(OutputBuffer* buffer, IR::ValueFlag flag) {
	if (flag & IR::VALUE_CONSTANT)      buffer->Write("CONSTANT");
	if (flag & IR::VALUE_LONG_CONSTANT) buffer->Write("LONG_CONSTANT");
}

static void Write(OutputBuffer* buffer, IR::Relation relation) {
	WriteRelationSymbol(buffer, relation.kind);
	Write(buffer, relation.to);
	buffer->Write("(");

	for (u32 i = 0; i < relation.context->keys.Count(); i++) {
		if (i > 0) buffer->Write(",");
		auto& key = relation.context->keys.elements[i];
		WriteRelationSymbol(buffer, key.kind);
		Write(buffer, key.from);
		buffer->Write(":");
		Write(buffer, key.to);
		if (key.value) {
			buffer->Write(":");
			Write(buffer, key.value);
		}
	}

	buffer->Write(")");
}

static void Write(OutputBuffer* buffer, Array<IR::Relation> relations) {
	buffer->Write("[");
	for (u32 i = 0; i < relations.length; i++) {
		if (i > 0) buffer->Write(", ");
		Write(buffer, relations[i]);
	}
	buffer->Write("]");
}

static void Write(OutputBuffer* buffer, IR::Context context) {
	buffer->Write("Context@");
	Write(buffer, (void*)&context);
	buffer->Write(" {");

	if (context.parent) {
		buffer->Write(" parent=");
		Write(buffer, (void*)context.parent);
	}

	buffer->Write(" keys=[");
	for (u32 i = 0; i < context.keys.Count(); i++) {
		if (i > 0) buffer->Write(", ");
		auto& key = context.keys.elements[i];
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
	buffer->Write("] }");
}

static void IR::PrintState() {
	Print("IR Values:\n");
	for (u32 i = 1; i < value_buffer.head; i++) {
		Value value = i;
		Print("% = %\n", value, value->relations.elements.ToArray());
	}
}

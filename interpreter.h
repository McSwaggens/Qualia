#pragma once

#include "array.h"
#include "type.h"
#include "parser.h"

// @Todo: Completely rewrite interpreter.

struct MemoryBlock
{
	byte* head;
	uint64 size;
	MemoryBlock* prev;
	MemoryBlock* next;
	byte data[];
};

struct Interpreter
{
	MemoryBlock* block;
};

struct Array_Value
{
	byte* address;
	int64 length;
};

union Value
{
	byte* ptr;
	int8  i8;
	int16 i16;
	int32 i32;
	int64 i64;
	float16 f16;
	float32 f32;
	float64 f64;
	Array_Value array;
	char data[];
};

StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter);
void Convert(Type* from_type, Value* from_value, Type* to_type, Value* to_value);
void Interpret(Ast_Code* code, char* output, StackFrame* frame, Interpreter* interpreter);
void Interpret(Ast_Function* function, char* input, char* output, Interpreter* interpreter);
void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter);
MemoryBlock* CreateMemoryBlock(uint64 min_size, MemoryBlock* prev = null);
Interpreter* CreateInterpreter(Ast_Module* module);

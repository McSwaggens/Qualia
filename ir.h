#pragma once

#include "int.h"
#include "type.h"
#include "list.h"

struct IrOperation;
struct IrValue;
struct IrBlock;
struct IrArgument;

struct Ast_Function;
struct Ast_Code;
struct Ast_VariableDeclaration;
struct Ast_Root;

enum IrOperation_Kind
{
	OPERATION_NOP = 0,

	OPERATION_STACK_ALLOCATE,
	OPERATION_MEMBER,
	OPERATION_ELEMENT,

	OPERATION_PHI,

	OPERATION_BRANCH,
	OPERATION_JUMP,
	OPERATION_CALL,

	OPERATION_RETURN,

	OPERATION_COPY,
	OPERATION_LOAD,
	OPERATION_STORE,

	// Binary expressions
	OPERATION_ADD,
	OPERATION_SUBTRACT,
	OPERATION_MULTIPLY,
	OPERATION_DIVIDE,
	OPERATION_MODULO,
	OPERATION_EXPONENTIAL,
	OPERATION_BITWISE_OR,
	OPERATION_BITWISE_AND,
	OPERATION_BITWISE_XOR,
	OPERATION_BITWISE_LEFT_SHIFT,
	OPERATION_BITWISE_RIGHT_SHIFT,

	// Unary expressions
	OPERATION_BITWISE_NOT,
	OPERATION_NOT,
	OPERATION_FLIP_SIGN,
	OPERATION_POSITIVE,
	OPERATION_SIGN_EXTEND,

	OPERATION_COMPARE_EQUAL,
	OPERATION_COMPARE_NOT_EQUAL,
	OPERATION_COMPARE_LESS,
	OPERATION_COMPARE_LESS_OR_EQUAL,
	OPERATION_COMPARE_GREATER,
	OPERATION_COMPARE_GREATER_OR_EQUAL,
	OPERATION_AND,
	OPERATION_OR,
};

static bool IsControlFlowOperation(IrOperation_Kind kind)
{
	switch (kind)
	{
		case OPERATION_BRANCH:
		case OPERATION_JUMP:
		case OPERATION_RETURN:
			return true;
		default:
			return false;
	}
}

enum IrValue_Kind
{
	IR_VALUE_UNUSED = 0,
	IR_VALUE_INLINED_CONSTANT,
	IR_VALUE_CONSTANT,
	IR_VALUE_ARGUMENT,
	IR_VALUE_REGISTER,
	IR_VALUE_BLOCK,
};

struct IrValue
{
	IrValue_Kind kind;
	Type* type;

	union
	{
		IrOperation* reg;
		IrBlock* block;
		IrArgument* argument;

		Value* value;

		bool value_bool;

		u64 value_pointer;

		s8  value_int8;
		s16 value_int16;
		s32 value_int32;
		s64 value_int64;

		u8  value_uint8;
		u16 value_uint16;
		u32 value_uint32;
		u64 value_uint64;

		f32 value_float32;
		f64 value_float64;
	};
};

struct IrPhiPair
{
	IrValue value;
	IrBlock* block;
};

struct IrArgument
{
	List<IrOperation*> users;
	IrBlock* block;
	Type* type;
	u32 id;
};

struct IrOperation
{
	IrOperation_Kind kind;
	s32 id;
	Type* type;
	IrValue a;
	IrValue b;
	IrValue c;
	List<IrValue> left_arguments;
	List<IrValue> right_arguments;
	List<IrOperation*> users;
};

struct IrBlock
{
	Ast_Function* function;
	List<IrOperation*> operations;
	List<IrOperation*> users;
	List<IrArgument*> arguments;
	s32 id;
};

struct IrContext
{
	Ast_Function* function;
	Stack_Allocator stack;
};

void Write(OutputBuffer* buffer, IrOperation op);
void Write(OutputBuffer* buffer, IrOperation_Kind kind); // Can't put this in print.h because C++ is a terrible language.
void Write(OutputBuffer* buffer, IrBlock* block);
void Write(OutputBuffer* buffer, List<IrBlock*> blocks);

void ConvertToIR(Ast_Root* root);


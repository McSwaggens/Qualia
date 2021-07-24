#pragma once

#include "int.h"
#include "type.h"
#include "list.h"

struct Ast_Function;
struct Ast_Code;
struct Ast_VariableDeclaration;
struct Ast_Root;

struct IrBlock;
struct IrInstruction;
struct IrFunction;
struct IrGlobal;

enum IrValue_Kind : u8
{
	IR_VALUE_NONE = 0,
	IR_VALUE_INSTRUCTION,
	IR_VALUE_CONSTANT,
	IR_VALUE_LARGE_CONSTANT,
	IR_VALUE_GLOBAL,
};

struct IrValue
{
	IrValue_Kind kind;
	Type* type;

	union
	{
		IrInstruction* instruction;

		char data[8];

		bool value_bool;

		u64 value_pointer;

		u8  value_uint8;
		u16 value_uint16;
		u32 value_uint32;
		u64 value_uint64;

		s8  value_int8;
		s16 value_int16;
		s32 value_int32;
		s64 value_int64;

		f32 value_float32;
		f64 value_float64;
	};
};

enum IrInstruction_Kind : u8
{
	IR_INSTRUCTION_NOP = 0,

	IR_INSTRUCTION_STACK_ALLOCATE,
	IR_INSTRUCTION_PARAMETER,
	IR_INSTRUCTION_MEMBER,
	IR_INSTRUCTION_ELEMENT,

	IR_INSTRUCTION_PHI,
	IR_INSTRUCTION_SELECT,

	IR_INSTRUCTION_BRANCH,
	IR_INSTRUCTION_JUMP,
	IR_INSTRUCTION_RETURN,

	IR_INSTRUCTION_CALL,
	IR_INSTRUCTION_COPY,
	IR_INSTRUCTION_LOAD,
	IR_INSTRUCTION_STORE,

	IR_INSTRUCTION_ADD,
	IR_INSTRUCTION_SUBTRACT,
	IR_INSTRUCTION_MULTIPLY,
	IR_INSTRUCTION_DIVIDE,
	IR_INSTRUCTION_MODULO,
	IR_INSTRUCTION_EXPONENTIAL,
	IR_INSTRUCTION_BITWISE_OR,
	IR_INSTRUCTION_BITWISE_AND,
	IR_INSTRUCTION_BITWISE_XOR,
	IR_INSTRUCTION_BITWISE_LEFT_SHIFT,
	IR_INSTRUCTION_BITWISE_RIGHT_SHIFT,

	IR_INSTRUCTION_BITWISE_NOT,
	IR_INSTRUCTION_NOT,
	IR_INSTRUCTION_FLIP_SIGN,
	IR_INSTRUCTION_POSITIVE,
	IR_INSTRUCTION_SIGN_EXTEND,

	IR_INSTRUCTION_COMPARE_EQUAL,
	IR_INSTRUCTION_COMPARE_NOT_EQUAL,
	IR_INSTRUCTION_COMPARE_LESS,
	IR_INSTRUCTION_COMPARE_LESS_OR_EQUAL,
	IR_INSTRUCTION_COMPARE_GREATER,
	IR_INSTRUCTION_COMPARE_GREATER_OR_EQUAL,

	IR_INSTRUCTION_AND,
	IR_INSTRUCTION_OR,
};

struct IrPhi
{
	IrBlock* block;
	IrValue value;
};

struct IrInstruction
{
	IrInstruction_Kind kind;
	u16 id;
	u16 block_id;
	IrInstruction* next;
	IrInstruction* dependency;
	Type* type;
	IrValue a;
	IrValue b;
	IrValue c;
	IrBlock* branch_a;
	IrBlock* branch_b;
	List<IrPhi> phis;
	List<IrInstruction*> users;
};

#define IR_INSTRUCTION_BUCKET_COUNT 64

struct IrInstruction_Bucket
{
	IrInstruction instructions[IR_INSTRUCTION_BUCKET_COUNT];
	IrInstruction_Bucket* next;
};

struct IrBlock
{
	u16 id;
	IrFunction* function;
	IrInstruction_Bucket* bucket;
	IrInstruction* bucket_head;
	IrInstruction* control;
	List<IrBlock*> users;
};

struct IrFunction
{
	u16 register_id_counter;
	u16 block_id_counter;
	Ast_Function* function;
	List<IrBlock*> blocks;
};

void Write(OutputBuffer* buffer, IrInstruction instruction);
void Write(OutputBuffer* buffer, IrInstruction_Kind kind); // Can't put this in print.h because C++ is a terrible language.
void Write(OutputBuffer* buffer, IrBlock* block);
void Write(OutputBuffer* buffer, IrFunction* function);
void Write(OutputBuffer* buffer, List<IrBlock*> blocks);
void Write(OutputBuffer* buffer, List<IrBlock> blocks);

void ConvertToIR(Ast_Root* root);


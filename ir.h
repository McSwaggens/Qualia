#pragma once

#include "int.h"
#include "type.h"
#include "list.h"

struct Ast_Function;
struct Ast_Module;

enum IrLogicKind  : u8
{
	LOGOS_NOP = 0,

	LOGOS_CONSTANT,
	LOGOS_BLOCK,
	LOGOS_FUNCTION,

	LOGOS_STACK,
	LOGOS_PARAMETER,
	LOGOS_MEMBER,
	LOGOS_INDEX,

	LOGOS_PHI,
	LOGOS_SELECT,

	LOGOS_BRANCH,
	LOGOS_JUMP,
	LOGOS_RETURN,

	LOGOS_CALL,
	LOGOS_COPY,
	LOGOS_LOAD,
	LOGOS_STORE,

	LOGOS_ADD,
	LOGOS_SUBTRACT,
	LOGOS_MULTIPLY,
	LOGOS_DIVIDE,
	LOGOS_MODULO,
	LOGOS_EXPONENTIAL,

	LOGOS_BITWISE_OR,
	LOGOS_BITWISE_AND,
	LOGOS_BITWISE_XOR,
	LOGOS_BITWISE_LEFT_SHIFT,
	LOGOS_BITWISE_RIGHT_SHIFT,

	LOGOS_BITWISE_NOT,
	LOGOS_NOT,
	LOGOS_POSITIVE,

	LOGOS_SIGN_EXTEND,
	LOGOS_ZERO_EXTEND,

	LOGOS_NARROW,

	LOGOS_INT_TO_FLOAT,
	LOGOS_FLOAT_TO_INT,

	LOGOS_FLOAT_CONVERT,

	LOGOS_COMPARE_EQUAL,
	LOGOS_COMPARE_NOT_EQUAL,

	LOGOS_SIGNED_COMPARE_LESS,
	LOGOS_SIGNED_COMPARE_LESS_OR_EQUAL,
	LOGOS_SIGNED_COMPARE_GREATER,
	LOGOS_SIGNED_COMPARE_GREATER_OR_EQUAL,

	LOGOS_UNSIGNED_COMPARE_LESS,
	LOGOS_UNSIGNED_COMPARE_LESS_OR_EQUAL,
	LOGOS_UNSIGNED_COMPARE_GREATER,
	LOGOS_UNSIGNED_COMPARE_GREATER_OR_EQUAL,

	LOGOS_FLOAT_COMPARE_LESS,
	LOGOS_FLOAT_COMPARE_LESS_OR_EQUAL,
	LOGOS_FLOAT_COMPARE_GREATER,
	LOGOS_FLOAT_COMPARE_GREATER_OR_EQUAL,

	LOGOS_AND,
	LOGOS_OR,
};

String ToString(IrLogicKind  opcode)
{
	switch (opcode)
	{
		case LOGOS_NOP:                      return "nop";
		case LOGOS_CONSTANT:                 return "constant";
		case LOGOS_BLOCK:                    return "block";
		case LOGOS_FUNCTION:                 return "function";
		case LOGOS_PHI:                      return "phi";
		case LOGOS_STACK:                    return "stack";
		case LOGOS_PARAMETER:                return "param";
		case LOGOS_MEMBER:                   return "member";
		case LOGOS_INDEX:                    return "index";
		case LOGOS_SELECT:                   return "select";
		case LOGOS_LOAD:                     return "load";
		case LOGOS_STORE:                    return "store";
		case LOGOS_COPY:                     return "copy";
		case LOGOS_CALL:                     return "call";
		case LOGOS_BRANCH:                   return "branch";
		case LOGOS_JUMP:                     return "jump";
		case LOGOS_RETURN:                   return "return";
		case LOGOS_ADD:                      return "add";
		case LOGOS_SUBTRACT:                 return "subtract";
		case LOGOS_MULTIPLY:                 return "multiply";
		case LOGOS_DIVIDE:                   return "divide";
		case LOGOS_MODULO:                   return "modulo";
		case LOGOS_EXPONENTIAL:              return "exponential";
		case LOGOS_NOT:                      return "not";
		case LOGOS_POSITIVE:                 return "pos";
		case LOGOS_SIGN_EXTEND:              return "sign_extend";
		case LOGOS_ZERO_EXTEND:              return "zero_extend";
		case LOGOS_NARROW:                   return "narrow";
		case LOGOS_INT_TO_FLOAT:             return "to_float";
		case LOGOS_FLOAT_TO_INT:             return "to_int";
		case LOGOS_FLOAT_CONVERT:            return "float_convert";
		case LOGOS_BITWISE_NOT:              return "NOT";
		case LOGOS_BITWISE_OR:               return "OR";
		case LOGOS_BITWISE_AND:              return "AND";
		case LOGOS_BITWISE_XOR:              return "XOR";
		case LOGOS_BITWISE_LEFT_SHIFT:       return "left_shift";
		case LOGOS_BITWISE_RIGHT_SHIFT:      return "right_shift";
		case LOGOS_COMPARE_EQUAL:                     return "compare_equal";
		case LOGOS_COMPARE_NOT_EQUAL:                 return "compare_not_equal";
		case LOGOS_SIGNED_COMPARE_LESS:               return "signed_compare_less";
		case LOGOS_SIGNED_COMPARE_LESS_OR_EQUAL:      return "signed_compare_less_or_equal";
		case LOGOS_SIGNED_COMPARE_GREATER:            return "signed_compare_greater";
		case LOGOS_SIGNED_COMPARE_GREATER_OR_EQUAL:   return "signed_compare_greater_or_equal";
		case LOGOS_UNSIGNED_COMPARE_LESS:             return "unsigned_compare_less";
		case LOGOS_UNSIGNED_COMPARE_LESS_OR_EQUAL:    return "unsigned_compare_less_or_equal";
		case LOGOS_UNSIGNED_COMPARE_GREATER:          return "unsigned_compare_greater";
		case LOGOS_UNSIGNED_COMPARE_GREATER_OR_EQUAL: return "unsigned_compare_greater_or_equal";
		case LOGOS_FLOAT_COMPARE_LESS:                return "float_compare_less";
		case LOGOS_FLOAT_COMPARE_LESS_OR_EQUAL:       return "float_compare_less_or_equal";
		case LOGOS_FLOAT_COMPARE_GREATER:             return "float_compare_greater";
		case LOGOS_FLOAT_COMPARE_GREATER_OR_EQUAL:    return "float_compare_greater_or_equal";
		case LOGOS_AND:                      return "and";
		case LOGOS_OR:                       return "or";
	}
}

using IrIndex = u16;

using IrLogicIndex        = IrIndex;
using IrBlockInfoIndex    = IrIndex;
using IrFunctionInfoIndex = IrIndex;

const IrIndex IR_NONE = -1;

struct IrBlockInfo
{
	List<IrLogicIndex> logic;
	IrLogicIndex branch;
	IrLogicIndex representation;
	IrBlockInfoIndex next;
};

struct IrLogic
{
	IrLogicKind kind;

	union
	{
		IrBlockInfoIndex block;
		IrLogicIndex next_logic;
	};

	Type* type;
	IrIndex operands[3];
	List<IrLogicIndex> users;

	union
	{
		bool constant_bool;

		s8  constant_int8;
		s16 constant_int16;
		s32 constant_int32;
		s64 constant_int64;

		f32 constant_float32;
		f64 constant_float64;
	};
};

struct IrFunctionInfo
{
	IrFunctionInfoIndex id;
	IrBlockInfoIndex free_block;
	IrLogicIndex free_logic;
	Ast_Function* function;
	List<IrBlockInfo> blocks;
	List<IrLogicIndex> constants; // @Todo: Store constants in array for quick search.
	List<IrLogic> logic;

	IrLogicIndex bool_cache[2];    // false, true
	IrLogicIndex int_cache[3];     // -1, 0, 1
	IrLogicIndex float32_cache[3]; // -1f, 0f, 1f
	IrLogicIndex float64_cache[3]; // -1f, 0f, 1f
};

static IrBlockInfo* GetBlockInfo(IrLogicIndex block, IrFunctionInfo* function)
{
	return &function->blocks[function->logic[block].block];
}

void Write(OutputBuffer* buffer, IrIndex id);
void GenerateIR(Ast_Module* module);


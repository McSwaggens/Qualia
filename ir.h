#pragma once

#include "general.h"
#include "type.h"
#include "list.h"

struct Ast_Function;
struct Ast_Module;

static const uint8 IR_BAND_MASK = (3 << (8-2));

static const uint8 IR_BAND0              = (0 << (8-2));
static const uint8 IR_BAND0_OPERAND_LIST = IR_BAND0 | (1 << (8-3));

static const uint8 IR_BAND1 = (1 << (8-2));
static const uint8 IR_BAND2 = (2 << (8-2));
static const uint8 IR_BAND3 = (3 << (8-2));

enum Instruction : uint8
{
	// ..
};

using BlockHandle = uint32;
using OperandInfo = uint8;

struct Instruction_Pool_Soa
{
	Instruction    kind[64];
	BlockHandle    block[64];
	OperandInfo    operand_info[64];
	Instruction*   operand0[64];
	Instruction*   operand1[64];
	Instruction*   operand2[64];
	Instruction*** users[64];
};

enum IrInstructionKind : uint8
{
// 0 Operands:
	IR_NOP    = 0 | IR_BAND0,
	IR_STACK  = 1 | IR_BAND0,
	IR_PHI    = 3 | IR_BAND0_OPERAND_LIST,
	IR_TUPLE  = 4 | IR_BAND0_OPERAND_LIST,

// 1 Operands:
	IR_PARAMETER    = 0  | IR_BAND1,
	IR_NOT          = 1  | IR_BAND1,
	IR_SIGN_EXTEND  = 2  | IR_BAND1,
	IR_ZERO_EXTEND  = 3  | IR_BAND1,
	IR_TRUNCATE     = 4  | IR_BAND1,
	IR_INT_TO_FLOAT = 5  | IR_BAND1,
	IR_FLOAT_TO_INT = 6  | IR_BAND1,
	IR_FLOAT_CAST   = 7  | IR_BAND1,
	IR_LOAD         = 8  | IR_BAND1,
	IR_BRANCH       = 9  | IR_BAND1,
	IR_RETURN       = 10 | IR_BAND1,

// 2 Operands:
	IR_MEMBER                            = 0  | IR_BAND2,
	IR_INDEX                             = 1  | IR_BAND2,

	IR_COPY                              = 2  | IR_BAND2,
	IR_STORE                             = 3  | IR_BAND2,
	IR_CALL                              = 4  | IR_BAND2,

	IR_INT_ADD                           = 5  | IR_BAND2,
	IR_INT_SUBTRACT                      = 6  | IR_BAND2,
	IR_INT_MULTIPLY                      = 7  | IR_BAND2,

	IR_SIGNED_DIVIDE                     = 8  | IR_BAND2,
	IR_SIGNED_MODULO                     = 9  | IR_BAND2,

	IR_UNSIGNED_DIVIDE                   = 10 | IR_BAND2,
	IR_UNSIGNED_MODULO                   = 11 | IR_BAND2,

	IR_SIGNED_LEFT_SHIFT                 = 12 | IR_BAND2,
	IR_SIGNED_RIGHT_SHIFT                = 13 | IR_BAND2,

	IR_UNSIGNED_LEFT_SHIFT               = 14 | IR_BAND2,
	IR_UNSIGNED_RIGHT_SHIFT              = 15 | IR_BAND2,

	IR_FLOAT_ADD                         = 16 | IR_BAND2,
	IR_FLOAT_SUBTRACT                    = 17 | IR_BAND2,
	IR_FLOAT_MULTIPLY                    = 18 | IR_BAND2,
	IR_FLOAT_DIVIDE                      = 19 | IR_BAND2,

	IR_OR                                = 20 | IR_BAND2,
	IR_AND                               = 21 | IR_BAND2,
	IR_XOR                               = 22 | IR_BAND2,

	IR_COMPARE_EQUAL                     = 23 | IR_BAND2,
	IR_COMPARE_NOT_EQUAL                 = 24 | IR_BAND2,

	IR_SIGNED_COMPARE_LESS               = 25 | IR_BAND2,
	IR_SIGNED_COMPARE_LESS_OR_EQUAL      = 26 | IR_BAND2,
	IR_SIGNED_COMPARE_GREATER            = 27 | IR_BAND2,
	IR_SIGNED_COMPARE_GREATER_OR_EQUAL   = 28 | IR_BAND2,

	IR_UNSIGNED_COMPARE_LESS             = 29 | IR_BAND2,
	IR_UNSIGNED_COMPARE_LESS_OR_EQUAL    = 30 | IR_BAND2,
	IR_UNSIGNED_COMPARE_GREATER          = 31 | IR_BAND2,
	IR_UNSIGNED_COMPARE_GREATER_OR_EQUAL = 32 | IR_BAND2,

	IR_FLOAT_COMPARE_LESS                = 33 | IR_BAND2,
	IR_FLOAT_COMPARE_LESS_OR_EQUAL       = 34 | IR_BAND2,
	IR_FLOAT_COMPARE_GREATER             = 35 | IR_BAND2,
	IR_FLOAT_COMPARE_GREATER_OR_EQUAL    = 36 | IR_BAND2,

// 3 Operands:
	IR_SELECT = 0 | IR_BAND3,
};

static bool IsControlFlowInstruction(IrInstructionKind kind)
{
	return kind == IR_RETURN || kind == IR_BRANCH;
}

static String ToString(IrInstructionKind opcode)
{
	switch (opcode)
	{
		case IR_NOP:                               return "nop";
		case IR_PHI:                               return "phi";
		case IR_TUPLE:                             return "tuple";
		case IR_STACK:                             return "stack";
		case IR_PARAMETER:                         return "param";
		case IR_MEMBER:                            return "member";
		case IR_INDEX:                             return "index";
		case IR_SELECT:                            return "select";
		case IR_LOAD:                              return "load";
		case IR_STORE:                             return "store";
		case IR_COPY:                              return "copy";
		case IR_CALL:                              return "call";
		case IR_BRANCH:                            return "branch";
		case IR_RETURN:                            return "return";
		case IR_INT_ADD:                           return "int_add";
		case IR_INT_SUBTRACT:                      return "int_subtract";
		case IR_INT_MULTIPLY:                      return "int_multiply";
		case IR_SIGNED_DIVIDE:                     return "signed_divide";
		case IR_UNSIGNED_DIVIDE:                   return "unsigned_divide";
		case IR_SIGNED_MODULO:                     return "signed_modulo";
		case IR_UNSIGNED_MODULO:                   return "unsigned_modulo";
		case IR_FLOAT_ADD:                         return "float_add";
		case IR_FLOAT_SUBTRACT:                    return "float_subtract";
		case IR_FLOAT_MULTIPLY:                    return "float_multiply";
		case IR_FLOAT_DIVIDE:                      return "float_divide";
		case IR_SIGN_EXTEND:                       return "sign_extend";
		case IR_ZERO_EXTEND:                       return "zero_extend";
		case IR_TRUNCATE:                          return "truncate";
		case IR_INT_TO_FLOAT:                      return "int_to_float";
		case IR_FLOAT_TO_INT:                      return "float_to_int";
		case IR_FLOAT_CAST:                        return "float_cast";
		case IR_NOT:                               return "not";
		case IR_OR:                                return "or";
		case IR_AND:                               return "and";
		case IR_XOR:                               return "xor";
		case IR_SIGNED_LEFT_SHIFT:                 return "signed_left_shift";
		case IR_SIGNED_RIGHT_SHIFT:                return "signed_right_shift";
		case IR_UNSIGNED_LEFT_SHIFT:               return "unsigned_left_shift";
		case IR_UNSIGNED_RIGHT_SHIFT:              return "unsigned_right_shift";
		case IR_COMPARE_EQUAL:                     return "compare_equal";
		case IR_COMPARE_NOT_EQUAL:                 return "compare_not_equal";
		case IR_SIGNED_COMPARE_LESS:               return "signed_compare_less";
		case IR_SIGNED_COMPARE_LESS_OR_EQUAL:      return "signed_compare_less_or_equal";
		case IR_SIGNED_COMPARE_GREATER:            return "signed_compare_greater";
		case IR_SIGNED_COMPARE_GREATER_OR_EQUAL:   return "signed_compare_greater_or_equal";
		case IR_UNSIGNED_COMPARE_LESS:             return "unsigned_compare_less";
		case IR_UNSIGNED_COMPARE_LESS_OR_EQUAL:    return "unsigned_compare_less_or_equal";
		case IR_UNSIGNED_COMPARE_GREATER:          return "unsigned_compare_greater";
		case IR_UNSIGNED_COMPARE_GREATER_OR_EQUAL: return "unsigned_compare_greater_or_equal";
		case IR_FLOAT_COMPARE_LESS:                return "float_compare_less";
		case IR_FLOAT_COMPARE_LESS_OR_EQUAL:       return "float_compare_less_or_equal";
		case IR_FLOAT_COMPARE_GREATER:             return "float_compare_greater";
		case IR_FLOAT_COMPARE_GREATER_OR_EQUAL:    return "float_compare_greater_or_equal";
	}
}

using IrIndex = uint32;
using IrInstructionID = IrIndex;
using IrFunctionID = IrIndex;

struct IrBlockID { IrIndex index; };

static const IrIndex   IR_NONE = -1;
static const IrBlockID IR_ROOT = IrBlockID { 0 };

enum IrValueKind : uint8
{
	IR_VALUE_NONE = 0,
	IR_VALUE_INSTRUCTION,
	IR_VALUE_CONSTANT_INT,
	IR_VALUE_CONSTANT_FLOAT32,
	IR_VALUE_CONSTANT_FLOAT64,
};

struct IrValue
{
	IrValueKind kind;

	union
	{
		IrInstructionID instruction;
		int64 constant_int;
		float32 constant_float32;
		float64 constant_float64;
	};
};

static inline bool IsValid(IrValue value)
{
	return value.kind != IR_VALUE_NONE;
}

static inline bool IsValid(IrBlockID block)
{
	return block.index != IR_NONE;
}

static inline IrValue None()
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_NONE;
	return value;
}

static inline IrValue GetValue(IrInstructionID id)
{
	Assert(id != IR_NONE);

	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_INSTRUCTION;
	value.instruction = id;
	return value;
}

static inline IrInstructionID GetInstruction(IrValue value)
{
	Assert(IsValid(value));
	Assert(value.kind == IR_VALUE_INSTRUCTION);
	return value.instruction;
}

static inline IrValue GetConstantInt(int64 n)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_CONSTANT_INT;
	value.constant_int = n;
	return value;
}

static inline IrValue GetConstantFloat32(float32 f)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_CONSTANT_FLOAT32;
	value.constant_float32 = f;
	return value;
}

static inline IrValue GetConstantFloat64(float64 f)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_CONSTANT_FLOAT64;
	value.constant_float64 = f;
	return value;
}

enum IrBlockKind
{
	IR_BLOCK_FREE = 0,
	IR_BLOCK_UNSPECIFIED,
	IR_BLOCK_JUMP,
	IR_BLOCK_BRANCH,
	IR_BLOCK_RETURN,
};

struct IrBlock
{
	IrBlockKind kind;

	List<IrInstructionID> instructions;
	List<IrInstructionID> phis;

	union
	{
		IrBlockID next;
		IrBlockID jump;
		IrInstructionID return_instruction;

		struct
		{
			IrBlockID true_branch;
			IrBlockID false_branch;
			IrInstructionID branch_instruction;
		} branch;
	};

	List<IrBlockID> users;
};

struct IrPhiEntry
{
	IrBlockID block;
	IrValue value;
};

struct IrInstruction
{
	IrInstructionKind kind;
	IrBlockID block;
	Type* type;

	union
	{
		IrValue operands[3];
		List<IrValue> tuple;
		List<IrPhiEntry> phi_entries;
	};

	List<IrInstructionID> users;
};

struct IrFunction
{
	IrFunctionID id;
	Ast_Function* function;
	IrInstructionID free_instruction;
	IrBlockID free_block;
	List<IrInstruction> instructions;
	List<IrBlock> blocks;
};

static IrBlock* GetBlock(IrFunction* function, IrBlockID block)
{
	Assert(block.index != IR_NONE);
	Assert(block.index < function->blocks.count);
	return &function->blocks[block.index];
}

static IrInstruction* GetInstruction(IrFunction* function, IrInstructionID id)
{
	Assert(id != IR_NONE);
	Assert(id < function->instructions.count);
	return &function->instructions[id];
}

static IrInstruction* GetInstruction(IrFunction* function, IrValue value)
{
	Assert(IsValid(value));
	return GetInstruction(function, value.instruction);
}

static void Write(OutputBuffer* buffer, IrFunction* function);
static void GenerateIR(Ast_Module* module);


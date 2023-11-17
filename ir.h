#pragma once

#include "general.h"
#include "type_system.h"
#include "list.h"

struct Ast_Function;
struct Ast_Module;

struct Instruction;
struct Block;
struct Procedure;

static const uint64 IR_AUX_BITCNT = 3;
static const uint16 IR_AUX_OPCNT_BITCNT = 2;
static const uint16 IR_AUX_RETBIT = 1<<15;
static const uint64 IR_OPCODE_BITCNT = 16 - IR_AUX_BITCNT;

static const uint16 IR_RETBIT = (4 << (16-IR_AUX_BITCNT));
static const uint16 IR_OPCNT0 = (0 << (16-IR_AUX_BITCNT));
static const uint16 IR_OPCNT1 = (1 << (16-IR_AUX_BITCNT));
static const uint16 IR_OPCNT2 = (2 << (16-IR_AUX_BITCNT));
static const uint16 IR_OPCNT3 = (3 << (16-IR_AUX_BITCNT));

enum OpCode : uint16
{
	// ---------------- 0 Operands ---------------- //
	IR_NOP    = 0  | IR_OPCNT0,
	IR_STACK  = 1  | IR_OPCNT0 | IR_RETBIT,
	IR_RET    = 2  | IR_OPCNT0,
	IR_PHI    = 3  | IR_OPCNT0 | IR_RETBIT,

	// ---------------- 1 Operands ---------------- //
	IR_PARAM  = 0  | IR_OPCNT1 | IR_RETBIT,
	IR_NOT    = 1  | IR_OPCNT1 | IR_RETBIT,
	IR_SEXT   = 2  | IR_OPCNT1 | IR_RETBIT,
	IR_ZEXT   = 3  | IR_OPCNT1 | IR_RETBIT,
	IR_TRUNC  = 4  | IR_OPCNT1 | IR_RETBIT,
	IR_ITOF   = 5  | IR_OPCNT1 | IR_RETBIT,
	IR_FTOI   = 6  | IR_OPCNT1 | IR_RETBIT,
	IR_FTOF   = 7  | IR_OPCNT1 | IR_RETBIT,
	IR_LOAD   = 8  | IR_OPCNT1 | IR_RETBIT,
	IR_RETV   = 10 | IR_OPCNT1,
	IR_JUMP   = 11 | IR_OPCNT1,

	// ---------------- 2 Operands ---------------- //
	IR_MEMBER = 0  | IR_OPCNT2 | IR_RETBIT,
	IR_INDEX  = 1  | IR_OPCNT2 | IR_RETBIT,

	IR_COPY   = 2  | IR_OPCNT2,
	IR_STORE  = 3  | IR_OPCNT2,
	IR_CALL   = 4  | IR_OPCNT2 | IR_RETBIT,

	IR_ADD    = 5  | IR_OPCNT2 | IR_RETBIT,
	IR_SUB    = 6  | IR_OPCNT2 | IR_RETBIT,

	IR_UMUL   = 7  | IR_OPCNT2 | IR_RETBIT,
	IR_SMUL   = 8  | IR_OPCNT2 | IR_RETBIT,

	IR_SDIV   = 9  | IR_OPCNT2 | IR_RETBIT,
	IR_SMOD   = 10 | IR_OPCNT2 | IR_RETBIT,

	IR_UDIV   = 11 | IR_OPCNT2 | IR_RETBIT,
	IR_UMOD   = 12 | IR_OPCNT2 | IR_RETBIT,

	IR_SLSH   = 13 | IR_OPCNT2 | IR_RETBIT,
	IR_SRSH   = 14 | IR_OPCNT2 | IR_RETBIT,

	IR_ULSH   = 15 | IR_OPCNT2 | IR_RETBIT,
	IR_URSH   = 16 | IR_OPCNT2 | IR_RETBIT,

	IR_FADD   = 17 | IR_OPCNT2 | IR_RETBIT,
	IR_FSUB   = 18 | IR_OPCNT2 | IR_RETBIT,
	IR_FMUL   = 19 | IR_OPCNT2 | IR_RETBIT,
	IR_FDIV   = 20 | IR_OPCNT2 | IR_RETBIT,

	IR_OR     = 21 | IR_OPCNT2 | IR_RETBIT,
	IR_AND    = 22 | IR_OPCNT2 | IR_RETBIT,
	IR_XOR    = 23 | IR_OPCNT2 | IR_RETBIT,

	IR_CMP    = 24 | IR_OPCNT2 | IR_RETBIT,
	IR_CMPNE  = 25 | IR_OPCNT2 | IR_RETBIT,

	IR_SCMPL  = 26 | IR_OPCNT2 | IR_RETBIT,
	IR_SCMPLE = 27 | IR_OPCNT2 | IR_RETBIT,
	IR_SCMPG  = 28 | IR_OPCNT2 | IR_RETBIT,
	IR_SCMPGE = 29 | IR_OPCNT2 | IR_RETBIT,

	IR_UCMPL  = 30 | IR_OPCNT2 | IR_RETBIT,
	IR_UCMPLE = 31 | IR_OPCNT2 | IR_RETBIT,
	IR_UCMPG  = 32 | IR_OPCNT2 | IR_RETBIT,
	IR_UCMPGE = 33 | IR_OPCNT2 | IR_RETBIT,

	IR_FCMPL  = 34 | IR_OPCNT2 | IR_RETBIT,
	IR_FCMPLE = 35 | IR_OPCNT2 | IR_RETBIT,
	IR_FCMPG  = 36 | IR_OPCNT2 | IR_RETBIT,
	IR_FCMPGE = 37 | IR_OPCNT2 | IR_RETBIT,

	// ---------------- 3 Operands ---------------- //
	IR_SELECT = 0  | IR_OPCNT3 | IR_RETBIT,
	IR_BRANCH = 1  | IR_OPCNT3,
};

static bool IsControlFlowInstruction(OpCode opcode)
{
	switch (opcode)
	{
		case IR_RET:
		case IR_RETV:
		case IR_BRANCH:
		case IR_JUMP:
			return true;
		default:
			return false;
	}
}

static String ToString(OpCode opcode)
{
	switch (opcode)
	{
		case IR_NOP:    return "nop";
		case IR_PHI:    return "phi";
		case IR_STACK:  return "stack";
		case IR_PARAM:  return "param";
		case IR_MEMBER: return "member";
		case IR_INDEX:  return "index";
		case IR_SELECT: return "select";
		case IR_LOAD:   return "load";
		case IR_STORE:  return "store";
		case IR_COPY:   return "copy";
		case IR_CALL:   return "call";
		case IR_BRANCH: return "branch";
		case IR_JUMP:   return "jump";
		case IR_RET:    return "ret";
		case IR_RETV:   return "retv";
		case IR_ADD:    return "add";
		case IR_SUB:    return "sub";
		case IR_SMUL:   return "smul";
		case IR_UMUL:   return "umul";
		case IR_SDIV:   return "sdiv";
		case IR_UDIV:   return "udiv";
		case IR_SMOD:   return "smod";
		case IR_UMOD:   return "umod";
		case IR_FADD:   return "fadd";
		case IR_FSUB:   return "fsub";
		case IR_FMUL:   return "fmul";
		case IR_FDIV:   return "fdiv";
		case IR_SEXT:   return "sext";
		case IR_ZEXT:   return "zext";
		case IR_TRUNC:  return "trunc";
		case IR_ITOF:   return "itof";
		case IR_FTOI:   return "ftoi";
		case IR_FTOF:   return "ftof";
		case IR_NOT:    return "not";
		case IR_OR:     return "or";
		case IR_AND:    return "and";
		case IR_XOR:    return "xor";
		case IR_SLSH:   return "slsh";
		case IR_SRSH:   return "srsh";
		case IR_ULSH:   return "ulsh";
		case IR_URSH:   return "ursh";
		case IR_CMP:    return "cmp";
		case IR_CMPNE:  return "cmpne";
		case IR_SCMPL:  return "scmpl";
		case IR_SCMPLE: return "scmple";
		case IR_SCMPG:  return "scmpg";
		case IR_SCMPGE: return "scmpge";
		case IR_UCMPL:  return "ucmpl";
		case IR_UCMPLE: return "ucmple";
		case IR_UCMPG:  return "ucmpg";
		case IR_UCMPGE: return "ucmpge";
		case IR_FCMPL:  return "fcmpl";
		case IR_FCMPLE: return "fcmple";
		case IR_FCMPG:  return "fcmpg";
		case IR_FCMPGE: return "fcmpge";
	}
}

enum ValueKind : uint8
{
	IR_NONE = 0,
	IR_INSTRUCTION,
	IR_BLOCK,
	IR_PROCEDURE,
	IR_CONST_INT,
	IR_CONST_FLOAT32,
	IR_CONST_FLOAT64,
};

struct Value
{
	ValueKind kind;

	union
	{
		Instruction* instruction;
		Block* block;
		Procedure* procedure;
		int64 const_int;
		float32 const_f32;
		float64 const_f64;
	};

	// Value() = default;
	Value()               : kind(IR_NONE),          const_int(0)   { }
	Value(Instruction* i) : kind(IR_INSTRUCTION),   instruction(i) { }
	Value(Block*       b) : kind(IR_BLOCK),         block(b)       { }
	Value(Procedure*   p) : kind(IR_PROCEDURE),     procedure(p)   { }
	Value(int64        n) : kind(IR_CONST_INT),     const_int(n)   { }
	Value(float32      f) : kind(IR_CONST_FLOAT32), const_f32(f)   { }
	Value(float64      f) : kind(IR_CONST_FLOAT64), const_f64(f)   { }
};

struct Procedure
{
	Ast_Function* function;
	List<Block*> blocks;

	Block* NewBlock();
};

struct Block
{
	Procedure* procedure;
	uint64 id;

	List<Instruction*> instructions;
	List<Instruction*> phis;

	Instruction* controlFlowInstruction;

	List<Block*> users;

	Instruction* NewInstruction(Instruction instruction);
	Instruction* Jump(Block* to);
	Instruction* Branch(Value condition, Block* btrue, Block* bfalse);
	Instruction* Return(Value value);
	Instruction* Return();
	Value Cast(Value value, TypeID from, TypeID to);
};

struct PhiEntry
{
	Block* block;
	Value value;
};

struct Instruction
{
	OpCode opcode;
	Block* block;
	TypeID type;
	uint64 id;

	union
	{
		struct { Value op0; Value op1; Value op2; }; Value ops[3];
		List<PhiEntry> entries;
	};

	List<Instruction*> users;

	bool DoesReturn()
	{
		return opcode & IR_RETBIT;
	}

	uint64 GetOperandCount()
	{
		return ((uint64)opcode >> IR_OPCODE_BITCNT) & ((1<<IR_AUX_OPCNT_BITCNT)-1);
	}
};

static inline Value None() { return Value(); }

static void Write(OutputBuffer* buffer, Procedure* function);
static void GenerateIR(Ast_Module* module);


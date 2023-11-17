#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

static List<Procedure*> procedures = null;

Block* Procedure::NewBlock()
{
	Block* block = Allocate<Block>();
	ZeroMemory(block);

	this->blocks.Add(block);

	return block;
}

Instruction* Block::NewInstruction(Instruction instruction)
{
	Instruction* result = Allocate<Instruction>();
	*result = instruction;

	result->block = this;

	if (result->op0.kind == IR_INSTRUCTION) result->op0.instruction->users.Add(result);
	if (result->op1.kind == IR_INSTRUCTION) result->op1.instruction->users.Add(result);
	if (result->op2.kind == IR_INSTRUCTION) result->op2.instruction->users.Add(result);

	this->instructions.Add(result);

	return result;
}

static void RemoveBlock(Procedure* procedure, Block* block)
{
	FreeList(block->users);
	FreeList(block->instructions);
	FreeList(block->phis);

	block->users = null;
	block->instructions = null;
	block->phis = null;
}

Instruction* Block::Jump(Block* to)
{
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_JUMP,
		.op0 = to,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Branch(Value cond, Block* btrue, Block* bfalse)
{
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_JUMP,
		.op0 = cond,
		.op1 = btrue,
		.op2 = bfalse,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Return(Value value)
{
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_RETV,
		.op0 = value,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Return()
{
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_RET,
	});

	this->controlFlowInstruction = result;

	return result;
}

Value Block::Cast(Value value, TypeID from, TypeID to)
{
	return value;
}

Value ExpressionToIR(Block*& block, Ast_Expression* expr);

Value ExpressionToIRLoad(Block*& block, Ast_Expression* expr)
{
	Value value = ExpressionToIR(block, expr);

	if (expr->flags & AST_EXPRESSION_FLAG_REFERENTIAL)
	{
		block->NewInstruction((Instruction){
			.opcode = IR_LOAD,
			.type = expr->type,
			.op0 = value,
		});
	}

	return value;
}

Value ExpressionToIR(Block*& block, Ast_Expression* expr)
{
	return None();
}

struct IrGen
{
	Block* return_block;
	Value  return_stack;
};

void CodeToIR(Block* block, Ast_Code* code, Block* bexit, Block* bbreak, IrGen* gen)
{
}

Procedure* FunctionToIR(Ast_Function* function)
{
	Procedure* result = Allocate<Procedure>();

	return result;
}

void GenerateIR(Ast_Module* module)
{
}


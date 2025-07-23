#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

static List<Procedure*> procedures = null;

static Procedure* MakeProcedure(String name) {
	Procedure* proc = Allocate<Procedure>();
	procedures.Add(proc);
	return proc;
}

Block* Procedure::NewBlock() {
	Block* block = Allocate<Block>();
	ZeroMemory(block);
	block->procedure = this;
	block->id = this->block_ticker++;

	this->blocks.Add(block);

	return block;
}


// -------------------------------------------------- //


Instruction* Block::NewInstruction(Instruction instruction) {
	Instruction* result = Allocate<Instruction>();
	*result = instruction;
	result->id = this->procedure->instruction_ticker++;

	result->block = this;

	if (result->op0.kind == IR_INSTRUCTION) result->op0.instruction->users.Add(result);
	if (result->op1.kind == IR_INSTRUCTION) result->op1.instruction->users.Add(result);
	if (result->op2.kind == IR_INSTRUCTION) result->op2.instruction->users.Add(result);

	this->instructions.Add(result);

	return result;
}

Instruction* Block::Param(u64 n) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_PARAM,
		.op0 = n,
	});

	return result;
}

Instruction* Block::Stack(Value size) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_STACK,
	});

	return result;
}

Instruction* Block::Load(Value addr) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_LOAD,
		.op0 = addr,
	});

	return result;
}

Instruction* Block::Store(Value dest, Value value) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_STORE,
		.op0 = dest,
		.op1 = value,
	});

	return result;
}

Instruction* Block::Add(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_ADD,
		.op0 = a,
		.op1 = b,
		.type = TYPE_INT64,
	});

	return result;
}

Instruction* Block::AddF32(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_ADD_F32,
		.op0 = a,
		.op1 = b,
		.type = TYPE_FLOAT32,
	});

	return result;
}

Instruction* Block::AddF64(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_ADD_F64,
		.op0 = a,
		.op1 = b,
		.type = TYPE_FLOAT64,
	});

	return result;
}

Instruction* Block::Sub(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_SUB,
		.op0 = a,
		.op1 = b,
		.type = TYPE_INT64,
	});

	return result;
}

Instruction* Block::SubF32(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_SUB_F32,
		.op0 = a,
		.op1 = b,
		.type = TYPE_FLOAT32,
	});

	return result;
}

Instruction* Block::SubF64(Value a, Value b) {
	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_SUB_F64,
		.op0 = a,
		.op1 = b,
		.type = TYPE_FLOAT64,
	});

	return result;
}

Instruction* Block::Jump(Block* to) {
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_JUMP,
		.op0 = to,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Branch(Value cond, Block* btrue, Block* bfalse) {
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_BRANCH,
		.op0 = cond,
		.op1 = btrue,
		.op2 = bfalse,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Return(Value value) {
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_RETV,
		.op0 = value,
	});

	this->controlFlowInstruction = result;

	return result;
}

Instruction* Block::Return() {
	Assert(!this->controlFlowInstruction);

	Instruction* result = this->NewInstruction((Instruction){
		.opcode = IR_RET,
	});

	this->controlFlowInstruction = result;

	return result;
}


// -------------------------------------------------- //


Value Block::Cast(Value value, TypeID from, TypeID to) {
	return value;
}


// -------------------------------------------------- //


void Block::Remove() {
	Procedure* proc = procedure;

	Free(users);
	Free(instructions);
	Free(phis);

	users = null;
	instructions = null;
	phis = null;

	if (proc->entry == this) {
		Assert();
	}

	proc->blocks.Remove(this);
}


// -------------------------------------------------- //

struct IrGenHelper
{
	Procedure* procedure;
	Block* return_block;
	Value  return_stack;
};

static void CodeToIR(Ast_Code* code, Block* block, Block* bexit, Block* bbreak, IrGenHelper* helper);

static Value ExpressionToIR(Ast_Expression* expr, Block*& block, bool remove_reference, IrGenHelper* helper) {
	Value result = Value();

	switch (expr->kind) {
		case AST_EXPRESSION_TERMINAL_NAME:
		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_INTRINSIC:
			Assert();

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expr;

			switch (expr->type) {
				default: AssertUnreachable();

				case TYPE_INT64:
				case TYPE_INT32:
				case TYPE_INT16:
				case TYPE_INT8:
				case TYPE_UINT64:
				case TYPE_UINT32:
				case TYPE_UINT16:
				case TYPE_UINT8:
					result = literal->value_int;
					break;

				case TYPE_FLOAT32:
					result = literal->value_f32;
					break;

				case TYPE_FLOAT64:
					result = literal->value_f64;
					break;
			}
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* varexpr = (Ast_Expression_Variable*)expr;
			Ast_Variable* var = varexpr ->variable;
			result = var->ir_stack;
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
		case AST_EXPRESSION_TERMINAL_ARRAY_BEGIN:
		case AST_EXPRESSION_TERMINAL_ARRAY_END:
		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		case AST_EXPRESSION_BINARY_DOT:
			Assert();

		case AST_EXPRESSION_BINARY_ADD:
		{
			Ast_Expression_Binary* bin = (Ast_Expression_Binary*)expr;
			Value vleft  = ExpressionToIR(bin->left,  block, true, helper);
			Value vright = ExpressionToIR(bin->right, block, true, helper);

			if (IsInteger(bin->left->type)) {
				result = block->Add(vleft, vright);
				break;
			}

			if (bin->left->type == TYPE_FLOAT32) {
				result = block->AddF32(vleft, vright);
				break;
			}

		} break;

		case AST_EXPRESSION_BINARY_SUBTRACT:
		{
			Ast_Expression_Binary* bin = (Ast_Expression_Binary*)expr;
			Value vleft  = ExpressionToIR(bin->left,  block, true, helper);
			Value vright = ExpressionToIR(bin->right, block, true, helper);
			result = block->Sub(vleft, vright);
		} break;

		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		case AST_EXPRESSION_IF_ELSE:
			Assert();

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expr;
			result = ExpressionToIR(cast->subexpression, block, true, helper);
		} break;

		case AST_EXPRESSION_CALL:
		case AST_EXPRESSION_DOT_CALL:
		case AST_EXPRESSION_SUBSCRIPT:
		case AST_EXPRESSION_LAMBDA:
		case AST_EXPRESSION_TUPLE:
		case AST_EXPRESSION_ARRAY:
		case AST_EXPRESSION_FIXED_ARRAY:
		case AST_EXPRESSION_AS:
			Assert();
	}

	if (remove_reference && (expr->flags & AST_EXPRESSION_FLAG_REFERENTIAL)) {
		result = block->Load(result);
	}

	return result;
}

static void BranchToIR(Ast_Branch* branch, Block* bbreak, Block* bexit, IrGenHelper* helper) {
	Procedure* proc = helper->procedure;

	if (branch->else_branch && !branch->else_branch->entry_block)
		BranchToIR(branch->else_branch, bbreak, bexit, helper);

	if (branch->then_branch && !branch->then_branch->entry_block)
		BranchToIR(branch->then_branch, bbreak, bexit, helper);

	Assert(!branch->entry_block);

	branch->entry_block = proc->NewBlock();

	Block* else_block = bexit;
	Block* then_block = bexit;

	if (branch->else_branch)
		else_block = branch->else_branch->entry_block;

	if (branch->then_branch)
		then_block = branch->then_branch->entry_block;

	switch (branch->kind) {
		case AST_BRANCH_NAKED:
		{
			CodeToIR(&branch->code, branch->entry_block, bexit, bbreak, helper);
		} break;

		case AST_BRANCH_IF:
		{
			Block* body = proc->NewBlock();

			Value cond = ExpressionToIR(branch->if_condition, branch->entry_block, true, helper);
			branch->entry_block->Branch(cond, body, else_block);

			CodeToIR(&branch->code, body, then_block, bbreak, helper);
		} break;

		case AST_BRANCH_WHILE:
		{
			Block* b = branch->entry_block;
			Block* loop_head = branch->entry_block;
			Block* loop_body = proc->NewBlock();

			Value cond = ExpressionToIR(branch->if_condition, b, true, helper);
			b->Branch(cond, loop_body, else_block);

			if (branch->then_branch || branch->else_branch) {
				loop_head = proc->NewBlock();

				b = loop_head;
				Value cond = ExpressionToIR(branch->if_condition, b, true, helper);
				b->Branch(cond, loop_body, then_block);
			}

			CodeToIR(&branch->code, loop_body, loop_head, bbreak, helper);
		} break;

		case AST_BRANCH_FOR_VERBOSE:
		{
			Block* b = branch->entry_block;

			Ast_Variable* var = branch->for_verbose.variable;
			Value stack = b->Stack(GetTypeSize(var->type));

			if (var->assignment) {
				Value v = ExpressionToIR(var->assignment, b, true, helper);
				b->Store(stack, v);
			}
			else
			{
				b->Store(stack, 0);
			}

			Block* loop_head = proc->NewBlock();
			Block* loop_body = proc->NewBlock();
			Block* loop_post = proc->NewBlock();

			b->Jump(loop_head);

			b = loop_head;
			Value cond = ExpressionToIR(branch->for_verbose.condition, b, true, helper);
			b->Branch(cond, loop_body, else_block);

			if (branch->then_branch || branch->else_branch) {
				loop_head = proc->NewBlock();
				b = loop_head;
				Value cond = ExpressionToIR(branch->for_verbose.condition, b, true, helper);
				b->Branch(cond, loop_body, then_block);
			}

			b = loop_post;
			Value vnext = ExpressionToIR(branch->for_verbose.next, b, true, helper);
			b->Store(stack, vnext);
			b->Jump(loop_head);

			CodeToIR(&branch->code, loop_body, loop_post, bbreak, helper);
		} break;

		default:
			AssertUnreachable();
	}
}

static Block* BranchBlockToIR(Ast_BranchBlock* bb, Block* block, Block* bbreak, IrGenHelper* helper) {
	Block* exit_block = helper->procedure->NewBlock();

	BranchToIR(&bb->branches[0], bbreak, exit_block, helper);

	block->Jump(bb->branches[0].entry_block);

	return exit_block;
}

static Block* StatementToIR(Ast_Statement* statement, Block* block, Block* bbreak, IrGenHelper* helper) {
	switch (statement->kind) {
		case AST_STATEMENT_EXPRESSION:
		{
			Value value = ExpressionToIR(statement->expression, block, false, helper);
		} break;

		case AST_STATEMENT_VARIABLE_DECLARATION:
		{
			Ast_Variable* var = &statement->variable_declaration;
			Instruction* stack = block->Stack(GetTypeSize(var->type));
			var->ir_stack = stack;
			Value value = ExpressionToIR(var->assignment, block, true, helper);
			block->Store(stack, value);
		} break;

		case AST_STATEMENT_ASSIGNMENT:
		case AST_STATEMENT_ASSIGNMENT_ADD:
		case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
		case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
		case AST_STATEMENT_ASSIGNMENT_DIVIDE:
		case AST_STATEMENT_ASSIGNMENT_XOR:
		{
		} break;

		case AST_STATEMENT_INCREMENT:
		case AST_STATEMENT_DECREMENT:
		{
			Ast_Increment* inc = &statement->increment;
			bool is_inc = statement->kind == AST_STATEMENT_INCREMENT;
			Value refval = ExpressionToIR(inc->expression, block, false, helper);
			Value val = block->Load(refval);

			if (IsInteger(inc->expression->type)) {
				val = is_inc ? block->Add(val, 1) : block->Sub(val, 1);
			}
			else if (inc->expression->type == TYPE_FLOAT32) {
				val = is_inc ? block->AddF32(val, 1) : block->SubF32(val, 1);
			}
			else if (inc->expression->type == TYPE_FLOAT32) {
				val = is_inc ? block->AddF64(val, 1) : block->SubF64(val, 1);
			}
			else AssertUnreachable();

			block->Store(refval, val);
		} break;

		case AST_STATEMENT_RETURN:
		{
		} break;

		case AST_STATEMENT_BREAK:
		{
			block->Jump(bbreak);
		} break;

		case AST_STATEMENT_CLAIM:

		case AST_STATEMENT_BRANCH_BLOCK:
		{
			block = BranchBlockToIR(&statement->branch_block, block, bbreak, helper);
		} break;

		case AST_STATEMENT_DEFER:
		{
		} break;
	}

	return block;
}

static void CodeToIR(Ast_Code* code, Block* block, Block* bexit, Block* bbreak, IrGenHelper* helper) {
	Procedure* proc = helper->procedure;
	for (u64 i = 0; i < code->statements.length; i++) {
		Ast_Statement* statement = &code->statements[i];
		block = StatementToIR(statement, block, bbreak, helper);

		if (block->controlFlowInstruction)
			break;
	}

	block->Jump(bexit);
}

static Procedure* FunctionToIR(Ast_Function* function) {
	Procedure* proc = Allocate<Procedure>();
	ZeroMemory(proc);
	proc->function = function;
	function->procedure = proc;

	Block* binit = proc->NewBlock();

	for (u64 i = 0; i < function->parameters.length; i++) {
		Ast_Variable* var = &function->parameters[i];
		Instruction* stack = binit->Stack(GetTypeSize(var->type));
		var->ir_stack = stack;
		Instruction* param = binit->Param(i);
		binit->Store(stack, param);
	}

	Block* exit = proc->NewBlock();
	exit->Return();

	IrGenHelper helper = (IrGenHelper) {
		.procedure = proc,
	};

	CodeToIR(&function->code, binit, exit, null, &helper);

	return proc;
}


// -------------------------------------------------- //


static void GenerateIR(Ast_Module* module) {
	for (u64 i = 0; i < module->scope.functions; i++) {
		Ast_Function* function = &module->scope.functions[i];
		Procedure* procedure = FunctionToIR(function);
	}

	for (u64 i = 0; i < module->scope.functions; i++) {
		Ast_Function* function = &module->scope.functions[i];
		Print("%\n", function->procedure);
	}
}


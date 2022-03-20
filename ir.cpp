#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

static List<IrFunction*> ir_functions = null;

static void RemoveInstruction(IrFunction* function, IrInstructionID id)
{
	IrInstruction* instruction = GetInstruction(function, id);
	instruction->kind = IR_NOP;
	instruction->operands[0] = GetValue(function->free_instruction);
	function->free_instruction = id;
	GetBlock(function, instruction->block)->instructions.Remove(id);
}

static void DeclareDependency(IrFunction* function, IrInstructionID dependant, IrInstructionID dependee)
{
	Assert(dependant != IR_NONE);
	Assert(dependee  != IR_NONE);
	function->instructions[dependant].users.Add(dependee);
}

static void RemoveDependency(IrFunction* function, IrInstructionID dependant, IrInstructionID dependee)
{
	Assert(dependant != IR_NONE);
	Assert(dependee  != IR_NONE);
	function->instructions[dependant].users.Remove(dependee);
}

static IrInstructionID Insert(IrFunction* function, IrInstruction instruction)
{
	IrInstructionID id = function->free_instruction;

	if (id == IR_NONE)
	{
		id = function->instructions.count;
		function->instructions.Add(instruction);
	}
	else
	{
		function->free_instruction = function->instructions[id].operands[0].instruction;
		function->instructions[id] = instruction;
	}

	return id;
}

static IrValue Insert(IrFunction* function, IrBlockID block, Type* type, IrInstructionKind kind, IrValue operand0 = None(), IrValue operand1 = None(), IrValue operand2 = None())
{
	IrInstruction instruction;
	ZeroMemory(&instruction);

	instruction.kind = kind;
	instruction.type = type;
	instruction.block = block;

	instruction.operands[0] = operand0;
	instruction.operands[1] = operand1;
	instruction.operands[2] = operand2;

	IrInstructionID id = Insert(function, instruction);

	if (instruction.operands[0].kind == IR_VALUE_INSTRUCTION) DeclareDependency(function, GetInstruction(instruction.operands[0]), id);
	if (instruction.operands[1].kind == IR_VALUE_INSTRUCTION) DeclareDependency(function, GetInstruction(instruction.operands[1]), id);
	if (instruction.operands[2].kind == IR_VALUE_INSTRUCTION) DeclareDependency(function, GetInstruction(instruction.operands[2]), id);

	GetBlock(function, instruction.block)->instructions.Add(id);

	return GetValue(id);
}

static IrInstructionID InsertPhi(IrFunction* function, IrBlockID block, Type* type)
{
	IrInstruction instruction;
	ZeroMemory(&instruction);
	instruction.kind = IR_PHI;
	instruction.block = block;

	IrInstructionID id = Insert(function, instruction);
	GetBlock(function, block)->phis.Add(id);

	return id;
}

static void AddPhiEntry(IrFunction* function, IrInstructionID phi, IrPhiEntry entry)
{
	IrInstruction* instruction = GetInstruction(function, phi);
	Assert(GetInstruction(function, phi)->kind == IR_PHI);
	instruction->phi_entries.Add(entry);
}

static IrBlockID CreateBlock(IrFunction* function)
{
	IrIndex id = function->free_block.index;

	IrBlock block;
	ZeroMemory(&block);
	block.kind = IR_BLOCK_UNSPECIFIED;

	if (id != IR_NONE)
	{
		function->free_block = GetBlock(function, IrBlockID { id })->next;
	}
	else
	{
		id = function->blocks.count;
		function->blocks.Add(block);
	}

	function->blocks[id] = block;
	return IrBlockID { id };
}

static void RemoveBlock(IrFunction* function, IrBlockID id)
{
	Assert(id.index != IR_NONE);
	Assert(id.index < function->blocks.count);
	Assert(id.index != 0);

	IrBlock* block = GetBlock(function, id);
	block->kind = IR_BLOCK_FREE;
	block->next = function->free_block;

	FreeList(block->users);
	FreeList(block->instructions);
	FreeList(block->phis);

	block->users = null;
	block->instructions = null;
	block->phis = null;
}

static void Jump(IrFunction* function, IrBlockID from_id, IrBlockID to_id)
{
	IrBlock* from = GetBlock(function, from_id);
	Assert(from->kind == IR_BLOCK_UNSPECIFIED);
	from->kind = IR_BLOCK_JUMP;
	from->jump = to_id;
}

static void Branch(IrFunction* function, IrBlockID block_id, IrValue condition, IrBlockID true_block, IrBlockID false_block)
{
	IrBlock* block = GetBlock(function, block_id);
	Assert(block->kind == IR_BLOCK_UNSPECIFIED);
	block->kind = IR_BLOCK_BRANCH;
	block->branch.branch_instruction = GetInstruction(Insert(function, block_id, null, IR_BRANCH, condition));
	block->branch.true_branch = true_block;
	block->branch.false_branch = false_block;
}

static void Return(IrFunction* function, IrBlockID block_id, IrValue value)
{
	IrBlock* block = GetBlock(function, block_id);
	Assert(block->kind == IR_BLOCK_UNSPECIFIED);

	block->kind = IR_BLOCK_RETURN;
	block->return_instruction = GetInstruction(Insert(function, block_id, null, IR_RETURN, value));
}

static IrValue InsertCast(IrFunction* function, IrBlockID block, IrValue value, Type* from, Type* to)
{
	if (IsEnum(from)) from = from->enumeration->underlying_type;
	if (IsEnum(to))   to   = to->enumeration->underlying_type;

	if (from == to) return value;

	if (IsPointer(from) && IsPointer(to)) return value;

	if (from->kind == TYPE_BASETYPE_BOOL && IsInteger(to)) return Insert(function, block, to, IR_ZERO_EXTEND, value);

	if (IsInteger(from)                     && to->kind == TYPE_BASETYPE_BOOL) return Insert(function, block, to, IR_COMPARE_NOT_EQUAL, value, GetConstantInt(0));
	if (IsPointer(from)                     && to->kind == TYPE_BASETYPE_BOOL) return Insert(function, block, to, IR_COMPARE_NOT_EQUAL, value, GetConstantInt(0));
	if (from->kind == TYPE_BASETYPE_FLOAT32 && to->kind == TYPE_BASETYPE_BOOL) return Insert(function, block, to, IR_COMPARE_NOT_EQUAL, value, GetConstantFloat32(0));
	if (from->kind == TYPE_BASETYPE_FLOAT64 && to->kind == TYPE_BASETYPE_BOOL) return Insert(function, block, to, IR_COMPARE_NOT_EQUAL, value, GetConstantFloat64(0));

	if (from->kind == TYPE_BASETYPE_FLOAT32 && to->kind == TYPE_BASETYPE_FLOAT64) return Insert(function, block, to, IR_FLOAT_CAST, value);
	if (from->kind == TYPE_BASETYPE_FLOAT64 && to->kind == TYPE_BASETYPE_FLOAT32) return Insert(function, block, to, IR_FLOAT_CAST, value);
	if (from->kind == TYPE_BASETYPE_FLOAT32 && IsInteger(to)) return Insert(function, block, to, IR_FLOAT_TO_INT, value);
	if (from->kind == TYPE_BASETYPE_FLOAT64 && IsInteger(to)) return Insert(function, block, to, IR_FLOAT_TO_INT, value);

	if (IsInteger(from)         && from->kind == TYPE_BASETYPE_FLOAT32)     return Insert(function, block, to, IR_INT_TO_FLOAT, value);
	if (IsInteger(from)         && from->kind == TYPE_BASETYPE_FLOAT64)     return Insert(function, block, to, IR_INT_TO_FLOAT, value);
	if (IsInteger(from)         && IsInteger(to) && from->size == to->size) return value;
	if (IsInteger(from)         && IsInteger(to) && from->size >  to->size) return Insert(function, block, to, IR_TRUNCATE, value);
	if (IsSignedInteger(from)   && IsInteger(to) && from->size <  to->size) return Insert(function, block, to, IR_SIGN_EXTEND, value);
	if (IsUnsignedInteger(from) && IsInteger(to) && from->size <  to->size) return Insert(function, block, to, IR_ZERO_EXTEND, value);

	// if (to->kind == TYPE_BASETYPE_TUPLE && to->tuple.count == from->tuple.count)
	if (from->kind == TYPE_BASETYPE_TUPLE && to->kind == TYPE_BASETYPE_TUPLE)
	{
		Assert(from->tuple.count == to->tuple.count);

		for (uint32 i = 0; i < from->tuple.count; i++)
		{
			Type* from_element = from->tuple[i];
			Type* to_element   = to->tuple[i];
		}

		Assert();
	}

	// return to->kind == TYPE_BASETYPE_BOOL
	// 	|| CanImplicitCast(from, to->subtype);
	if (from->kind == TYPE_SPECIFIER_OPTIONAL)
	{
		Assert();
	}

	// return (to->kind == TYPE_SPECIFIER_FIXED_ARRAY
	// 	&& from->length == to->length
	// 	&& CanImplicitCast(from->subtype, to->subtype));
		// || (from->length == 1 && CanImplicitCast(from->subtype, to->subtype));
	if (from->kind == TYPE_SPECIFIER_FIXED_ARRAY)
	{
		Assert();
	}

	if (from->kind == TYPE_BASETYPE_BYTE) return value;

	Assert();
	return value;
}

static IrValue ExpressionToIR(IrFunction* function, IrBlockID& block, Ast_Expression* expression);

static IrValue ExpressionToIRLoad(IrFunction* function, IrBlockID& block, Ast_Expression* expression)
{
	IrValue value = ExpressionToIR(function, block, expression);

	if ((expression->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
	{
		value = Insert(function, block, expression->type, IR_LOAD, value);
	}

	return value;
}

static IrValue ExpressionToIR(IrFunction* function, IrBlockID& block, Ast_Expression* expression)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_NAME: Assert(); Unreachable();

		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_INTRINSIC:
		{
			Assert();
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			if (IsInteger(literal->type) || IsPointer(literal->type) || literal->type == &type_bool)
			{
				return GetConstantInt(literal->value_int);
			}
			else if (literal->type == &type_float64)
			{
				return GetConstantFloat64(literal->value_f64);
			}
			else if (literal->type == &type_float32)
			{
				return GetConstantFloat32(literal->value_f32);
			}
			else if (literal->type == &type_float16)
			{
				Assert();
				return GetConstantFloat32(literal->value_f16);
			}
			else Assert();
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* var = (Ast_Expression_Variable*)expression;
			return var->variable->ir;
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
		case AST_EXPRESSION_TERMINAL_ARRAY_BEGIN:
		case AST_EXPRESSION_TERMINAL_ARRAY_END:
		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Assert();
		} break;

		case AST_EXPRESSION_DYNAMIC_ARRAY:
		{
			Assert();
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Assert();
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left  = ExpressionToIRLoad(function, block, binary->left);
			IrValue right = ExpressionToIRLoad(function, block, binary->right);

			IrInstructionKind kind = binary->kind == AST_EXPRESSION_BINARY_COMPARE_EQUAL ? IR_COMPARE_EQUAL : IR_COMPARE_NOT_EQUAL;
			return Insert(function, block, &type_bool, kind, left, right);
		}

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left  = ExpressionToIRLoad(function, block, binary->left);
			IrValue right = ExpressionToIRLoad(function, block, binary->right);

			IrInstructionKind kind;

			if (IsFloat(binary->left->type))
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = IR_FLOAT_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = IR_FLOAT_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = IR_FLOAT_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = IR_FLOAT_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}
			else if (IsSignedInteger(binary->left->type))
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = IR_SIGNED_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = IR_SIGNED_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = IR_SIGNED_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = IR_SIGNED_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}
			else
			{
				Assert(IsUnsignedInteger(binary->left->type));
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = IR_UNSIGNED_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = IR_UNSIGNED_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = IR_UNSIGNED_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = IR_UNSIGNED_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}

			return Insert(function, block, &type_bool, kind, left, right);

		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrValue value = ExpressionToIRLoad(function, block, unary->subexpression);

			if (unary->type == &type_float64)
			{
				return Insert(function, block, unary->type, IR_FLOAT_SUBTRACT, GetConstantFloat64(0), value);
			}
			else if (unary->type == &type_float32)
			{
				return Insert(function, block, unary->type, IR_FLOAT_SUBTRACT, GetConstantFloat32(0), value);
			}
			else if (IsInteger(unary->type))
			{
				return Insert(function, block, unary->type, IR_INT_SUBTRACT, GetConstantInt(0), value);
			}

			Assert();
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return Insert(function, block, &type_bool, IR_COMPARE_EQUAL, ExpressionToIRLoad(function, block, unary->subexpression), GetConstantInt(0));
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrValue value = ExpressionToIRLoad(function, block, unary->subexpression);
			IrValue value_flipped = Insert(function, block, unary->type, IR_INT_SUBTRACT, GetConstantInt(0), value);
			IrValue is_negative = Insert(function, block, &type_bool, IR_SIGNED_COMPARE_LESS, value, GetConstantInt(0));
			return Insert(function, block, unary->type, IR_SELECT, is_negative, value_flipped, value);
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return Insert(function, block, unary->type, IR_NOT, ExpressionToIRLoad(function, block, unary->subexpression));
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return ExpressionToIR(function, block, unary->subexpression);
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return ExpressionToIR(function, block, unary->subexpression);
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			IrInstructionKind kind;

			IrValue left = ExpressionToIRLoad(function, block, binary->left);
			IrValue right = ExpressionToIRLoad(function, block, binary->right);

			if (IsPointer(binary->left->type) && IsInteger(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_ADD)
			{
				return Insert(function, block, binary->type, IR_INDEX, left, right);
			}
			else if (IsPointer(binary->left->type) && IsInteger(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
			{
				right = Insert(function, block, binary->right->type, IR_INT_SUBTRACT, GetConstantInt(0), right);
				return Insert(function, block, binary->type, IR_INDEX, left, right);
			}
			else if (IsPointer(binary->left->type) && IsPointer(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
			{
				left = Insert(function, block, binary->left->type, IR_INT_SUBTRACT, left, right);
				return Insert(function, block, binary->type, IR_UNSIGNED_DIVIDE, left, GetConstantInt(binary->left->type->size));
			}
			else if (IsInteger(binary->type))
			{
				bool is_signed = IsSignedInteger(binary->type);

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_ADD:         kind = IR_INT_ADD;         break;
					case AST_EXPRESSION_BINARY_SUBTRACT:    kind = IR_INT_SUBTRACT;    break;
					case AST_EXPRESSION_BINARY_MULTIPLY:    kind = IR_INT_MULTIPLY;    break;
					case AST_EXPRESSION_BINARY_DIVIDE:      kind = is_signed ? IR_SIGNED_DIVIDE : IR_UNSIGNED_DIVIDE;  break;
					case AST_EXPRESSION_BINARY_MODULO:      kind = is_signed ? IR_SIGNED_MODULO : IR_UNSIGNED_MODULO;  break;
					case AST_EXPRESSION_BINARY_EXPONENTIAL: Assert(); break;
					case AST_EXPRESSION_BINARY_LEFT_SHIFT:  kind = is_signed ? IR_SIGNED_LEFT_SHIFT  : IR_UNSIGNED_LEFT_SHIFT;  break;
					case AST_EXPRESSION_BINARY_RIGHT_SHIFT: kind = is_signed ? IR_SIGNED_RIGHT_SHIFT : IR_UNSIGNED_RIGHT_SHIFT; break;
					case AST_EXPRESSION_BINARY_BITWISE_OR:  kind = IR_OR;  break;
					case AST_EXPRESSION_BINARY_BITWISE_XOR: kind = IR_XOR; break;
					case AST_EXPRESSION_BINARY_BITWISE_AND: kind = IR_AND; break;
					default: Assert(); Unreachable();
				}

				return Insert(function, block, binary->type, kind, left, right);
			}
			else
			{
				Assert(IsFloat(binary->type));

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_ADD:         kind = IR_FLOAT_ADD;      break;
					case AST_EXPRESSION_BINARY_SUBTRACT:    kind = IR_FLOAT_SUBTRACT; break;
					case AST_EXPRESSION_BINARY_MULTIPLY:    kind = IR_FLOAT_MULTIPLY; break;
					case AST_EXPRESSION_BINARY_DIVIDE:      kind = IR_FLOAT_DIVIDE;   break;
					case AST_EXPRESSION_BINARY_EXPONENTIAL: Assert(); break;
					default: Assert(); Unreachable();
				}

				return Insert(function, block, binary->type, kind, left, right);
			}
		} break;

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			return InsertCast(function, block, ExpressionToIRLoad(function, block, cast->subexpression), cast->subexpression->type, cast->type);
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			Assert(tuple->elements.count);

			if (tuple->elements.count == 1)
			{
				return ExpressionToIR(function, block, tuple->elements[0]);
			}
			else
			{
				Assert();
			}

		} break;

		case AST_EXPRESSION_BINARY_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left = ExpressionToIRLoad(function, block, binary->left);
			IrBlockID right_block = CreateBlock(function);
			IrValue right = ExpressionToIRLoad(function, right_block, binary->right);

			IrBlockID post = CreateBlock(function);

			Branch(function, block, left, right_block, post);
			Jump(function, right_block, post);

			IrInstructionID phi = InsertPhi(function, post, &type_bool);
			AddPhiEntry(function, phi, IrPhiEntry { block, GetConstantInt(0) });
			AddPhiEntry(function, phi, IrPhiEntry { right_block, right });

			block = post;

			return GetValue(phi);
		} break;

		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left = ExpressionToIRLoad(function, block, binary->left);
			IrBlockID right_block = CreateBlock(function);
			IrValue right = ExpressionToIRLoad(function, right_block, binary->right);

			IrBlockID post = CreateBlock(function);

			Branch(function, block, left, post, right_block);
			Jump(function, right_block, post);

			IrInstructionID phi = InsertPhi(function, post, &type_bool);
			AddPhiEntry(function, phi, IrPhiEntry { block, GetConstantInt(1) });
			AddPhiEntry(function, phi, IrPhiEntry { right_block, right });

			block = post;

			return GetValue(phi);
		} break;

		case AST_EXPRESSION_CALL:
		{
			Assert();
		} break;

		case AST_EXPRESSION_DOT_CALL:
		{
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;

			if (IsPointer(subscript->array->type))
			{
				IrValue pointer = ExpressionToIRLoad(function, block, subscript->array);
				IrValue index   = ExpressionToIRLoad(function, block, subscript->index);
				return Insert(function, block, subscript->array->type, IR_INDEX, pointer, index);
			}
			else
			{
				Assert();
			}

		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			Assert();
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			return InsertCast(function, block, ExpressionToIRLoad(function, block, as->expression), as->expression->type, as->type);
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Assert();
		} break;
	}

	Assert();
	return None();
}

struct IrGeneratorInfo
{
	IrBlockID return_block;
	IrValue   return_stack;
};

static void CodeToIR(IrFunction* function, IrBlockID block, Ast_Code* code, IrBlockID exit_block, IrBlockID break_block, IrGeneratorInfo* generator_info)
{
	for (uint32 j = 0; j < code->statements.count; j++)
	{
		Ast_Statement* statement = &code->statements[j];

		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* branch_block = &statement->branch_block;

				for (Ast_Branch* branch = branch_block->branches.Begin(); branch < branch_block->branches.End(); branch++)
				{
					branch->ir = CreateBlock(function);
				}

				Jump(function, block, branch_block->branches[0].ir);
				IrBlockID post_block = CreateBlock(function);

				for (Ast_Branch* branch = branch_block->branches.Begin(); branch < branch_block->branches.End(); branch++)
				{
					bool clean = !branch->else_branch && !branch->then_branch;

					IrBlockID else_block = branch->else_branch ? branch->else_branch->ir : post_block;
					IrBlockID then_block = branch->then_branch ? branch->then_branch->ir : post_block;
					IrBlockID head_block = branch->ir;
					IrBlockID body_block = CreateBlock(function);

					switch (branch->kind)
					{
						case AST_BRANCH_IF:
						{
							CodeToIR(function, body_block, &branch->code, then_block, break_block, generator_info);

							IrValue condition = ExpressionToIRLoad(function, head_block, branch->if_condition);
							Branch(function, head_block, condition, body_block, else_block);
						} break;

						case AST_BRANCH_WHILE:
						{
							if (clean)
							{
								CodeToIR(function, body_block, &branch->code, head_block, then_block, generator_info);

								IrValue head_condition = ExpressionToIRLoad(function, head_block, branch->while_condition);
								Branch(function, head_block, head_condition, body_block, else_block);
							}
							else
							{
								IrBlockID tail_block = CreateBlock(function);
								CodeToIR(function, body_block, &branch->code, tail_block, then_block, generator_info);

								IrValue head_condition = ExpressionToIRLoad(function, head_block, branch->while_condition);
								Branch(function, head_block, head_condition, body_block, else_block);

								IrValue tail_condition = ExpressionToIRLoad(function, tail_block, branch->while_condition);
								Branch(function, tail_block, tail_condition, body_block, then_block);
							}
						} break;

						case AST_BRANCH_FOR_RANGE:
						{
							Assert();
						} break;

						case AST_BRANCH_FOR_VERBOSE:
						{
							Assert();
						} break;

						case AST_BRANCH_NAKED:
						{
							CodeToIR(function, branch->ir, &branch->code, then_block, break_block, generator_info);
						} break;
					}
				}

				block = post_block;
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				ExpressionToIR(function, block, statement->expression);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_Variable* var = &statement->variable_declaration;

				IrValue stack = Insert(function, block, var->type, IR_STACK);
				var->ir = stack;

				if (var->assignment)
				{
					IrValue value = ExpressionToIRLoad(function, block, var->assignment);
					Insert(function, block, var->type, IR_STORE, stack, value);
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				IrValue left = ExpressionToIR(function, block, assignment->left);
				IrValue right = ExpressionToIRLoad(function, block, assignment->right);
				Assert(assignment->left->type->kind != TYPE_BASETYPE_TUPLE);
				// @Todo: Handle tuple assignment.
				Insert(function, block, assignment->left->type, IR_STORE, left, right);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL:
			{
				Ast_Assignment* assignment = &statement->assignment;
				IrValue left_address = ExpressionToIR(function, block, assignment->left);
				IrValue left_value = Insert(function, block, assignment->left->type, IR_LOAD, left_address);
				IrValue right = ExpressionToIRLoad(function, block, assignment->right);

				IrValue new_value = None();

				if (IsPointer(assignment->left->type))
				{
					Assert(IsInteger(assignment->right->type));

					if (statement->kind == AST_STATEMENT_ASSIGNMENT_SUBTRACT)
					{
						right = Insert(function, block, assignment->right->type, IR_INT_SUBTRACT, GetConstantInt(0), right);
					}

					new_value = Insert(function, block, assignment->left->type, IR_INDEX, left_value, right);
				}
				else if (IsFloat(assignment->left->type))
				{
					IrInstructionKind kind;

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:      kind = IR_FLOAT_ADD;      break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT: kind = IR_FLOAT_SUBTRACT; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY: kind = IR_FLOAT_MULTIPLY; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:   kind = IR_FLOAT_DIVIDE;   break;
						case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL:
						default: Assert();
					}

					new_value = Insert(function, block, assignment->left->type, kind, left_value, right);
				}
				else
				{
					Assert(IsInteger(assignment->left->type));

					IrInstructionKind kind;

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:      kind = IR_INT_ADD;      break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT: kind = IR_INT_SUBTRACT; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY: kind = IR_INT_MULTIPLY; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:   kind = IsSignedInteger(assignment->left->type) ? IR_SIGNED_DIVIDE : IR_UNSIGNED_DIVIDE; break;
						case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL:
						default: Assert();
					}

					new_value = Insert(function, block, assignment->left->type, kind, left_value, right);
				}

				Insert(function, block, assignment->left->type, IR_STORE, left_address, new_value);
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;

				IrValue address = ExpressionToIR(function, block, inc->expression);
				IrValue value = Insert(function, block, inc->expression->type, IR_LOAD, address);

				bool direction = statement->kind == AST_STATEMENT_INCREMENT;

				if (IsPointer(inc->expression->type))
				{
					value = Insert(function, block, inc->expression->type, IR_INDEX, value, direction ? GetConstantInt(1) : GetConstantInt(-1));
				}
				else if (IsInteger(inc->expression->type))
				{
					value = Insert(function, block, inc->expression->type, direction ? IR_INT_ADD : IR_INT_SUBTRACT, value, GetConstantInt(1));
				}
				else if (inc->expression->type == &type_float32)
				{
					value = Insert(function, block, inc->expression->type, direction ? IR_FLOAT_ADD : IR_FLOAT_SUBTRACT, value, GetConstantFloat32(1));
				}
				else if (inc->expression->type == &type_float64)
				{
					value = Insert(function, block, inc->expression->type, direction ? IR_FLOAT_ADD : IR_FLOAT_SUBTRACT, value, GetConstantFloat64(1));
				}
				else Assert();

				Insert(function, block, inc->expression->type, IR_STORE, address, value);
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Return* ret = &statement->ret;

				if (ret->expression)
				{
					Insert(function, block, ret->expression->type, IR_STORE, generator_info->return_stack, ExpressionToIRLoad(function, block, ret->expression));
				}

				Jump(function, block, generator_info->return_block);
			} return;

			case AST_STATEMENT_BREAK:
			{
				Jump(function, block, break_block);
			} return;

			case AST_STATEMENT_CLAIM:
			{
				// Insert 'etherial' instructions?
				Assert();
			} break;

			case AST_STATEMENT_DEFER:
			{
				Assert();
			} break;
		}
	}

	if (IsValid(exit_block))
	{
		Jump(function, block, exit_block);
	}
}

static IrFunction* FunctionToIR(Ast_Function* function)
{
	if (!function->ir)
	{
		IrFunction* ir = Allocate<IrFunction>();
		ZeroMemory(ir);

		function->ir = ir;

		ir->id = ir_functions.count;
		ir->function = function;
		ir->instructions = AllocateList<IrInstruction>(64);
		ir->free_block.index = IR_NONE;
		ir->free_instruction = IR_NONE;
		ir_functions.Add(ir);

		IrBlockID initial_block = CreateBlock(ir);
		IrBlockID return_block  = CreateBlock(ir);

		bool does_return_value = function->return_type != &empty_tuple;

		IrGeneratorInfo generator_info;
		generator_info.return_block = return_block;
		generator_info.return_stack = None();

		if (does_return_value)
		{
			generator_info.return_stack = Insert(ir, initial_block, GetPointer(function->return_type), IR_STACK);
		}

		for (uint32 i = 0; i < function->parameters.count; i++)
		{
			Ast_Variable* param = &function->parameters[i];
			param->ir = Insert(ir, initial_block, GetPointer(param->type), IR_PARAMETER, GetConstantInt(i));
		}

		CodeToIR(ir, initial_block, &function->code, return_block, { IR_NONE }, &generator_info);

		IrValue return_value = None();

		if (does_return_value)
		{
			return_value = Insert(ir, return_block, function->return_type, IR_LOAD, generator_info.return_stack);
		}

		Return(ir, return_block, return_value);

		Print("%\n", ir);
	}

	return function->ir;
}

static void GenerateIR(Ast_Module* module)
{
	// Print("module->scope.functions.count = %\n", module->scope.functions.count);
	for (uint32 i = 0; i < module->scope.functions.count; i++)
	{
		Ast_Function* function = &module->scope.functions[i];
		FunctionToIR(function);
	}
}


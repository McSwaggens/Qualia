#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

List<IrFunctionInfo*> ir_functions = null;

void RemoveLogic(IrLogicIndex logic_index, IrFunctionInfo* function)
{
	IrLogic* logic = &function->logic[logic_index];
	logic->kind = LOGOS_NOP;
	logic->operands[0] = function->free_logic;
	function->free_logic = logic_index;
	function->blocks[logic->block].logic.Remove(logic_index);
}

void DeclareDependency(IrIndex dependant, IrLogicIndex dependee, IrFunctionInfo* function)
{
	if (dependant) return;
	function->logic[dependant].users.Add(dependee);
}

void RemoveDependency(IrIndex dependant, IrLogicIndex dependee, IrFunctionInfo* function)
{
	if (dependant) return;
	function->logic[dependant].users.Remove(dependee);
}

IrLogicIndex InsertLogic(IrLogic logic, IrFunctionInfo* function, IrLogicIndex block)
{
	IrLogicIndex index;

	logic.block = block;

	if (function->free_logic)
	{
		index = function->logic.count;
		function->logic.Add(logic);
	}
	else
	{
		index = function->free_logic;
		function->free_logic = function->logic[index].operands[0];
		function->logic[index] = logic;
	}

	GetBlockInfo(block, function)->logic.Add(index);

	DeclareDependency(logic.operands[0], index, function);
	DeclareDependency(logic.operands[1], index, function);
	DeclareDependency(logic.operands[2], index, function);

	return index;
}

IrLogicIndex InsertLogic(IrLogicKind kind, Type* type, IrIndex operand0, IrIndex operand1, IrIndex operand2, IrFunctionInfo* function, IrLogicIndex block)
{
	IrLogic logic;
	ZeroMemory(&logic);

	logic.kind = kind;
	logic.type = type;
	logic.operands[0] = operand0;
	logic.operands[1] = operand1;
	logic.operands[2] = operand2;

	return InsertLogic(logic, function, block);
}

IrLogicIndex InsertLogic(IrLogicKind kind, Type* type, IrIndex operand0, IrIndex operand1, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(kind, type, operand0, operand1, IR_NONE, function, block);
}

IrLogicIndex InsertLogic(IrLogicKind kind, Type* type, IrIndex operand0, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(kind, type, operand0, IR_NONE, IR_NONE, function, block);
}

IrLogicIndex InsertLogic(IrLogicKind kind, Type* type, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(kind, type, IR_NONE, IR_NONE, IR_NONE, function, block);
}

IrLogicIndex GetConstantBool(bool b, IrFunctionInfo* function)
{
	if (function->bool_cache[b] != IR_NONE)
	{
		return function->bool_cache[b];
	}

	IrLogic logic;
	ZeroMemory(&logic);
	logic.kind = LOGOS_CONSTANT;
	logic.type = &type_bool;
	logic.constant_bool = b;
	logic.operands[0] = IR_NONE;
	logic.operands[1] = IR_NONE;
	logic.operands[2] = IR_NONE;

	IrLogicIndex index = InsertLogic(logic, function, 0); // @Bug: Block 0 may not be valid.
	function->constants.Add(index);
	function->bool_cache[b] = index;

	return index;
}

IrLogicIndex GetConstantInt(s64 n, IrFunctionInfo* function)
{
	IrLogicIndex* constant_index_pointer = null;

	if ((u64)(n+1) < 3 && function->int_cache[n+1] != IR_NONE)
	{
		return function->int_cache[n+1];
	}

	for (u32 i = 0; i < function->constants.count; i++)
	{
		IrLogicIndex index = function->constants[i];
		IrLogic* logic = &function->logic[index];

		if (logic->constant_int64 == n && logic->type == &type_int64) // @FixMe: type_int64
		{
			return index;
		}
	}

	IrLogic logic;
	ZeroMemory(&logic);
	logic.kind = LOGOS_CONSTANT;
	logic.type = &type_int64;
	logic.constant_int64 = n;
	logic.operands[0] = IR_NONE;
	logic.operands[1] = IR_NONE;
	logic.operands[2] = IR_NONE;

	IrLogicIndex index = InsertLogic(logic, function, 0); // @Bug: Block 0 may not be valid.
	function->constants.Add(index);

	if ((u64)(n+1) < 3)
	{
		function->int_cache[n+1] = index;
	}

	return index;
}

IrLogicIndex GetConstantFloat32(f32 f, IrFunctionInfo* function)
{
	for (u32 i = 0; i < function->constants.count; i++)
	{
		IrLogicIndex index = function->constants[i];
		IrLogic* constant = &function->logic[index];

		if (constant->constant_float32 == f && constant->type == &type_float32)
		{
			return index;
		}
	}

	IrLogic constant;
	ZeroMemory(&constant);
	constant.kind = LOGOS_CONSTANT;
	constant.type = &type_float32;
	constant.constant_float32 = f;

	IrLogicIndex index = InsertLogic(constant, function, 0); // @Bug: Block 0 may not be valid.
	function->constants.Add(index);

	return index;
}

IrLogicIndex InsertInversion(Type* type, IrIndex v, IrFunctionInfo* function, IrLogicIndex block)
{
	Assert(IsInteger(type));
	return InsertLogic(LOGOS_SUBTRACT, type, v, GetConstantInt(0, function), function, block);
}

// @RemoveMe?
IrLogicIndex InsertStack(Type* type, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(LOGOS_STACK, type, function, block);
}

// @RemoveMe?
IrLogicIndex InsertParameter(Type* type, u32 parameter_index, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(LOGOS_PARAMETER, type, parameter_index, function, block);
}

// @RemoveMe?
IrLogicIndex InsertLoad(Type* type, IrIndex address, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(LOGOS_LOAD, type, address, function, block);
}

// @RemoveMe?
IrLogicIndex InsertStore(Type* type, IrIndex address, IrIndex value, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(LOGOS_STORE, type, address, value, function, block);
}

// @RemoveMe?
IrLogicIndex InsertCopy(Type* type, IrIndex destination, IrIndex source, IrFunctionInfo* function, IrLogicIndex block)
{
	return InsertLogic(LOGOS_COPY, type, destination, source, function, block);
}

IrLogicIndex InsertBranch(IrIndex condition, IrIndex true_block, IrIndex false_block, IrFunctionInfo* function, IrLogicIndex block)
{
	IrLogicIndex branch = InsertLogic(LOGOS_BRANCH, null, condition, true_block, false_block, function, block);
	GetBlockInfo(block, function)->branch = branch;
	return branch;
}

IrLogicIndex InsertJump(IrLogicIndex to_block, IrFunctionInfo* function, IrLogicIndex block)
{
	IrLogicIndex jump = InsertLogic(LOGOS_JUMP, null, to_block, function, block);
	GetBlockInfo(block, function)->branch = jump;
	return jump;
}

IrLogicIndex InsertReturnValue(IrLogicIndex value, IrFunctionInfo* function, IrLogicIndex block)
{
	IrLogicIndex ret = InsertLogic(LOGOS_RETURN, null, value, function, block);
	GetBlockInfo(block, function)->branch = ret;
	return ret;
}

void RemoveBlock(IrLogicIndex block_index, IrFunctionInfo* function)
{
	IrBlockInfo* block = GetBlockInfo(block_index, function);
	block->next = function->free_block;
	FreeList(block->logic);
}

IrLogicIndex CreateBlock(IrFunctionInfo* function)
{
	IrBlockInfoIndex block_index = function->free_block;

	if (block_index == IR_NONE)
	{
		block_index = function->blocks.count;

		IrBlockInfo block;
		ZeroMemory(&block);

		block.next = IR_NONE;
		block.branch = IR_NONE;
		function->blocks.Add(block);
	}
	else
	{
		function->free_block = function->blocks[block_index].next;
		ZeroMemory(&function->blocks[block_index]);
		function->blocks[block_index].branch = IR_NONE;
		function->blocks[block_index].next = IR_NONE;
	}

	IrLogicIndex rep = InsertLogic(LOGOS_BLOCK, null, function, block_index);
	function->blocks[block_index].representation = rep;

	return rep;
}

IrLogicIndex InsertConversion(IrLogicIndex value, Type* from, Type* to, IrFunctionInfo* function, IrLogicIndex block)
{
	if (from == to) return value;

	if (IsFloat(from) && IsFloat(to))
	{
		return InsertLogic(LOGOS_FLOAT_CONVERT, to, value, function, block);
	}

	if (IsInteger(from) && IsFloat(to))
	{
		return InsertLogic(LOGOS_INT_TO_FLOAT, to, value, function, block);
	}

	if (IsFloat(from) && IsInteger(to))
	{
		return InsertLogic(LOGOS_FLOAT_TO_INT, to, value, function, block);
	}

	Assert();
	return value;
}

IrIndex GenerateIR(Ast_Expression* expression, IrFunctionInfo* function, IrLogicIndex& block);

IrIndex GenerateIRLoad(Ast_Expression* expression, IrFunctionInfo* function, IrLogicIndex& block)
{
	IrLogicIndex value = GenerateIR(expression, function, block);

	if (expression->is_referential_value)
	{
		value = InsertLoad(expression->type, value, function, block);
	}

	return value;
}

IrLogicIndex GenerateIR(Ast_Expression* expression, IrFunctionInfo* function, IrLogicIndex& block)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_NAME: Assert(); Unreachable();

		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION:
		{
			Assert();
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			if (IsPointer(literal->type))
			{
				return GetConstantInt(literal->value_int64, function);
			}
			else if (IsInteger(literal->type))
			{
				return GetConstantInt(literal->value_int64, function);
			}
			else if (literal->type == &type_float32)
			{
				return GetConstantFloat32(literal->value_float32, function);
			}
			else if (literal->type == &type_bool)
			{
				return GetConstantBool(literal->value_bool, function);
			}
			else
			{
				Assert();
			}
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* var = (Ast_Expression_Variable*)expression;
			return var->variable->stack;
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
		case AST_EXPRESSION_TERMINAL_ARRAY_DATA:
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

			IrLogicIndex left = GenerateIRLoad(binary->left, function, block);
			IrLogicIndex right = GenerateIRLoad(binary->right, function, block);

			IrLogicKind logos = binary->kind == AST_EXPRESSION_BINARY_COMPARE_EQUAL ? LOGOS_COMPARE_EQUAL : LOGOS_COMPARE_NOT_EQUAL;
			return InsertLogic(logos, &type_bool, left, right, function, block);
		}

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrLogicIndex left = GenerateIRLoad(binary->left, function, block);
			IrLogicIndex right = GenerateIRLoad(binary->right, function, block);

			IrLogicKind kind;

			if (IsFloat(binary->left->type))
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = LOGOS_FLOAT_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = LOGOS_FLOAT_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = LOGOS_FLOAT_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = LOGOS_FLOAT_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}
			else if (IsSignedInteger(binary->left->type))
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = LOGOS_SIGNED_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = LOGOS_SIGNED_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = LOGOS_SIGNED_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = LOGOS_SIGNED_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}
			else
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = LOGOS_UNSIGNED_COMPARE_LESS;             break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = LOGOS_UNSIGNED_COMPARE_LESS_OR_EQUAL;    break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = LOGOS_UNSIGNED_COMPARE_GREATER;          break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = LOGOS_UNSIGNED_COMPARE_GREATER_OR_EQUAL; break;
					default: Unreachable();
				}
			}

			return InsertLogic(kind, &type_bool, left, right, function, block);

		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			Assert();
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return InsertLogic(LOGOS_COMPARE_EQUAL, &type_bool, GenerateIRLoad(unary->subexpression, function, block), GetConstantBool(false, function), function, block);
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrLogicKind kind;

			switch (unary->kind)
			{
				case AST_EXPRESSION_UNARY_BITWISE_NOT: kind = LOGOS_BITWISE_NOT; break;
				case AST_EXPRESSION_UNARY_NOT:         Assert(); break; // compare_equal(false)
				case AST_EXPRESSION_UNARY_PLUS:        Assert(); break;
				default: Assert(); Unreachable();
			}

			return InsertLogic(kind, unary->type, GenerateIRLoad(unary->subexpression, function, block), function, block);
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return GenerateIR(unary->subexpression, function, block);
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return GenerateIR(unary->subexpression, function, block);
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			IrLogicKind kind;

			IrLogicIndex left = GenerateIRLoad(binary->left, function, block);
			IrLogicIndex right = GenerateIRLoad(binary->right, function, block);

			if (IsPointer(binary->left->type) && IsInteger(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_ADD)
			{
				return InsertLogic(LOGOS_INDEX, binary->type, left, right, function, block);
			}
			else if (IsPointer(binary->left->type) && IsInteger(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
			{
				return InsertLogic(LOGOS_INDEX, binary->type, left, InsertInversion(binary->right->type, right, function, block), function, block);
			}
			else if (IsPointer(binary->left->type) && IsPointer(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
			{
				left = InsertLogic(LOGOS_SUBTRACT, binary->left->type, left, right, function, block);
				return InsertLogic(LOGOS_DIVIDE, binary->type, left, GetConstantInt(binary->left->type->size, function), function, block);
			}
			else
			{
				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_ADD:         kind = LOGOS_ADD;         break;
					case AST_EXPRESSION_BINARY_SUBTRACT:    kind = LOGOS_SUBTRACT;    break;
					case AST_EXPRESSION_BINARY_MULTIPLY:    kind = LOGOS_MULTIPLY;    break;
					case AST_EXPRESSION_BINARY_DIVIDE:      kind = LOGOS_DIVIDE;      break;
					case AST_EXPRESSION_BINARY_MODULO:      kind = LOGOS_MODULO;      break;
					case AST_EXPRESSION_BINARY_EXPONENTIAL: kind = LOGOS_EXPONENTIAL; break;
					case AST_EXPRESSION_BINARY_BITWISE_OR:  kind = LOGOS_BITWISE_OR;  break;
					case AST_EXPRESSION_BINARY_BITWISE_XOR: kind = LOGOS_BITWISE_XOR; break;
					case AST_EXPRESSION_BINARY_BITWISE_AND: kind = LOGOS_BITWISE_AND; break;
					case AST_EXPRESSION_BINARY_LEFT_SHIFT:  kind = LOGOS_BITWISE_LEFT_SHIFT;  break;
					case AST_EXPRESSION_BINARY_RIGHT_SHIFT: kind = LOGOS_BITWISE_RIGHT_SHIFT; break;
					default: Assert(); Unreachable();
				}

				return InsertLogic(kind, binary->type, left, right, function, block);
			}
		} break;

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			return InsertConversion(GenerateIR(cast->subexpression, function, block), cast->subexpression->type, cast->type, function, block);
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			Assert(tuple->elements.count);

			if (tuple->elements.count == 1)
			{
				return GenerateIR(tuple->elements[0], function, block);
			}
			else
			{
				Assert();
			}

		} break;

		case AST_EXPRESSION_BINARY_AND:
		{
			Assert();
		} break;

		case AST_EXPRESSION_BINARY_OR:
		{
			Assert();
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
			Assert();
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			Assert();
		} break;

		case AST_EXPRESSION_AS:
		{
			Assert();
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Assert();
		} break;
	}

	return IR_NONE;
}

IrLogicIndex GenerateIR(Ast_Code* code, IrFunctionInfo* function, IrLogicIndex initial_block, IrLogicIndex exit_block, IrLogicIndex break_block)
{
	IrLogicIndex block = initial_block;

	for (u32 j = 0; j < code->statements.count; j++)
	{
		Ast_Statement* statement = &code->statements[j];

		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* branch_block = &statement->branch_block;
				IrLogicIndex post_block = CreateBlock(function);

				IrLogicIndex blocks[branch_block->branches.count];

				if (branch_block->branches[0].kind == AST_BRANCH_IF)
				{
					blocks[0] = block;
				}
				else
				{
					blocks[0] = CreateBlock(function);
					InsertJump(blocks[0], function, block);
				}

				for (u32 i = 1; i < branch_block->branches.count; i++)
				{
					blocks[i] = CreateBlock(function);
				}

				for (u32 i = 0; i < branch_block->branches.count; i++)
				{
					Ast_Branch* branch = &branch_block->branches[i];

					IrLogicIndex false_block = branch->else_branch_index != AST_BRANCH_INDEX_NONE
						? branch->else_branch_index
						: post_block;

					IrLogicIndex condition_block = blocks[i];

					switch (branch->kind)
					{
						case AST_BRANCH_IF:
						{
							IrLogicIndex true_block = GenerateIR(
								&branch->code,
								function,
								CreateBlock(function),
								branch->then_branch_index != AST_BRANCH_INDEX_NONE ? blocks[branch->then_branch_index] : post_block,
								post_block);

							IrLogicIndex condition_value = GenerateIR(branch->condition, function, condition_block);
							InsertBranch(condition_value, true_block, false_block, function, condition_block);
						} break;

						case AST_BRANCH_WHILE:
						{
							Assert();
							IrLogicIndex true_block = GenerateIR(
								&branch->code,
								function,
								CreateBlock(function),
								branch->then_branch_index != AST_BRANCH_INDEX_NONE
									? blocks[branch->then_branch_index]
									: post_block,
								post_block);

							IrLogicIndex false_block = branch->else_branch_index != AST_BRANCH_INDEX_NONE
								? blocks[branch->else_branch_index]
								: post_block;

							IrLogicIndex initial_condition_value = GenerateIR(branch->condition, function, condition_block);
							InsertBranch(initial_condition_value, true_block, false_block, function, condition_block);

							IrLogicIndex inner_condition_block = CreateBlock(function);

							if (branch->then_branch_index != AST_BRANCH_INDEX_NONE)
							{
								IrLogicIndex initial_condition_value = GenerateIR(branch->condition, function, condition_block);
								InsertBranch(initial_condition_value, true_block, false_block, function, condition_block);
							}

						} break;

						case AST_BRANCH_FOR_COUNT:
						{
							Assert();
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
							GenerateIR(&branch->code, function, blocks[i], post_block, break_block);
						} break;
					}
				}

				block = post_block;
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				GenerateIR(statement->expression, function, block);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* var = &statement->variable_declaration;

				IrLogicIndex stack = InsertStack(var->type, function, block);
				var->stack = stack;

				if (var->assignment)
				{
					IrIndex value = GenerateIR(var->assignment, function, block);
					InsertStore(var->type, stack, value, function, block);
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				IrIndex left = GenerateIR(assignment->left, function, block);
				IrIndex right = GenerateIRLoad(assignment->right, function, block);
				InsertStore(assignment->left->type, left, right, function, block);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;
				IrIndex left_address = GenerateIR(assignment->left, function, block);
				IrIndex left_value = InsertLoad(assignment->left->type, left_address, function, block);
				IrIndex right = GenerateIRLoad(assignment->right, function, block);

				IrIndex new_value;

				if (IsPointer(assignment->left->type))
				{
					Assert(IsInteger(assignment->right->type));
					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:
						{
							new_value = InsertLogic(LOGOS_INDEX, assignment->left->type, left_value, right, function, block);
						} break;

						case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
						{
							right = InsertInversion(assignment->right->type, right, function, block);
							new_value = InsertLogic(LOGOS_INDEX, assignment->left->type, left_value, right, function, block);
						} break;

						default: Assert(); Unreachable();
					}
				}
				else
				{
					IrLogicKind kind;

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:      kind = LOGOS_ADD;         break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT: kind = LOGOS_SUBTRACT;    break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY: kind = LOGOS_MULTIPLY;    break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:   kind = LOGOS_DIVIDE;      break;
						case AST_STATEMENT_ASSIGNMENT_POWER:    kind = LOGOS_EXPONENTIAL; break;
						default: Assert(); Unreachable();
					}

					new_value = InsertLogic(kind, assignment->left->type, left_value, right, function, block);
				}

				InsertStore(assignment->left->type, left_address, new_value, function, block);
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;

				bool direction = statement->kind == AST_STATEMENT_INCREMENT;
				IrIndex address = GenerateIR(inc->expression, function, block);
				IrIndex value = InsertLoad(inc->expression->type, address, function, block);

				if (IsPointer(inc->expression->type))
				{
					value = InsertLogic(LOGOS_INDEX, inc->expression->type, value, GetConstantInt(direction ? 1 : -1, function), function, block);
				}
				else if (IsInteger(inc->expression->type))
				{
					value = InsertLogic(direction ? LOGOS_ADD : LOGOS_SUBTRACT, inc->expression->type, value, GetConstantInt(1, function), function, block);
				}
				else if (inc->expression->type == &type_float32)
				{
					value = InsertLogic(direction ? LOGOS_ADD : LOGOS_SUBTRACT, inc->expression->type, value, GetConstantFloat32(1, function), function, block);
				}
				else Assert();

				InsertStore(inc->expression->type, address, value, function, block);
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Return* ret = &statement->ret;

				if (ret->expression)
				{
					IrIndex value = GenerateIRLoad(ret->expression, function, block);
					InsertLogic(LOGOS_RETURN, function->function->type->output, value, function, block);
				}

				InsertLogic(LOGOS_RETURN, function->function->type->output, function, block);
			} return initial_block;

			case AST_STATEMENT_BREAK:
			{
				InsertJump(break_block, function, block);
			} return initial_block;

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

	if (exit_block != IR_NONE)
	{
		InsertJump(exit_block, function, block);
	}

	return initial_block;
}

IrFunctionInfoIndex GenerateIR(Ast_Function* function)
{
	if (function->ir_index == IR_NONE)
	{
		IrFunctionInfo* ir = Allocate<IrFunctionInfo>();
		ZeroMemory(ir);

		ir->bool_cache[0] = IR_NONE;
		ir->bool_cache[1] = IR_NONE;

		ir->int_cache[0] = IR_NONE;
		ir->int_cache[1] = IR_NONE;
		ir->int_cache[2] = IR_NONE;

		ir->float32_cache[0] = IR_NONE;
		ir->float32_cache[1] = IR_NONE;
		ir->float32_cache[2] = IR_NONE;

		ir->float64_cache[0] = IR_NONE;
		ir->float64_cache[1] = IR_NONE;
		ir->float64_cache[2] = IR_NONE;

		ir->id = ir_functions.count;
		ir->function = function;
		ir->free_block = IR_NONE;
		ir->free_logic = IR_NONE;
		ir_functions.Add(ir);

		IrLogicIndex initial_block = CreateBlock(ir);
		IrLogicIndex return_block = CreateBlock(ir);
		InsertLogic(LOGOS_RETURN, null, ir, return_block);

		GenerateIR(&function->code, ir, initial_block, return_block, IR_NONE);
		Print("%\n", ir);
	}

	return function->ir_index;
}

void GenerateIR(Ast_Module* module)
{
	for (u32 i = 0; i < module->scope.functions.count; i++)
	{
		Ast_Function* function = &module->scope.functions[i];
		GenerateIR(function);
	}
}


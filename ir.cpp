#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

IrValue MakeConstU8(u8 n)   { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_uint8;   v.value_uint8   = n; return v; }
IrValue MakeConstU16(u16 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_uint16;  v.value_uint16  = n; return v; }
IrValue MakeConstU32(u32 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_uint32;  v.value_uint32  = n; return v; }
IrValue MakeConstU64(u64 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_uint64;  v.value_uint64  = n; return v; }
IrValue MakeConstS8(s8 n)   { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_int8;    v.value_int8    = n; return v; }
IrValue MakeConstS16(s16 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_int16;   v.value_int16   = n; return v; }
IrValue MakeConstS32(s32 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_int32;   v.value_int32   = n; return v; }
IrValue MakeConstS64(s64 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_int64;   v.value_int64   = n; return v; }
IrValue MakeConstF32(f32 f) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_float32; v.value_float32 = f; return v; }
IrValue MakeConstF64(f64 f) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_float64; v.value_float64 = f; return v; }
IrValue MakeConstBool(bool b) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_CONSTANT; v.type = &type_bool;  v.value_bool    = b; return v; }
IrValue NoValue() { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_NONE; return v; }

IrValue ConvertToIR(Ast_Expression* expression, IrBlock* block);

IrValue Dependency(IrValue dependency, IrInstruction* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_NONE: break;
		case IR_VALUE_INSTRUCTION: dependency.instruction->users.Add(user); break;
		case IR_VALUE_CONSTANT: break;
		case IR_VALUE_LARGE_CONSTANT: break;
		case IR_VALUE_GLOBAL: Assert();
	}

	return dependency;
}

void RemoveDependency(IrValue dependency, IrInstruction* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_NONE: break;
		case IR_VALUE_INSTRUCTION: dependency.instruction->users.Remove(user); break;
		case IR_VALUE_CONSTANT: break;
		case IR_VALUE_LARGE_CONSTANT: break;
		case IR_VALUE_GLOBAL: Assert();
	}
}

IrInstruction* AllocateIrInstruction(IrBlock* block)
{
	if (block->bucket_head)
	{
		IrInstruction* result = block->bucket_head;
		block->bucket_head = block->bucket_head->next;
		return result;
	}

	IrInstruction_Bucket* bucket = Allocate<IrInstruction_Bucket>();
	ZeroMemory(bucket);
	bucket->next = block->bucket;

	for (u32 i = 1; i < IR_INSTRUCTION_BUCKET_COUNT-1; i++)
	{
		IrInstruction* instruction = bucket->instructions + i;
		instruction->next = instruction + 1;
	}

	bucket->instructions[IR_INSTRUCTION_BUCKET_COUNT-1].next = null;
	block->bucket = bucket;
	block->bucket_head = bucket->instructions + 1;

	return bucket->instructions;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrBlock* block)
{
	IrInstruction* instruction = AllocateIrInstruction(block);
	ZeroMemory(instruction);
	instruction->kind = kind;
	instruction->type = type;
	instruction->id = type ? block->function->register_id_counter++ : -1;
	instruction->block_id = block->id;
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, block);
	instruction->a = Dependency(a, instruction);
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrValue b, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, a, block);
	instruction->b = Dependency(b, instruction);
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrValue b, IrValue c, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, a, b, block);
	instruction->c = Dependency(c, instruction);
	return instruction;
}

IrValue GetValue(IrInstruction* instruction)
{
	IrValue v;
	ZeroMemory(&v);
	v.kind = IR_VALUE_INSTRUCTION;
	v.instruction = instruction;
	v.type = instruction->type;
	return v;
}

IrBlock* NewBlock(IrFunction* function)
{
	IrBlock* block = Allocate<IrBlock>();
	ZeroMemory(block);
	block->id = function->block_id_counter++; // Can't use function->blocks.count because of branch ordering nonsense.
	block->function = function;
	function->blocks.Add(block);
	return block;
}

IrValue PushLoad(IrValue address, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(address.type->size <= 8);
	return GetValue(PushInstruction(IR_INSTRUCTION_LOAD, address.type->subtype, address, block));
}

IrValue CastValue(IrValue v, Type* to, IrBlock* block)
{
	Type* from = v.type;
	if (from == to) return v;

	if (IsSignedInteger(from) && IsSignedInteger(to) && from->size < to->size)
	{
		return GetValue(PushInstruction(IR_INSTRUCTION_SIGN_EXTEND, to, v, block));
	}

	if (IsInteger(from) && IsInteger(to) ||
		IsPointer(from) && IsPointer(to) ||
		IsInteger(from) && IsEnum(to))
	{
		v.type = to;
		return v;
	}

	Assert();

	return v;
}

void PushStore(IrValue address, IrValue value, IrBlock* block)
{
	value = CastValue(value, address.type->subtype, block);

	// Print("address.type->subtype = %\n", address.type->subtype);
	// Print("value.type = %\n", value.type);

	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(value.type);
	Assert(address.type->subtype == value.type);

	PushInstruction(IR_INSTRUCTION_STORE, null, address, value, block);
}

void PushCopy(IrValue destination, IrValue source, IrBlock* block)
{
	Assert(destination.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(destination.type == source.type);

	PushInstruction(IR_INSTRUCTION_COPY, null, destination, source, block);
}

IrValue LoadConvertToIR(Ast_Expression* expression, IrBlock* block)
{
	IrValue v = ConvertToIR(expression, block);

	if (expression->is_referential_value)
	{
		return PushLoad(v, block);
	}

	return v;
}

void DeclareUser(IrBlock* user, IrBlock* block)
{
	block->users.Add(user);
}

void PushJump(IrBlock* from, IrBlock* to)
{
	Assert(!from->control);
	IrInstruction* instruction = PushInstruction(IR_INSTRUCTION_JUMP, null, from);
	instruction->branch_a = to;
	DeclareUser(from, to);
	from->control = instruction;
}

void PushBranch(IrBlock* from, IrValue condition, IrBlock* block_true, IrBlock* block_false)
{
	Assert(!from->control);
	IrInstruction* instruction = PushInstruction(IR_INSTRUCTION_BRANCH, null, condition, from);
	instruction->branch_a = block_true;
	instruction->branch_b = block_false;
	DeclareUser(from, block_true);
	DeclareUser(from, block_false);
	from->control = instruction;
}

void PushReturn(IrBlock* from)
{
	PushInstruction(IR_INSTRUCTION_RETURN, null, from);
}

IrValue PushTest(IrValue v, IrBlock* block)
{
	IrValue zero = MakeConstU64(0);
	zero.type = v.type;
	return GetValue(PushInstruction(IR_INSTRUCTION_COMPARE_NOT_EQUAL, &type_bool, v, zero, block));
}

IrValue TrimPointers(IrValue v, IrBlock* block)
{
	Assert(v.type);
	Assert(v.type->kind == TYPE_SPECIFIER_POINTER);

	while (v.type->subtype->kind == TYPE_SPECIFIER_POINTER)
	{
		v = PushLoad(v, block);
	}

	return v;
}

IrValue ConvertToIR(Ast_Expression* expression, IrBlock* block)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL:
		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION:
			Assert();

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			IrValue v;
			ZeroMemory(&v);
			v.kind = IR_VALUE_CONSTANT;
			v.type = literal->type;

			switch (literal->type->kind)
			{
				case TYPE_BASETYPE_BOOL:    v.value_bool    = literal->value_bool;    break;
				case TYPE_BASETYPE_UINT8:   v.value_uint8   = literal->value_uint8;   break;
				case TYPE_BASETYPE_UINT16:  v.value_uint16  = literal->value_uint16;  break;
				case TYPE_BASETYPE_UINT32:  v.value_uint32  = literal->value_uint32;  break;
				case TYPE_BASETYPE_UINT64:  v.value_uint64  = literal->value_uint64;  break;
				case TYPE_BASETYPE_INT8:    v.value_int8    = literal->value_int8;    break;
				case TYPE_BASETYPE_INT16:   v.value_int16   = literal->value_int16;   break;
				case TYPE_BASETYPE_INT32:   v.value_int32   = literal->value_int32;   break;
				case TYPE_BASETYPE_INT64:   v.value_int64   = literal->value_int64;   break;
				// case TYPE_BASETYPE_FLOAT16: v.value_float16 = literal->value_float16; break;
				case TYPE_BASETYPE_FLOAT32: v.value_float32 = literal->value_float32; break;
				case TYPE_BASETYPE_FLOAT64: v.value_float64 = literal->value_float64; break;
				default: Assert();
			}

			return v;
		};

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_VariableDeclaration* var = ((Ast_Expression_Variable*)expression)->variable;
			return var->address;
		};

		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_ARRAY_DATA:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
			Assert();

		case AST_EXPRESSION_FIXED_ARRAY:
			Assert();

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrValue subval = LoadConvertToIR(unary->subexpression, block);

			IrInstruction_Kind kind;
			switch (expression->kind)
			{
				case AST_EXPRESSION_UNARY_BITWISE_NOT: kind = IR_INSTRUCTION_BITWISE_NOT; break;
				case AST_EXPRESSION_UNARY_NOT:         kind = IR_INSTRUCTION_NOT;         break;
				case AST_EXPRESSION_UNARY_MINUS:       kind = IR_INSTRUCTION_FLIP_SIGN;   break;
				case AST_EXPRESSION_UNARY_PLUS:        kind = IR_INSTRUCTION_POSITIVE;    break;
				default: Unreachable();
			}

			return GetValue(PushInstruction(kind, expression->type, subval, block));
		}

		case AST_EXPRESSION_UNARY_VALUE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return ConvertToIR(unary->subexpression, block);
		};

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
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
			IrValue left = LoadConvertToIR(binary->left, block);
			IrValue right = LoadConvertToIR(binary->right, block);

			IrInstruction_Kind kind;

			switch (expression->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            kind = IR_INSTRUCTION_COMPARE_EQUAL;            break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        kind = IR_INSTRUCTION_COMPARE_NOT_EQUAL;        break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = IR_INSTRUCTION_COMPARE_LESS;             break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = IR_INSTRUCTION_COMPARE_LESS_OR_EQUAL;    break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = IR_INSTRUCTION_COMPARE_GREATER;          break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = IR_INSTRUCTION_COMPARE_GREATER_OR_EQUAL; break;
				case AST_EXPRESSION_BINARY_ADD:                      kind = IR_INSTRUCTION_ADD;                      break;
				case AST_EXPRESSION_BINARY_SUBTRACT:                 kind = IR_INSTRUCTION_SUBTRACT;                 break;
				case AST_EXPRESSION_BINARY_MULTIPLY:                 kind = IR_INSTRUCTION_MULTIPLY;                 break;
				case AST_EXPRESSION_BINARY_DIVIDE:                   kind = IR_INSTRUCTION_DIVIDE;                   break;
				case AST_EXPRESSION_BINARY_MODULO:                   kind = IR_INSTRUCTION_MODULO;                   break;
				case AST_EXPRESSION_BINARY_EXPONENTIAL:              kind = IR_INSTRUCTION_EXPONENTIAL;              break;
				case AST_EXPRESSION_BINARY_BITWISE_OR:               kind = IR_INSTRUCTION_BITWISE_OR;               break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR:              kind = IR_INSTRUCTION_BITWISE_XOR;              break;
				case AST_EXPRESSION_BINARY_BITWISE_AND:              kind = IR_INSTRUCTION_BITWISE_AND;              break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:               kind = IR_INSTRUCTION_BITWISE_LEFT_SHIFT;       break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT:              kind = IR_INSTRUCTION_BITWISE_RIGHT_SHIFT;      break;
				default: Unreachable();
			}

			return GetValue(PushInstruction(kind, expression->type, left, right, block));
		} break;

		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			// case AST_EXPRESSION_BINARY_AND: kind = IR_INSTRUCTION_AND; break;
			// case AST_EXPRESSION_BINARY_OR:  kind = IR_INSTRUCTION_OR;  break;
			Assert();
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM)
			{
				Ast_Enum_Member* member = ((Ast_Expression_Enum_Member*)binary->right)->member;
				Assert(member->expression->type);
				return ConvertToIR(member->expression, block); // @Todo @FixMe: This should be constantly evaluated before IR.
			}

			IrValue left = TrimPointers(ConvertToIR(binary->left, block), block);
			Ast_Struct_Member* member = ((Ast_Expression_Struct_Member*)binary->right)->member;

			return GetValue(PushInstruction(IR_INSTRUCTION_MEMBER, GetPointer(member->type.type), left, MakeConstU64(member->index), block));
		};

		case AST_EXPRESSION_CALL:
		{
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			IrValue array = LoadConvertToIR(subscript->array, block);
			IrValue index = LoadConvertToIR(subscript->index, block);

			return GetValue(PushInstruction(IR_INSTRUCTION_ELEMENT, GetPointer(subscript->type), array, index, block));
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Assert();
		} break;

		case AST_EXPRESSION_LAMBDA:
		case AST_EXPRESSION_TUPLE:
		case AST_EXPRESSION_AS:
			Assert();
	}

	Assert();
	Unreachable();
}

IrBlock* ConvertToIR(Ast_Code* code, IrBlock* block, IrBlock* exit_block, IrBlock* break_block, IrFunction* function)
{
	if (!block)
	{
		block = NewBlock(function);
	}

	IrBlock* initial_block = block;

	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* branch_block = &statement->branch_block;

				for (Ast_Branch* branch = branch_block->branches; branch < branch_block->branches.End(); branch++)
				{
					branch->initial_condition_block = NewBlock(function);
				}

				PushJump(block, branch_block->branches[0].initial_condition_block);

				IrBlock* exit_block = NewBlock(function);

				for (Ast_Branch* branch = branch_block->branches; branch < branch_block->branches.End(); branch++)
				{
					IrBlock* else_branch = exit_block;
					IrBlock* then_branch = exit_block;

					if (branch->else_branch)
					{
						else_branch = branch->else_branch->initial_condition_block;
					}

					if (branch->then_branch)
					{
						then_branch = branch->then_branch->initial_condition_block;
					}

					if (branch->condition)
					{

						if (branch->token->kind == TOKEN_WHILE)
						{
							IrBlock* loop_block = NewBlock(function);
							IrBlock* inner_block = ConvertToIR(&branch->code, null, loop_block, then_branch, function);

							{
								IrValue condition = LoadConvertToIR(branch->condition, branch->initial_condition_block);
								IrValue test = PushTest(condition, branch->initial_condition_block);

								PushBranch(branch->initial_condition_block, test, inner_block, else_branch);
							}

							{
								IrValue condition = LoadConvertToIR(branch->condition, loop_block);
								IrValue test = PushTest(condition, loop_block);

								PushBranch(loop_block, test, inner_block, then_branch);
							}
						}
						else
						{
							Assert(branch->token->kind == TOKEN_IF);

							IrBlock* inner_block = ConvertToIR(&branch->code, null, then_branch, break_block, function);
							IrValue condition = LoadConvertToIR(branch->condition, branch->initial_condition_block);
							IrValue test = PushTest(condition, branch->initial_condition_block);

							PushBranch(branch->initial_condition_block, test, else_branch, inner_block);
						}
					}
					else
					{
						ConvertToIR(&branch->code, branch->initial_condition_block, exit_block, break_block, function);
					}
				}

				block = exit_block;
			} break;

			case AST_STATEMENT_DEFER:
			{
				Assert();
				ConvertToIR(&statement->defer.code, null, exit_block, break_block, function);
			} break;

			case AST_STATEMENT_CLAIM:
			{
				Assert();
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Return* ret = &statement->ret;

				IrValue v = ConvertToIR(ret->expression, block);
				// @Todo: PushStore return value or something...
				PushReturn(block);
				return initial_block;
			} break;

			case AST_STATEMENT_BREAK:
			{
				Assert(break_block);
				PushJump(block, break_block);
				return initial_block;
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				bool direction = statement->kind == AST_STATEMENT_INCREMENT;

				IrValue address = ConvertToIR(inc->expression, block);
				IrValue value = PushLoad(address, block);

				IrValue one;

				switch (inc->expression->type->kind)
				{
					case TYPE_SPECIFIER_POINTER: one = MakeConstU64(1); break; // @FixMe: IR_INSTRUCTION_ELEMENT
					case TYPE_BASETYPE_UINT8:    one = MakeConstU8(1);  break;
					case TYPE_BASETYPE_UINT16:   one = MakeConstU16(1); break;
					case TYPE_BASETYPE_UINT32:   one = MakeConstU32(1); break;
					case TYPE_BASETYPE_UINT64:   one = MakeConstU64(1); break;
					case TYPE_BASETYPE_INT8:     one = MakeConstS8(1);  break;
					case TYPE_BASETYPE_INT16:    one = MakeConstS16(1); break;
					case TYPE_BASETYPE_INT32:    one = MakeConstS32(1); break;
					case TYPE_BASETYPE_INT64:    one = MakeConstS64(1); break;
					// case TYPE_BASETYPE_FLOAT16: one = MakeConstF16(1.0); break;
					case TYPE_BASETYPE_FLOAT32:  one = MakeConstF32(1.0f); break;
					case TYPE_BASETYPE_FLOAT64:  one = MakeConstF64(1.0);  break;
					default: Assert(); Unreachable();
				}

				value = GetValue(PushInstruction(direction ? IR_INSTRUCTION_ADD : IR_INSTRUCTION_SUBTRACT, inc->expression->type, value, one, block));
				PushStore(address, value, block);
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				ConvertToIR(statement->expression, block);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* var = &statement->variable_declaration;

				IrValue address = GetValue(PushInstruction(IR_INSTRUCTION_STACK_ALLOCATE, GetPointer(var->type), MakeConstU64(var->type->size), block));
				var->address = address;

				if (var->assignment)
				{
					IrValue v = LoadConvertToIR(var->assignment, block);
					PushStore(address, v, block);
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;

				IrValue left = ConvertToIR(assignment->left, block);
				IrValue right = LoadConvertToIR(assignment->right, block);

				PushStore(left, right, block);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;
			} break;
		}
	}

	if (exit_block)
	{
		PushJump(block, exit_block);
	}
	else
	{
		PushReturn(block);
	}

	return initial_block;
}

void ConvertToIR(Ast_Function* function)
{
	IrFunction ir_function;
	ZeroMemory(&ir_function);
	ir_function.function = function;

	IrBlock* entry_block = NewBlock(&ir_function);

	for (u32 i = 0; i < function->parameters.count; i++)
	{
		Ast_VariableDeclaration* param = &function->parameters[i];
		if (param->type->size <= 8)
		{
			param->address = GetValue(PushInstruction(IR_INSTRUCTION_STACK_ALLOCATE, GetPointer(param->type), MakeConstU64(param->type->size), entry_block));
			IrValue v = GetValue(PushInstruction(IR_INSTRUCTION_PARAMETER, param->type, MakeConstU64(i), entry_block));
			PushStore(param->address, v, entry_block);
		}
		else
		{
			param->address = GetValue(PushInstruction(IR_INSTRUCTION_PARAMETER, GetPointer(param->type), MakeConstU64(i), entry_block));
		}
	}

	ConvertToIR(&function->code, entry_block, null, null, &ir_function);

	Print("%\n", &ir_function);

}

void ConvertToIR(Ast_Root* root)
{
	for (Ast_Function* function = root->scope.functions; function < root->scope.functions.End(); function++)
	{
		ConvertToIR(function);
	}
}


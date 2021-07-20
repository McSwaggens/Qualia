#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

IrValue MakeConstU8(u8 n)   { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_uint8;   v.value_uint8   = n; return v; }
IrValue MakeConstU16(u16 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_uint16;  v.value_uint16  = n; return v; }
IrValue MakeConstU32(u32 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_uint32;  v.value_uint32  = n; return v; }
IrValue MakeConstU64(u64 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_uint64;  v.value_uint64  = n; return v; }
IrValue MakeConstS8(s8 n)   { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_int8;    v.value_int8    = n; return v; }
IrValue MakeConstS16(s16 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_int16;   v.value_int16   = n; return v; }
IrValue MakeConstS32(s32 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_int32;   v.value_int32   = n; return v; }
IrValue MakeConstS64(s64 n) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_int64;   v.value_int64   = n; return v; }
IrValue MakeConstF32(f32 f) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_float32; v.value_float32 = f; return v; }
IrValue MakeConstF64(f64 f) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_float64; v.value_float64 = f; return v; }
IrValue MakeConstBool(bool b) { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_INLINED_CONSTANT; v.type = &type_bool;  v.value_bool    = b; return v; }
IrValue NoValue() { IrValue v; ZeroMemory(&v); v.kind = IR_VALUE_UNUSED; return v; }

IrValue ConvertToIR(Ast_Expression* expression, IrBlock* block, IrContext* context);

IrValue Dependency(IrValue dependency, IrOperation* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_REGISTER: dependency.reg->users.Add(user);      break;
		case IR_VALUE_BLOCK:    dependency.block->users.Add(user);    break;
		case IR_VALUE_ARGUMENT: dependency.argument->users.Add(user); break;

		case IR_VALUE_CONSTANT:
			Assert();

		case IR_VALUE_UNUSED:
		case IR_VALUE_INLINED_CONSTANT:
			break;
	}

	return dependency;
}

void RemoveDependency(IrValue dependency, IrOperation* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_CONSTANT:
			Assert();

		case IR_VALUE_REGISTER: dependency.reg->users.Remove(user);      break;
		case IR_VALUE_BLOCK:    dependency.block->users.Remove(user);    break;
		case IR_VALUE_ARGUMENT: dependency.argument->users.Remove(user); break;

		case IR_VALUE_UNUSED:
		case IR_VALUE_INLINED_CONSTANT:
			break;
	}
}

IrOperation* PushOperation(IrOperation_Kind kind, Type* type, IrBlock* block, IrContext* context)
{
	IrOperation* op = Allocate<IrOperation>();
	ZeroMemory(op);
	op->kind = kind;
	op->type = type;
	op->id = type ? context->function->register_id_counter++ : -1;
	block->operations.Add(op);
	return op;
}

IrOperation* PushOperation(IrOperation_Kind kind, Type* type, IrValue a, IrBlock* block, IrContext* context)
{
	IrOperation* op = PushOperation(kind, type, block, context);
	op->a = Dependency(a, op);
	return op;
}

IrOperation* PushOperation(IrOperation_Kind kind, Type* type, IrValue a, IrValue b, IrBlock* block, IrContext* context)
{
	IrOperation* op = PushOperation(kind, type, a, block, context);
	op->b = Dependency(b, op);
	return op;
}

IrOperation* PushOperation(IrOperation_Kind kind, Type* type, IrValue a, IrValue b, IrValue c, IrBlock* block, IrContext* context)
{
	IrOperation* op = PushOperation(kind, type, a, b, block, context);
	op->c = Dependency(c, op);
	return op;
}

IrValue GetValue(IrOperation* op)
{
	IrValue v;
	ZeroMemory(&v);
	v.kind = IR_VALUE_REGISTER;
	v.reg = op;
	v.type = op->type;
	return v;
}

IrArgument* PushArgument(Type* type, IrBlock* block)
{
	IrArgument* argument = Allocate<IrArgument>();
	ZeroMemory(argument);
	argument->id = block->function->register_id_counter++;
	argument->block = block;
	argument->type = type;
	block->arguments.Add(argument);
	return argument;
}

IrValue GetValue(IrArgument* argument)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_ARGUMENT;
	value.argument = argument;
	value.type = argument->type;
	return value;
}

IrBlock* NewBlock(Ast_Function* function, IrContext* context)
{
	IrBlock* block = Allocate<IrBlock>();
	ZeroMemory(block);
	block->id = function->block_id_counter++; // Can't use function->blocks.count because of branch ordering nonsense.
	block->function = function;
	function->blocks.Add(block);
	return block;
}

IrValue GetValue(IrBlock* block)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_BLOCK;
	value.block = block;
	return value;
}

IrValue LoadOp(IrValue address, IrBlock* block, IrContext* context)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	return GetValue(PushOperation(OPERATION_LOAD, address.type->subtype, address, block, context));
}

IrValue CastValue(IrValue v, Type* to, IrBlock* block, IrContext* context)
{
	Type* from = v.type;
	if (from == to) return v;

	if (IsSignedInteger(from) && IsSignedInteger(to) && from->size < to->size)
	{
		return GetValue(PushOperation(OPERATION_SIGN_EXTEND, to, v, block, context));
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

void StoreOp(IrValue address, IrValue value, IrBlock* block, IrContext* context)
{
	value = CastValue(value, address.type->subtype, block, context);

	// Print("address.type->subtype = %\n", address.type->subtype);
	// Print("value.type = %\n", value.type);

	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(value.type);
	Assert(address.type->subtype == value.type);

	PushOperation(OPERATION_STORE, null, address, value, block, context);
}

IrValue LoadConvertToIR(Ast_Expression* expression, IrBlock* block, IrContext* context)
{
	IrValue v = ConvertToIR(expression, block, context);

	if (expression->is_referential_value)
	{
		return LoadOp(v, block, context);
	}

	return v;
}

void PushJumpOp(IrBlock* from, IrBlock* to, IrContext* context)
{
	PushOperation(OPERATION_JUMP, null, GetValue(to), from, context);
}

IrValue PushTestOp(IrValue v, IrBlock* block, IrContext* context)
{
	return GetValue(PushOperation(OPERATION_COMPARE_NOT_EQUAL, &type_bool, v, MakeConstBool(false), block, context));
}

IrValue TrimPointers(IrValue v, IrBlock* block, IrContext* context)
{
	Assert(v.type);
	Assert(v.type->kind == TYPE_SPECIFIER_POINTER);

	while (v.type->subtype->kind == TYPE_SPECIFIER_POINTER)
	{
		v = LoadOp(v, block, context);
	}

	return v;
}

IrValue ConvertToIR(Ast_Expression* expression, IrBlock* block, IrContext* context)
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
			v.kind = IR_VALUE_INLINED_CONSTANT;
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
			IrValue subval = LoadConvertToIR(unary->subexpression, block, context);

			IrOperation_Kind kind;
			switch (expression->kind)
			{
				case AST_EXPRESSION_UNARY_BITWISE_NOT: kind = OPERATION_BITWISE_NOT; break;
				case AST_EXPRESSION_UNARY_NOT:         kind = OPERATION_NOT;         break;
				case AST_EXPRESSION_UNARY_MINUS:       kind = OPERATION_FLIP_SIGN;   break;
				case AST_EXPRESSION_UNARY_PLUS:        kind = OPERATION_POSITIVE;    break;
				default: Unreachable();
			}

			return GetValue(PushOperation(kind, expression->type, subval, block, context));
		}

		case AST_EXPRESSION_UNARY_VALUE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			return ConvertToIR(unary->subexpression, block, context);
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
			IrValue left = LoadConvertToIR(binary->left, block, context);
			IrValue right = LoadConvertToIR(binary->right, block, context);

			IrOperation_Kind kind;

			switch (expression->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            kind = OPERATION_COMPARE_EQUAL;            break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        kind = OPERATION_COMPARE_NOT_EQUAL;        break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = OPERATION_COMPARE_LESS;             break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = OPERATION_COMPARE_LESS_OR_EQUAL;    break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = OPERATION_COMPARE_GREATER;          break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = OPERATION_COMPARE_GREATER_OR_EQUAL; break;
				case AST_EXPRESSION_BINARY_ADD:                      kind = OPERATION_ADD;                      break;
				case AST_EXPRESSION_BINARY_SUBTRACT:                 kind = OPERATION_SUBTRACT;                 break;
				case AST_EXPRESSION_BINARY_MULTIPLY:                 kind = OPERATION_MULTIPLY;                 break;
				case AST_EXPRESSION_BINARY_DIVIDE:                   kind = OPERATION_DIVIDE;                   break;
				case AST_EXPRESSION_BINARY_MODULO:                   kind = OPERATION_MODULO;                   break;
				case AST_EXPRESSION_BINARY_EXPONENTIAL:              kind = OPERATION_EXPONENTIAL;              break;
				case AST_EXPRESSION_BINARY_BITWISE_OR:               kind = OPERATION_BITWISE_OR;               break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR:              kind = OPERATION_BITWISE_XOR;              break;
				case AST_EXPRESSION_BINARY_BITWISE_AND:              kind = OPERATION_BITWISE_AND;              break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:               kind = OPERATION_BITWISE_LEFT_SHIFT;       break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT:              kind = OPERATION_BITWISE_RIGHT_SHIFT;      break;
				default: Unreachable();
			}

			return GetValue(PushOperation(kind, expression->type, left, right, block, context));
		} break;

		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			// case AST_EXPRESSION_BINARY_AND: kind = OPERATION_AND; break;
			// case AST_EXPRESSION_BINARY_OR:  kind = OPERATION_OR;  break;
			Assert();
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM)
			{
				Ast_Enum_Member* member = ((Ast_Expression_Enum_Member*)binary->right)->member;
				Assert(member->expression->type);
				return ConvertToIR(member->expression, block, context); // @Todo @FixMe: This should be constantly evaluated before IR.
			}

			IrValue left = TrimPointers(ConvertToIR(binary->left, block, context), block, context);
			Ast_Struct_Member* member = ((Ast_Expression_Struct_Member*)binary->right)->member;

			return GetValue(PushOperation(OPERATION_MEMBER, GetPointer(member->type.type, &context->stack), left, MakeConstU64(member->index), block, context));
		};

		case AST_EXPRESSION_CALL:
		{
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			IrValue array = LoadConvertToIR(subscript->array, block, context);
			IrValue index = LoadConvertToIR(subscript->index, block, context);

			return GetValue(PushOperation(OPERATION_ELEMENT, GetPointer(subscript->type, &context->stack), array, index, block, context));
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

IrValue ConvertToIR(Ast_Code* code, IrBlock* block, IrBlock* exit_block, IrBlock* break_block, Ast_Function* function, IrContext* context)
{
	if (!block)
	{
		block = NewBlock(function, context);
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
					branch->initial_condition_block = NewBlock(function, context);
				}

				PushJumpOp(block, branch_block->branches[0].initial_condition_block, context);

				IrBlock* exit_block = NewBlock(function, context);
				IrValue  exit_block_value = GetValue(exit_block);

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
							IrBlock* loop_block = NewBlock(function, context);
							IrValue inner_block = ConvertToIR(&branch->code, null, loop_block, then_branch, function, context);

							{
								IrValue condition = LoadConvertToIR(branch->condition, branch->initial_condition_block, context);
								IrValue test = PushTestOp(condition, branch->initial_condition_block, context);

								PushOperation(OPERATION_BRANCH, null, test, inner_block, GetValue(else_branch), branch->initial_condition_block, context);
							}

							{
								IrValue condition = LoadConvertToIR(branch->condition, loop_block, context);
								IrValue test = PushTestOp(condition, loop_block, context);

								PushOperation(OPERATION_BRANCH, null, test, inner_block, GetValue(then_branch), loop_block, context);
							}
						}
						else
						{
							Assert(branch->token->kind == TOKEN_IF);

							IrValue inner_block = ConvertToIR(&branch->code, null, then_branch, break_block, function, context);
							IrValue condition = LoadConvertToIR(branch->condition, branch->initial_condition_block, context);
							IrValue test = PushTestOp(condition, branch->initial_condition_block, context);

							PushOperation(OPERATION_BRANCH, null, test, GetValue(else_branch), inner_block, branch->initial_condition_block, context);
						}
					}
					else
					{
						ConvertToIR(&branch->code, branch->initial_condition_block, exit_block, break_block, function, context);
					}
				}

				block = exit_block;
			} break;

			case AST_STATEMENT_DEFER:
			{
				ConvertToIR(&statement->defer.code, null, exit_block, break_block, function, context);
			} break;

			case AST_STATEMENT_CLAIM:
			{
				Assert();
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Return* ret = &statement->ret;

				if (ret->expression)
				{
					IrValue v = ConvertToIR(ret->expression, block, context);
					PushOperation(OPERATION_RETURN, null, v, block, context);
				}
				else
				{
					PushOperation(OPERATION_RETURN, null, block, context);
				}
				return GetValue(initial_block);
			} break;

			case AST_STATEMENT_BREAK:
			{
				Assert(break_block);
				PushJumpOp(block, break_block, context);
				return GetValue(initial_block);
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				bool direction = statement->kind == AST_STATEMENT_INCREMENT;

				IrValue address = ConvertToIR(inc->expression, block, context);
				IrValue value = LoadOp(address, block, context);

				IrValue one;

				switch (inc->expression->type->kind)
				{
					case TYPE_SPECIFIER_POINTER: one = MakeConstU64(1); break; // @FixMe: OPERATION_ELEMENT
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

				value = GetValue(PushOperation(direction ? OPERATION_ADD : OPERATION_SUBTRACT, inc->expression->type, value, one, block, context));
				StoreOp(address, value, block, context);
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				ConvertToIR(statement->expression, block, context);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* var = &statement->variable_declaration;

				IrValue address = GetValue(PushOperation(OPERATION_STACK_ALLOCATE, GetPointer(var->type, &context->stack), MakeConstU64(var->type->size), block, context));
				var->address = address;

				if (var->assignment)
				{
					IrValue v = LoadConvertToIR(var->assignment, block, context);
					StoreOp(address, v, block, context);
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;

				IrValue left = ConvertToIR(assignment->left, block, context);
				IrValue right = LoadConvertToIR(assignment->right, block, context);

				StoreOp(left, right, block, context);
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
		PushJumpOp(block, exit_block, context);
	}
	else if (!block->operations.count)
	{
		PushOperation(OPERATION_RETURN, null, block, context);
	}

	return GetValue(initial_block);
}

void ConvertToIR(Ast_Function* function, IrContext* context)
{
	Ast_Function* previous_function = context->function;
	context->function = function;

	IrBlock* entry_block = NewBlock(function, context);

	for (u32 i = 0; i < function->parameters.count; i++)
	{
		Ast_VariableDeclaration* param = &function->parameters[i];
		IrArgument* argument = PushArgument(GetPointer(param->type, &context->stack), entry_block);
		param->address = GetValue(argument);
	}

	ConvertToIR(&function->code, entry_block, null, null, function, context);

	Print("% {\n%}\n", function->name, function->blocks);

	context->function = previous_function;
}

void ConvertToIR(Ast_Root* root)
{
	IrContext context;
	ZeroMemory(&context);
	context.stack = NewStackAllocator();

	for (Ast_Function* function = root->scope.functions; function < root->scope.functions.End(); function++)
	{
		ConvertToIR(function, &context);
	}
}


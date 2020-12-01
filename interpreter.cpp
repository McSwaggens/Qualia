#include "parser.h"
#include "print.h"
#include "assert.h"
#include "util.h"
#include "memory.h"

static u64 GetExpressionMinSize(Ast_Expression* expression)
{
	return Max(expression->type->size, 8ull);
}

static u64 ConvertNumericalToUnsignedInt(Value* value, Type* type)
{
	switch (type->primitive)
	{
		case TOKEN_BOOL:    return value->value_bool;
		case TOKEN_INT8:    return value->value_int8;
		case TOKEN_INT16:   return value->value_int16;
		case TOKEN_INT32:   return value->value_int32;
		case TOKEN_INT64:   return value->value_int64;

		case TOKEN_UINT8:   return value->value_uint8;
		case TOKEN_UINT16:  return value->value_uint16;
		case TOKEN_UINT32:  return value->value_uint32;
		case TOKEN_UINT64:  return value->value_uint64;

		case TOKEN_FLOAT16: Assert();
		case TOKEN_FLOAT32: return value->value_float32;
		case TOKEN_FLOAT64: return value->value_float64;

		default:

			if (IsIntegerLike(type))
			{
				return value->value_uint64;
			}

			Assert();
			Unreachable();
	}
}

static f64 ConvertNumericalToFloat(Value* value, Type* type)
{
	switch (type->primitive)
	{
		case TOKEN_BOOL:    return value->value_bool;
		case TOKEN_INT8:    return value->value_int8;
		case TOKEN_INT16:   return value->value_int16;
		case TOKEN_INT32:   return value->value_int32;
		case TOKEN_INT64:   return value->value_int64;

		case TOKEN_UINT8:   return value->value_uint8;
		case TOKEN_UINT16:  return value->value_uint16;
		case TOKEN_UINT32:  return value->value_uint32;
		case TOKEN_UINT64:  return value->value_uint64;

		case TOKEN_FLOAT16: Assert();
		case TOKEN_FLOAT32: return value->value_float32;
		case TOKEN_FLOAT64: return value->value_float64;

		default:

			if (IsIntegerLike(type))
			{
				return value->value_uint64;
			}

			Assert();
			Unreachable();
	}
}

static void ConvertNumerical(Value* value, Type* from, Type* to)
{
	if (from == to) return;

	switch (to->primitive)
	{
		case TOKEN_INT8:  value->value_int8  = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_INT16: value->value_int16 = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_INT32: value->value_int32 = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_INT64: value->value_int64 = ConvertNumericalToUnsignedInt(value, from); break;

		case TOKEN_UINT8:  value->value_uint8  = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_UINT16: value->value_uint16 = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_UINT32: value->value_uint32 = ConvertNumericalToUnsignedInt(value, from); break;
		case TOKEN_UINT64: value->value_uint64 = ConvertNumericalToUnsignedInt(value, from); break;

		case TOKEN_FLOAT16: Assert();
		case TOKEN_FLOAT32: value->value_float32 = ConvertNumericalToFloat(value, from); break;
		case TOKEN_FLOAT64: value->value_float64 = ConvertNumericalToFloat(value, from); break;

		default:

			if (IsIntegerLike(to))
			{
				value->value_uint64 = ConvertNumericalToUnsignedInt(value, from);
				break;
			}

			Assert();
	}
}

void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter)
{
	if (expression->kind == AST_EXPRESSION_BINARY_DOT)
	{
		Ast_Expression_Binary* binary = expression->GetBinary();
		char data[binary->left->type->size];
		Interpret(binary->left, data, true, frame, interpreter);
		Assert(binary->right->kind == AST_EXPRESSION_TERMINAL_STRUCT_MEMBER);

		if (binary->left->is_referential_value)
		{
			char* reference = *(char**)data;
			reference += binary->right->GetStructMember()->member->offset;

			if (allow_referential)
			{
				*(char**)output = reference;
			}
			else
			{
				CopyMemory(output, reference, binary->type->size);
			}
		}
		else
		{
			CopyMemory(output, data + binary->right->GetStructMember()->member->offset, binary->right->type->size);
		}
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_VARIABLE)
	{
		Ast_Expression_Variable* variable = expression->GetVariable();
		if (allow_referential)
		{
			*(char**)output = frame->GetData(variable->variable);
		}
		else
		{
			CopyMemory(output, frame->GetData(variable->variable), expression->type->size);
		}
	}
	else if (IsBinaryExpression(expression->kind))
	{
		Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

		char left[GetExpressionMinSize(binary->left)];
		char right[GetExpressionMinSize(binary->right)];

		ZeroMemory(left, sizeof left);
		ZeroMemory(right, sizeof right);

		Interpret(binary->left,  left,  false, frame, interpreter);
		Interpret(binary->right, right, false, frame, interpreter);

		Assert(IsNumerical(binary->left->type));
		Assert(IsNumerical(binary->right->type));

		Type* dominant = GetDominantType(binary->left->type, binary->right->type);

		if (IsFloat(dominant))
		{
			*(f64*)left  = ConvertNumericalToFloat((Value*)left,  binary->left->type);
			*(f64*)right = ConvertNumericalToFloat((Value*)right, binary->right->type);

			switch (binary->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_LESS:             *(bool*)output = *(f64*)left <  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    *(bool*)output = *(f64*)left <= *(f64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER:          *(bool*)output = *(f64*)left >  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: *(bool*)output = *(f64*)left >= *(f64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            *(bool*)output = *(f64*)left == *(f64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        *(bool*)output = *(f64*)left != *(f64*)right; break;
				case AST_EXPRESSION_BINARY_ADD:         *(f64*)output = *(f64*)left +  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_SUBTRACT:    *(f64*)output = *(f64*)left -  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_MULTIPLY:    *(f64*)output = *(f64*)left *  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_DIVIDE:      *(f64*)output = *(f64*)left /  *(f64*)right; break;
				case AST_EXPRESSION_BINARY_EXPONENTIAL: *(f64*)output = Pow(*(f64*)left, *(f64*)right); break;
				default: Assert();
			}

			Print("(% % %) = \n", *(f64*)left, binary->op, *(f64*)right);

			if (binary->type == &primitive_bool) Print("%\n", *(bool*)output);
			else Print("%\n", *(f64*)output);

			ConvertNumerical((Value*)output, &primitive_float64, binary->type);

		}
		else if (IsIntegerLike(dominant))
		{
			u64 n;
			bool is_signed = IsSignedInteger(dominant);

			switch (binary->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_LESS:
					n = is_signed ? (*(s64*)left <  *(s64*)right) : (*(u64*)left <  *(u64*)right);
					break;

				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
					n = is_signed ? (*(s64*)left <= *(s64*)right) : (*(u64*)left <= *(u64*)right);
					break;

				case AST_EXPRESSION_BINARY_COMPARE_GREATER:
					n = is_signed ? (*(s64*)left >  *(s64*)right) : (*(u64*)left >  *(u64*)right);
					break;

				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
					n = is_signed ? (*(s64*)left >= *(s64*)right) : (*(u64*)left >= *(u64*)right);
					break;

				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:     n = *(u64*)left == *(u64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL: n = *(u64*)left != *(u64*)right; break;
				case AST_EXPRESSION_BINARY_ADD:               n = *(u64*)left +  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_SUBTRACT:          n = *(u64*)left -  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_MULTIPLY:          n = *(u64*)left *  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_DIVIDE:            n = *(u64*)left /  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_MODULO:            n = *(u64*)left %  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_EXPONENTIAL:       n = Pow(*(u64*)left, *(u64*)right) + 0.5; break; // @TestMe
				case AST_EXPRESSION_BINARY_BITWISE_OR:        n = *(u64*)left |  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR:       n = *(u64*)left ^  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_BITWISE_AND:       n = *(u64*)left &  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:        n = *(u64*)left << *(u64*)right; break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT:       n = *(u64*)left >> *(u64*)right; break;
				case AST_EXPRESSION_BINARY_AND:               n = *(u64*)left && *(u64*)right; break;
				case AST_EXPRESSION_BINARY_OR:                n = *(u64*)left || *(u64*)right; break;
				default: Assert();
			}

			*(u64*)output = (n = MaskLowerBytes(n, binary->type->size));

			if (is_signed) Print("(% % %) = ", *(s64*)left, binary->op, *(s64*)right, (s64)n);
			else           Print("(% % %) = ", *(u64*)left, binary->op, *(u64*)right, (u64)n);

			if (binary->type == &primitive_bool) Print("%\n", (bool)n);
			else if (is_signed) Print("%\n", (s64)n);
			else Print("%\n", (u64)n);
		}
		else Assert();
	}
	else if (IsUnaryExpression(expression->kind))
	{
		Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
		switch (unary->kind)
		{
			case AST_EXPRESSION_UNARY_BINARY_NOT:
			{
				s64 n = 0;
				Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
				n = ~n;
				CopyMemory(output, (char*)&n, unary->type->size);
			} break;

			case AST_EXPRESSION_UNARY_NOT:
			{
				s64 n = 0;
				Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
				n = -n;
				*(bool*)output = !n;
			} break;

			case AST_EXPRESSION_UNARY_MINUS:
			{
				s64 n = 0;
				Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
				n = -n;
				CopyMemory(output, (char*)&n, unary->type->size);
			} break;

			case AST_EXPRESSION_UNARY_PLUS:
			{
				s64 n = 0;
				Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
				n = Abs(n);
				CopyMemory(output, (char*)&n, unary->type->size);
			} break;

			case AST_EXPRESSION_UNARY_VALUE_OF:
			{
				char* ref;
				Interpret(unary->subexpression, (char*)&ref, false, frame, interpreter);
				if (allow_referential)
				{
					*(char**)output = ref;
				}
				else
				{
					CopyMemory(output, ref, unary->type->size);
				}
			} break;

			case AST_EXPRESSION_UNARY_ADDRESS_OF:
			{
				Interpret(unary->subexpression, output, true, frame, interpreter);
			} break;

			default:
				Unreachable();
		}
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_LITERAL)
	{
		Ast_Expression_Literal* literal = expression->GetLiteral();
		if (literal->token->kind == TOKEN_INTEGER_LITERAL)
		{
			CopyMemory(output, (char*)&literal->token->info.integer.value, literal->type->size);
		}
		else if (literal->token->kind == TOKEN_FLOAT_LITERAL)
		{
			if (literal->type->primitive == TOKEN_FLOAT64)
			{
				*(f64*)output = (f64)literal->token->info.floating_point.value;
			}
			else if (literal->type->primitive == TOKEN_FLOAT32)
			{
				*(f32*)output = (f32)literal->token->info.floating_point.value;
			}
			else Assert();
		}
		else if (literal->token->kind == TOKEN_TRUE)
		{
			*(bool*)output = true;
		}
		else if (literal->token->kind == TOKEN_FALSE)
		{
			*(bool*)output = false;
		}
		else if (literal->token->kind == TOKEN_NULL)
		{
			*(char**)output = null;
		}
		else Assert();
	}
	else if (expression->kind == AST_EXPRESSION_TUPLE)
	{
		Ast_Expression_Tuple* tuple = expression->GetTuple();
		for (u32 i = 0; i < tuple->elements.count; i++)
		{
			Ast_Expression* element = tuple->elements[i];
			Interpret(element, output, false, frame, interpreter);
			output += element->type->size;
		}
	}
	else if (expression->kind == AST_EXPRESSION_CALL)
	{
		Ast_Expression_Call* call = expression->GetCall();
		Ast_Function* function = call->function->GetFunction()->function;

		char input[function->type->function.input->size];
		Interpret(call->parameters, input, false, frame, interpreter);

		char data[function->type->function.output->size];
		Interpret(function, input, data, interpreter);
		CopyMemory(output, data, expression->type->size);
	}
	else
	{
		Assert();
	}
}

void Interpret(Ast_Code* code, char* output, StackFrame* frame, Interpreter* interpreter)
{
	u32 defer_count = 0;
	for (Ast_Statement* statement = code->statements;
		statement < code->statements.End() && !frame->do_return && !frame->do_break;
		statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_EXPRESSION:
			{
				Ast_Expression* expression = statement->expression;

				char data[GetExpressionMinSize(expression)];
				Interpret(expression, data, true, frame, interpreter);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* variable = &statement->variable_declaration;
				char* variable_data = frame->GetData(variable);
				ZeroMemory(frame->GetData(variable), variable->type->size);

				if (variable->assignment)
				{
					char data[GetExpressionMinSize(variable->assignment)];
					ZeroMemory(data, sizeof data);
					Interpret(variable->assignment, data, false, frame, interpreter);
					CopyMemory(frame->GetData(variable), data, Min(variable->type->size, variable->assignment->type->size));
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				char* reference;
				Interpret(assignment->left, (char*)&reference, true,  frame, interpreter);
				char data[GetExpressionMinSize(assignment->right)];
				ZeroMemory(data, sizeof data);
				Interpret(assignment->right, data, false, frame, interpreter);
				CopyMemory(reference, data, Min(assignment->left->type->size, assignment->right->type->size));
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;
				char* reference;
				Interpret(assignment->left, (char*)&reference, true, frame, interpreter);

				if (IsIntegerLike(assignment->left->type))
				{
					u64 left = 0;
					CopyMemory((char*)&left, reference, assignment->left->type->size);

					char right_data[GetExpressionMinSize(assignment->right)];
					ZeroMemory(right_data, sizeof right_data);
					Interpret(assignment->right, right_data, false, frame, interpreter);
					u64 right = *(u64*)right_data;

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:      left += right; break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT: left -= right; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY: left *= right; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:   left /= right; break;
						case AST_STATEMENT_ASSIGNMENT_POWER:    left  = Pow(left, right) + 0.5; break; // @TestMe
						default: Unreachable();
					}

					CopyMemory(reference, (char*)&left, assignment->left->type->size);
				}
				else Assert();
			} break;

			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* block = &statement->branch_block;
				Ast_Branch* branch = block->branches;

				while (branch)
				{
					if (branch->condition)
					{
						bool is_if = branch->token->kind == TOKEN_IF;
						u64 loop_count = 0;
						char data[GetExpressionMinSize(branch->condition)];
						ZeroMemory(data, sizeof data);

						while (!frame->do_break && !frame->do_return)
						{
							Interpret(branch->condition, data, false, frame, interpreter);
							bool passed = *(u64*)data;

							if (passed)
							{
								loop_count++;
								Interpret(&branch->code, output, frame, interpreter);
							}

							if (!passed || is_if)
							{
								break;
							}
						}

						if (!is_if)
						{
							frame->do_break = false;
						}

						branch = loop_count
							? branch->then_branch
							: branch->else_branch;
					}
					else
					{
						Interpret(&branch->code, output, frame, interpreter);
						break;
					}
				}

			} break;

			case AST_STATEMENT_ALIAS:
			case AST_STATEMENT_CLAIM:
				continue;

			case AST_STATEMENT_DEFER:
			{
				defer_count++;
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Expression* expression = statement->ret.expression;
				frame->do_return = true;

				if (expression)
				{
					char data[GetExpressionMinSize(expression)];
					ZeroMemory(data, sizeof data);
					Interpret(expression, data, false, frame, interpreter);
					CopyMemory(output, data, frame->function->return_type->size);
				}

			} break;

			case AST_STATEMENT_BREAK:
			{
				frame->do_break = true;
			} break;

			case AST_STATEMENT_INCREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				char* ref;
				Interpret(inc->expression, (char*)&ref, true, frame, interpreter);
				Type* type = inc->expression->type;

				if (type->kind == TYPE_SPECIFIER_POINTER)
				{
					*(char**)ref += type->subtype->size;
				}
				else switch (type->size)
				{
					case 1: ++*(u8 *)ref; break;
					case 2: ++*(u16*)ref; break;
					case 4: ++*(u32*)ref; break;
					case 8: ++*(u64*)ref; break;
					default: Assert();
				}
			} break;

			case AST_STATEMENT_DECREMENT:
			{
				Ast_Decrement* dec = &statement->decrement;
				char* ref;
				Interpret(dec->expression, (char*)&ref, true, frame, interpreter);
				Type* type = dec->expression->type;

				if (type->kind == TYPE_SPECIFIER_POINTER)
				{
					*(char**)ref += type->subtype->size;
				}
				else switch (type->size)
				{
					case 1: --*(u8 *)ref; break;
					case 2: --*(u16*)ref; break;
					case 4: --*(u32*)ref; break;
					case 8: --*(u64*)ref; break;
					default: Assert();
				}
			} break;
		}
	}

	for (u32 i = 0; i < defer_count; i++)
	{
		Ast_Defer* defer = code->defers[i];

		// Gross!
		bool temp_do_return = frame->do_return;
		bool temp_do_break  = frame->do_break;
		frame->do_return = false;
		frame->do_break  = false;
		Interpret(&defer->code, output, frame, interpreter);
		frame->do_return = temp_do_return;
		frame->do_break  = temp_do_break;
	}
}

void Interpret(Ast_Function* function, char* input, char* output, Interpreter* interpreter)
{
	Print("Interpreting function: %\n", function->name);
	StackFrame frame = CreateStackFrame(function, interpreter);

	if (function->parameters.count)
	{
		// @Note: Assuming The front of the stack is the input parameters.
		// @Warn: This might not always be the case.
		CopyMemory(frame.data, input, function->type->function.input->size);
	}

	for (Ast_VariableDeclaration** variable = function->code.scope.variables; variable < function->code.scope.variables.End(); variable++)
	{
		Print("&% = %\n", (*variable)->name, (u64)frame.GetData(*variable));
	}

	standard_output_buffer.Flush();

	Interpret(&function->code, output, &frame, interpreter);
}


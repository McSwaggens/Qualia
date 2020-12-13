#include "parser.h"
#include "print.h"
#include "assert.h"
#include "util.h"
#include "memory.h"

static u64 GetExpressionMinSize(Ast_Expression* expression)
{
	return Max(expression->type->size, 8ull);
}

template<typename T>
static T ConvertNumerical(Value* value, Type_Kind type)
{
	switch (type)
	{
		case TYPE_BASETYPE_BOOL: return value->value_bool;

		case TYPE_BASETYPE_INT8:  return value->value_int8;
		case TYPE_BASETYPE_INT16: return value->value_int16;
		case TYPE_BASETYPE_INT32: return value->value_int32;
		case TYPE_BASETYPE_INT64: return value->value_int64;

		case TYPE_BASETYPE_UINT8:  return value->value_uint8;
		case TYPE_BASETYPE_UINT16: return value->value_uint16;
		case TYPE_BASETYPE_UINT32: return value->value_uint32;
		case TYPE_BASETYPE_UINT64: return value->value_uint64;

		case TYPE_SPECIFIER_POINTER: return value->value_uint64;

		case TYPE_BASETYPE_FLOAT16: Assert();
		case TYPE_BASETYPE_FLOAT32: return value->value_float32;
		case TYPE_BASETYPE_FLOAT64: return value->value_float64;

		default:
			Assert();
			Unreachable();
	}
}

static void ConvertNumerical(Value* value, Type_Kind from, Type_Kind to)
{
	if (from == to) return;

	switch (to)
	{
		case TYPE_BASETYPE_BOOL:    value->value_bool = ConvertNumerical<bool>(value, from); break;

		case TYPE_BASETYPE_INT8:    value->value_int8  = ConvertNumerical<s64>(value, from); break;
		case TYPE_BASETYPE_INT16:   value->value_int16 = ConvertNumerical<s64>(value, from); break;
		case TYPE_BASETYPE_INT32:   value->value_int32 = ConvertNumerical<s64>(value, from); break;
		case TYPE_BASETYPE_INT64:   value->value_int64 = ConvertNumerical<s64>(value, from); break;

		case TYPE_BASETYPE_UINT8:   value->value_uint8  = ConvertNumerical<u64>(value, from); break;
		case TYPE_BASETYPE_UINT16:  value->value_uint16 = ConvertNumerical<u64>(value, from); break;
		case TYPE_BASETYPE_UINT32:  value->value_uint32 = ConvertNumerical<u64>(value, from); break;
		case TYPE_BASETYPE_UINT64:  value->value_uint64 = ConvertNumerical<u64>(value, from); break;

		case TYPE_BASETYPE_FLOAT16: Assert();
		case TYPE_BASETYPE_FLOAT32: value->value_float32 = ConvertNumerical<f64>(value, from); break;
		case TYPE_BASETYPE_FLOAT64: value->value_float64 = ConvertNumerical<f64>(value, from); break;

		default: Assert();
	}
}

void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			Ast_Expression_Struct_Member* struct_member = (Ast_Expression_Struct_Member*)binary->right;

			char data[binary->left->type->size];
			Interpret(binary->left, data, true, frame, interpreter);
			Assert(binary->right->kind == AST_EXPRESSION_TERMINAL_STRUCT_MEMBER);

			if (binary->left->is_referential_value)
			{
				char* reference = *(char**)data;
				reference += struct_member->member->offset;

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
				CopyMemory(output, data + struct_member->member->offset, binary->right->type->size);
			}
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* variable = (Ast_Expression_Variable*)expression;

			if (allow_referential)
			{
				*(char**)output = frame->GetData(variable->variable);
			}
			else
			{
				CopyMemory(output, frame->GetData(variable->variable), expression->type->size);
			}
		} break;

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
		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			char left[GetExpressionMinSize(binary->left)];
			char right[GetExpressionMinSize(binary->right)];

			ZeroMemory(left,  sizeof left);
			ZeroMemory(right, sizeof right);

			Interpret(binary->left,  left,  false, frame, interpreter);
			Interpret(binary->right, right, false, frame, interpreter);

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (expression->kind == AST_EXPRESSION_BINARY_AND ||
				expression->kind == AST_EXPRESSION_BINARY_OR)
			{
				bool l = ConvertNumerical<bool>((Value*)left,  binary->left->type->kind);
				bool r = ConvertNumerical<bool>((Value*)right, binary->right->type->kind);

				if (expression->kind == AST_EXPRESSION_BINARY_AND)
				{
					*(bool*)output = l && r;
				}
				else
				{
					*(bool*)output = l || r;
				}

				Print("% % % = %\n", *(bool*)left, binary->op, *(bool*)right, *(bool*)output);
			}
			else if (IsFloat(dominant))
			{
				f64 l = ConvertNumerical<f64>((Value*)left,  binary->left->type->kind);
				f64 r = ConvertNumerical<f64>((Value*)right, binary->right->type->kind);

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             *(bool*)output = l <  r; break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    *(bool*)output = l <= r; break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          *(bool*)output = l >  r; break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: *(bool*)output = l >= r; break;
					case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            *(bool*)output = l == r; break;
					case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        *(bool*)output = l != r; break;
					case AST_EXPRESSION_BINARY_ADD:         *(f64*)output = l +  r; break;
					case AST_EXPRESSION_BINARY_SUBTRACT:    *(f64*)output = l -  r; break;
					case AST_EXPRESSION_BINARY_MULTIPLY:    *(f64*)output = l *  r; break;
					case AST_EXPRESSION_BINARY_DIVIDE:      *(f64*)output = l /  r; break;
					case AST_EXPRESSION_BINARY_EXPONENTIAL: *(f64*)output = Pow(l, r); break;
					default: Assert();
				}

				Print("% % % = ", l, binary->op, r);

				if (binary->type == &type_bool) Print("%\n", *(bool*)output);
				else Print("%\n", *(f64*)output);

				if (binary->type != &type_bool)
				{
					ConvertNumerical((Value*)output, TYPE_BASETYPE_FLOAT64, binary->type->kind);
				}
			}
			else if (IsIntegerLike(dominant))
			{
				bool is_signed = IsSignedInteger(dominant);

				if (is_signed && binary->kind != AST_EXPRESSION_BINARY_LEFT_SHIFT && binary->kind != AST_EXPRESSION_BINARY_RIGHT_SHIFT)
				{
					s64 l = ConvertNumerical<s64>((Value*)left,  binary->left->type->kind);
					s64 r = ConvertNumerical<s64>((Value*)right, binary->right->type->kind);
					s64 n;

					switch (binary->kind)
					{
						case AST_EXPRESSION_BINARY_COMPARE_LESS:             n = l <  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    n = l <= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER:          n = l >  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: n = l >= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = l == r; break;
						case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = l != r; break;
						case AST_EXPRESSION_BINARY_ADD:         n = l +  r; break;
						case AST_EXPRESSION_BINARY_SUBTRACT:    n = l -  r; break;
						case AST_EXPRESSION_BINARY_MULTIPLY:    n = l *  r; break;
						case AST_EXPRESSION_BINARY_DIVIDE:      n = l /  r; break;
						case AST_EXPRESSION_BINARY_MODULO:      n = l %  r; break;
						case AST_EXPRESSION_BINARY_EXPONENTIAL: n = Pow(l, r) + 0.5; break;
						case AST_EXPRESSION_BINARY_BITWISE_OR:  n = l |  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_XOR: n = l ^  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_AND: n = l &  r; break;
						default: Assert();
					}

					*(s64*)output = n;
					ConvertNumerical((Value*)output, TYPE_BASETYPE_INT64, binary->type->kind);

					Print("% % % = ", l, binary->op, r);
					if (binary->type == &type_bool) Print("%\n", (bool)n);
					else Print("%\n", n);
				}
				else
				{
					u64 l = ConvertNumerical<u64>((Value*)left,  binary->left->type->kind);
					u64 r = ConvertNumerical<u64>((Value*)right, binary->right->type->kind);
					u64 n;

					switch (binary->kind)
					{
						case AST_EXPRESSION_BINARY_COMPARE_LESS:             n = l <  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    n = l <= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER:          n = l >  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: n = l >= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = l == r; break;
						case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = l != r; break;
						case AST_EXPRESSION_BINARY_ADD:         n = l +  r; break;
						case AST_EXPRESSION_BINARY_SUBTRACT:    n = l -  r; break;
						case AST_EXPRESSION_BINARY_MULTIPLY:    n = l *  r; break;
						case AST_EXPRESSION_BINARY_DIVIDE:      n = l /  r; break;
						case AST_EXPRESSION_BINARY_MODULO:      n = l %  r; break;
						case AST_EXPRESSION_BINARY_EXPONENTIAL: n = Pow(l, r) + 0.5; break;
						case AST_EXPRESSION_BINARY_BITWISE_OR:  n = l |  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_XOR: n = l ^  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_AND: n = l &  r; break;
						case AST_EXPRESSION_BINARY_LEFT_SHIFT:  n = l << r; break;
						case AST_EXPRESSION_BINARY_RIGHT_SHIFT: n = l >> r; break;
						default: Assert();
					}

					*(u64*)output = n;
					ConvertNumerical((Value*)output, TYPE_BASETYPE_UINT64, binary->type->kind);

					Print("% % % = ", l, binary->op, r);
					if (binary->type == &type_bool) Print("%\n", (bool)n);
					else Print("%\n", n);
				}
			}
			else
			{
				Assert(binary->left->type == binary->right->type);

				bool b;

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_EQUAL:     b =  CompareMemory(left, right, binary->left->type->size); break;
					case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL: b = !CompareMemory(left, right, binary->left->type->size); break;
					default: Assert();
				}

				*(bool*)output = b;

				Print("% % % = %\n", binary->left, binary->op, binary->right, b);
			}
		} break;

		case AST_EXPRESSION_UNARY_BINARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			s64 n = 0;
			Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
			n = ~n;
			*(s64*)output = n;
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			s64 n = 0;
			Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
			*(bool*)output = !n;
		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			char data[GetExpressionMinSize(unary->subexpression)];
			Interpret(unary->subexpression, data, false, frame, interpreter);

			switch (unary->subexpression->type->kind)
			{
				case TYPE_BASETYPE_BOOL:
					// @TestMe: I don't actually think this is semantically allowed?
					// Maybe make -bool evaluate to an int?
					*(bool*)output = -*(bool*)data;
					break;

				case TYPE_BASETYPE_INT8:
				case TYPE_BASETYPE_UINT8:
					*(s8*)output  = -*(s8*)data;
					break;

				case TYPE_BASETYPE_INT16:
				case TYPE_BASETYPE_UINT16:
					*(s16*)output = -*(s16*)data;
					break;

				case TYPE_BASETYPE_INT32:
				case TYPE_BASETYPE_UINT32:
					*(s32*)output = -*(s32*)data;
					break;

				case TYPE_BASETYPE_INT64:
				case TYPE_BASETYPE_UINT64:
					*(s64*)output = -*(s64*)data;
					break;

				case TYPE_BASETYPE_FLOAT16:
					Assert();

				case TYPE_BASETYPE_FLOAT32:
					*(f32*)output = -*(f32*)data;
					break;

				case TYPE_BASETYPE_FLOAT64:
					*(f64*)output = -*(f64*)data;
					break;

				default:
					Assert();
			}
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			char data[GetExpressionMinSize(unary->subexpression)];
			Interpret(unary->subexpression, data, false, frame, interpreter);

			switch (unary->subexpression->type->kind)
			{
				case TYPE_BASETYPE_BOOL: *(bool*)output = *(bool*)data; break;

				case TYPE_BASETYPE_UINT8:  *(u8*)output  = *(u8*)data;  break;
				case TYPE_BASETYPE_UINT16: *(u16*)output = *(u16*)data; break;
				case TYPE_BASETYPE_UINT32: *(u32*)output = *(u32*)data; break;
				case TYPE_BASETYPE_UINT64: *(u64*)output = *(u64*)data; break;

				case TYPE_BASETYPE_INT8:  *(s8*)output  = Abs(*(s8*)data);  break;
				case TYPE_BASETYPE_INT16: *(s16*)output = Abs(*(s16*)data); break;
				case TYPE_BASETYPE_INT32: *(s32*)output = Abs(*(s32*)data); break;
				case TYPE_BASETYPE_INT64: *(s64*)output = Abs(*(s64*)data); break;

				case TYPE_BASETYPE_FLOAT16: Assert();
				case TYPE_BASETYPE_FLOAT32: *(f32*)output = Abs(*(f32*)data); break;
				case TYPE_BASETYPE_FLOAT64: *(f64*)output = Abs(*(f64*)data); break;

				default:
					Assert();
			}
		} break;

		case AST_EXPRESSION_UNARY_VALUE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
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
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			Interpret(unary->subexpression, output, true, frame, interpreter);
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			if (literal->token->kind == TOKEN_INTEGER_LITERAL)
			{
				CopyMemory(output, (char*)&literal->token->info.integer.value, literal->type->size);
			}
			else if (literal->token->kind == TOKEN_FLOAT_LITERAL)
			{
				if (literal->type->kind == TYPE_BASETYPE_FLOAT32)
				{
					*(f32*)output = (f32)literal->token->info.floating_point.value;
				}
				else if (literal->type->kind == TYPE_BASETYPE_FLOAT64)
				{
					*(f64*)output = (f64)literal->token->info.floating_point.value;
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
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				Interpret(element, output, false, frame, interpreter);
				output += element->type->size;
			}
		} break;

		case AST_EXPRESSION_CALL:
		{
			// @Todo @Bug: Make this work for "external" functions, or function pointers.
			//        ...  Maybe make them not work in compile time?
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;
			Ast_Function* function = ((Ast_Expression_Function*)call->function)->function;

			char input[function->type->function.input->size];
			Interpret(call->parameters, input, false, frame, interpreter);

			char data[function->type->function.output->size];
			Interpret(function, input, data, interpreter);
			CopyMemory(output, data, expression->type->size);
		} break;

		default:
			Assert();
			Unreachable();
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


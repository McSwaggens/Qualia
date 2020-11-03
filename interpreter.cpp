#include "parser.h"
#include "print.h"
#include "assert.h"
#include "util.h"
#include "memory.h"

static u64 GetExpressionMinSize(Ast_Expression* expression)
{
	return Max(expression->type->size, 8ull);
}

void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter)
{
	if (expression->kind == AST_EXPRESSION_BINARY_DOT)
	{
		char data[expression->left->type->size];
		Interpret(expression->left, data, true, frame, interpreter);
		Assert(expression->right->kind == AST_EXPRESSION_TERMINAL_STRUCT_MEMBER);

		if (expression->left->is_referential_value)
		{
			char* reference = *(char**)data;
			reference += expression->right->struct_member->offset;

			if (allow_referential)
			{
				*(char**)output = reference;
			}
			else
			{
				CopyMemory(output, reference, expression->type->size);
			}
		}
		else
		{
			CopyMemory(output, data + expression->right->struct_member->offset, expression->right->type->size);
		}
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_VARIABLE)
	{
		if (allow_referential)
		{
			*(char**)output = frame->GetData(expression->variable);
		}
		else
		{
			CopyMemory(output, frame->GetData(expression->variable), expression->type->size);
		}
	}
	else if (IsBinaryExpression(expression->kind))
	{
		char left[GetExpressionMinSize(expression->left)];
		char right[GetExpressionMinSize(expression->right)];

		ZeroMemory(left, sizeof left);
		ZeroMemory(right, sizeof right);

		Interpret(expression->left,  left,  false, frame, interpreter);
		Interpret(expression->right, right, false, frame, interpreter);

		if (IsIntegerLikeType(expression->left->type) && IsIntegerLikeType(expression->right->type))
		{
			u64 n;
			bool is_signed = IsSignedIntegerType(expression->left->type) || IsSignedIntegerType(expression->right->type);

			switch (expression->kind)
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

				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = *(u64*)left == *(u64*)right; break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = *(u64*)left != *(u64*)right; break;
				case AST_EXPRESSION_BINARY_ADD:                      n = *(u64*)left +  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_SUBTRACT:                 n = *(u64*)left -  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_MULTIPLY:                 n = *(u64*)left *  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_DIVIDE:                   n = *(u64*)left /  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_MODULO:                   n = *(u64*)left %  *(u64*)right; break; // @TestMe
				case AST_EXPRESSION_BINARY_EXPONENTIAL:              n = Pow(*(u64*)left, *(u64*)right) + 0.5; break; // @TestMe
				case AST_EXPRESSION_BINARY_BITWISE_OR:               n = *(u64*)left |  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR:              n = *(u64*)left ^  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_BITWISE_AND:              n = *(u64*)left &  *(u64*)right; break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:               n = *(u64*)left << *(u64*)right; break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT:              n = *(u64*)left >> *(u64*)right; break;
				case AST_EXPRESSION_BINARY_AND:                      n = *(u64*)left && *(u64*)right; break;
				case AST_EXPRESSION_BINARY_OR:                       n = *(u64*)left || *(u64*)right; break;
				case AST_EXPRESSION_BINARY_DOT:
				default: Unreachable();
			}

			*(u64*)output = (n = MaskLowerBytes(n, expression->type->size));

			if (is_signed)
			{
				Print("(% % %) = %\n", *(s64*)left, expression->token, *(s64*)right, (s64)n);
			}
			else
			{
				Print("(% % %) = %\n", *(u64*)left, expression->token, *(u64*)right, (u64)n);
			}
		}
		else Assert();
	}
	else if (IsUnaryExpression(expression->kind))
	{
		switch (expression->kind)
		{
			case AST_EXPRESSION_UNARY_BINARY_NOT:
			{
				s64 n = 0;
				Interpret(expression->right, (char*)&n, false, frame, interpreter);
				n = ~n;
				CopyMemory(output, (char*)&n, expression->type->size);
			} break;

			case AST_EXPRESSION_UNARY_NOT:
			{
				s64 n = 0;
				Interpret(expression->right, (char*)&n, false, frame, interpreter);
				n = -n;
				*(bool*)output = !n;
			} break;

			case AST_EXPRESSION_UNARY_MINUS:
			{
				s64 n = 0;
				Interpret(expression->right, (char*)&n, false, frame, interpreter);
				n = -n;
				CopyMemory(output, (char*)&n, expression->type->size);
			} break;

			case AST_EXPRESSION_UNARY_PLUS:
			{
				s64 n = 0;
				Interpret(expression->right, (char*)&n, false, frame, interpreter);
				n = Abs(n);
				CopyMemory(output, (char*)&n, expression->type->size);
			} break;

			case AST_EXPRESSION_UNARY_VALUE_OF:
			{
				char* ref;
				Interpret(expression->right, (char*)&ref, false, frame, interpreter);
				if (allow_referential)
				{
					*(char**)output = ref;
				}
				else
				{
					CopyMemory(output, ref, expression->type->size);
				}
			} break;

			case AST_EXPRESSION_UNARY_ADDRESS_OF:
			{
				Interpret(expression->right, output, true, frame, interpreter);
			} break;

			default:
				Unreachable();
		}
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_LITERAL)
	{
		if (expression->token->kind == TOKEN_INTEGER_LITERAL)
		{
			CopyMemory(output, (char*)&expression->token->info.integer.value, expression->type->size);
		}
		else if (expression->token->kind == TOKEN_FLOAT_LITERAL)
		{
			if (expression->type->primitive == TOKEN_FLOAT64)
			{
				*(f64*)output = (f64)expression->token->info.floating_point.value;
			}
			else if (expression->type->primitive == TOKEN_FLOAT32)
			{
				*(f32*)output = (f32)expression->token->info.floating_point.value;
			}
			else Assert();
		}
		else if (expression->token->kind == TOKEN_TRUE)
		{
			*(bool*)output = true;
		}
		else if (expression->token->kind == TOKEN_FALSE)
		{
			*(bool*)output = false;
		}
		else if (expression->token->kind == TOKEN_NULL)
		{
			*(char**)output = null;
		}
		else Assert();
	}
	else if (expression->kind == AST_EXPRESSION_TUPLE)
	{
		for (Ast_Expression** sub = expression->begin; sub < expression->end; sub++)
		{
			Interpret(*sub, output, false, frame, interpreter);
			output += (*sub)->type->size;
		}
	}
	else if (expression->kind == AST_EXPRESSION_CALL)
	{
		Ast_Function* function = expression->left->function;

		char input[function->type->function.input->size];
		Interpret(expression->right, input, false, frame, interpreter);

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

				if (IsIntegerLikeType(assignment->left->type))
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


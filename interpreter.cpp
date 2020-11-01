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
		char left_data[GetExpressionMinSize(expression->left)];
		char right_data[GetExpressionMinSize(expression->right)];

		ZeroMemory(left_data, sizeof left_data);
		ZeroMemory(right_data, sizeof right_data);

		Interpret(expression->left,  left_data,  false, frame, interpreter);
		Interpret(expression->right, right_data, false, frame, interpreter);

		if (IsIntegerLikeType(expression->left->type) && IsIntegerLikeType(expression->right->type))
		{
			u64 n;
			u64 left  = *Cast(left_data,  u64*);
			u64 right = *Cast(right_data, u64*);

			switch (expression->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = left == right; break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = left != right; break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS:             n = left <  right; break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    n = left <= right; break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER:          n = left >  right; break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: n = left >= right; break;
				case AST_EXPRESSION_BINARY_ADD:                      n = left +  right; break;
				case AST_EXPRESSION_BINARY_SUBTRACT:                 n = left -  right; break;
				case AST_EXPRESSION_BINARY_MULTIPLY:                 n = left *  right; break;
				case AST_EXPRESSION_BINARY_DIVIDE:                   n = left /  right; break;
				case AST_EXPRESSION_BINARY_MODULO:                   n = left %  right; break;
				case AST_EXPRESSION_BINARY_EXPONENTIAL:              n = (u64)(__builtin_powl(left, right) + 0.5); break; // @TestMe
				case AST_EXPRESSION_BINARY_BITWISE_OR:               n = left |  right; break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR:              n = left ^  right; break;
				case AST_EXPRESSION_BINARY_BITWISE_AND:              n = left &  right; break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:               n = left << right; break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT:              n = left >> right; break;
				case AST_EXPRESSION_BINARY_AND:                      n = left && right; break;
				case AST_EXPRESSION_BINARY_OR:                       n = left || right; break;
				case AST_EXPRESSION_BINARY_DOT:
				default: Unreachable();
			}

			CopyMemory(output, (char*)&n, expression->type->size);
			n &= -1 >> (64-(expression->type->size * 8));
			Print("(% % %) = %\n", left, expression->token, right, n);
		}
		else Assert();
	}
	else if (IsUnaryExpression(expression->kind))
	{
		switch (expression->kind)
		{
			case AST_EXPRESSION_UNARY_BINARY_NOT:
			{
				Assert();
			} break;

			case AST_EXPRESSION_UNARY_NOT:
			{
				Assert();
			} break;

			case AST_EXPRESSION_UNARY_MINUS:
			{
				Assert();
			} break;

			case AST_EXPRESSION_UNARY_PLUS:
			{
				Assert();
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
				Interpret(assignment->left,  (char*)&reference,   true,  frame, interpreter);
				Interpret(assignment->right, reference, false, frame, interpreter); // @Bug: Left size could be smaller than right size and thus buffer overflow. #ToLazyToFixRnFamalamajam
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
						case AST_STATEMENT_ASSIGNMENT_POWER:    left  = (u64)(__builtin_powl(left, right) + 0.5); break; // @TestMe
						default: Unreachable();
					}

					CopyMemory(reference, (char*)&left, assignment->left->type->size);
				}
				else Assert();
			} break;

			case AST_STATEMENT_BRANCH_BLOCK:
			{
				// @Todo: Handle frame->do_break

				Ast_BranchBlock* block = &statement->branch_block;

				for (Ast_Branch* branch = block->branches; branch < block->branches.End(); branch++)
				{
				}

			} break;

			case AST_STATEMENT_ALIAS:
				continue;

			case AST_STATEMENT_DEFER:
			case AST_STATEMENT_CLAIM:
				Assert();

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
		}
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


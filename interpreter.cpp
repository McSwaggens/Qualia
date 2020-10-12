#include "parser.h"
#include "print.h"
#include "assert.h"
#include "util.h"
#include "memory.h"

void Interpret(Ast_Expression* expression, char* output)
{
	Print("Interpreting expression: %\n", expression);
	if (IsBinaryExpression(expression->kind))
	{
		u64 left_size = Max(expression->left->type->size, 8llu);
		u64 right_size = Max(expression->right->type->size, 8llu);

		char left_data[left_size];
		char right_data[right_size];

		ZeroMemory(left_data, left_size);
		ZeroMemory(right_data, right_size);

		Interpret(expression->left,  left_data);
		Interpret(expression->right, right_data);

		if (IsIntegerType(expression->left->type) && IsIntegerType(expression->right->type))
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
			Print("% % % = %\n", left, expression->token, right, n);
		}
		else Assert();
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
	else Assert();
}

void Interpret(Ast_Code* code, char* output, StackFrame frame, Interpreter* interpreter, Parse_Info* info)
{
	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_EXPRESSION:
			{
				Ast_Expression* expression = statement->expression;
				char data[Max(expression->type->size, 8llu)];
				Interpret(expression, data);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* variable = &statement->variable_declaration;
				char* variable_data = frame.GetData(variable);
				ZeroMemory(frame.GetData(variable), variable->type->size);

				if (variable->assignment)
				{
					if (variable->assignment->is_referential_value)
					{
						union { char array[8]; char* pointer; } ref;
						Interpret(variable->assignment, ref.array);
						CopyMemory(frame.GetData(variable), ref.pointer, Min(variable->type->size, variable->assignment->type->size));
					}
					else
					{
						char data[variable->assignment->type->size];
						Interpret(variable->assignment, data);
						CopyMemory(frame.GetData(variable), data, Min(variable->type->size, variable->assignment->type->size));
					}
				}

			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				union { char array[8]; char* pointer; } ref;
				Interpret(assignment->left, ref.array);
				Interpret(assignment->right, ref.pointer);
			} break;

			case AST_STATEMENT_BRANCH_BLOCK:
			case AST_STATEMENT_DEFER:
			case AST_STATEMENT_CLAIM:
			case AST_STATEMENT_ALIAS:
			case AST_STATEMENT_RETURN:
			case AST_STATEMENT_BREAK:
			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
				Assert();
		}
	}
}

void Interpret(Ast_Function* function, char* output, Interpreter* interpreter, Parse_Info* info)
{
	Print("Interpreting function %\n", function->name);
	StackFrame frame = CreateStackFrame(function, interpreter);
	Interpret(&function->code, output, frame, interpreter, info);
}


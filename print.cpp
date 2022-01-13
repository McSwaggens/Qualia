#include "print.h"
#include "token.h"
#include "assert.h"
#include "parser.h"
#include "ir.h"

void Write(OutputBuffer* buffer, u8  n)
{
	Write(buffer, (u64)n);
}

void Write(OutputBuffer* buffer, u16 n)
{
	Write(buffer, (u64)n);
}

void Write(OutputBuffer* buffer, u32 n)
{
	Write(buffer, (u64)n);
}

// 3 5 10 20
void Write(OutputBuffer* buffer, u64 n)
{
	constexpr int max = 20; // ceil(log10(pow(2, sizeof(n)*8-1)))
	char digits[max];
	int count = 0;
	do
	{
		digits[max - count - 1] = '0' + n % 10;
	} while (++count < max && (n /= 10));
	buffer->Write(digits + (max - count), count);
}

void Write(OutputBuffer* buffer, s8  n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)Abs(n));
}

void Write(OutputBuffer* buffer, s16 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)Abs(n));
}

void Write(OutputBuffer* buffer, s32 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)Abs(n));
}

void Write(OutputBuffer* buffer, s64 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)Abs(n));
}

void Write(OutputBuffer* buffer, f32 f)
{
	Write(buffer, (f64)f);
}

void Write(OutputBuffer* buffer, f64 f)
{
	// @FixMe: This really isn't that great, but it's good enough for now.
	Write(buffer, (s64)f);
	Write(buffer, '.');
	Write(buffer, (s64)Abs((f-(s64)f) * Pow(10, 9)));
}

void Write(OutputBuffer* buffer, Token_Kind kind)
{
	Write(buffer, ToString(kind));
}

void Write(OutputBuffer* buffer, Token& token)
{
	if (token.kind == TOKEN_IDENTIFIER_CASUAL || token.kind == TOKEN_IDENTIFIER_FORMAL)
	{
		Write(buffer, token.string);
	}
	else if (token.kind == TOKEN_STRING_LITERAL)
	{
		buffer->Write('"');
		Write(buffer, token.span);
		buffer->Write('"');
	}
	else if (token.kind == TOKEN_INTEGER_LITERAL)
	{
		Write(buffer, token.integer.value);
	}
	else if (token.kind == TOKEN_FLOAT_LITERAL)
	{
		Write(buffer, token.floating_point.value);
	}
	else
	{
		Write(buffer, token.kind);
	}
}

void Write(OutputBuffer* buffer, Token* token)
{
	if (!token) Write(buffer, "null");
	else Write(buffer, *token);
}

void Write(OutputBuffer* buffer, Span<Token> span)
{
	for (u64 i = 0; i < span.Length(); i++)
	{
		if (i != 0) Write(buffer, " ");
		Write(buffer, span[i]);
	}
}

void Write(OutputBuffer* buffer, SourceLocation loc)
{
	Write(buffer, loc.line + 1);
	buffer->Write(':');
	Write(buffer, loc.offset + 1);
}

void Write(OutputBuffer* buffer, Type* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	// Assert(type);

	switch (type->kind)
	{
		case TYPE_SPECIFIER_POINTER:
			Write(buffer, "*");
			Write(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_OPTIONAL:
			Write(buffer, "?");
			Write(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_FIXED_ARRAY:
			Write(buffer, '[');
			Write(buffer, type->length);
			Write(buffer, ']');
			Write(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
			Write(buffer, "[]");
			Write(buffer, type->subtype);
			break;

		case TYPE_BASETYPE_BYTE:    Write(buffer, TOKEN_BYTE);    break;
		case TYPE_BASETYPE_BOOL:    Write(buffer, TOKEN_BOOL);    break; 
		case TYPE_BASETYPE_INT8:    Write(buffer, TOKEN_INT8);    break; 
		case TYPE_BASETYPE_INT16:   Write(buffer, TOKEN_INT16);   break; 
		case TYPE_BASETYPE_INT32:   Write(buffer, TOKEN_INT32);   break; 
		case TYPE_BASETYPE_INT64:   Write(buffer, TOKEN_INT64);   break; 
		case TYPE_BASETYPE_UINT8:   Write(buffer, TOKEN_UINT8);   break; 
		case TYPE_BASETYPE_UINT16:  Write(buffer, TOKEN_UINT16);  break; 
		case TYPE_BASETYPE_UINT32:  Write(buffer, TOKEN_UINT32);  break; 
		case TYPE_BASETYPE_UINT64:  Write(buffer, TOKEN_UINT64);  break; 
		case TYPE_BASETYPE_FLOAT16: Write(buffer, TOKEN_FLOAT16); break; 
		case TYPE_BASETYPE_FLOAT32: Write(buffer, TOKEN_FLOAT32); break; 
		case TYPE_BASETYPE_FLOAT64: Write(buffer, TOKEN_FLOAT64); break; 

		case TYPE_BASETYPE_FUNCTION:
			Write(buffer, type->input);
			Write(buffer, " -> ");
			Write(buffer, type->output);
			break;

		case TYPE_BASETYPE_TUPLE:
			Write(buffer, '(');
			for (u32 i = 0; i < type->tuple.count; i++)
			{
				if (i) Write(buffer, ", ");
				Write(buffer, type->tuple[i]);
			}
			Write(buffer, ')');
			break;

		case TYPE_BASETYPE_STRUCT:
			Write(buffer, type->structure->name);
			break;

		case TYPE_BASETYPE_ENUM:
			Write(buffer, type->enumeration->name);
			break;

	}
}

void Write(OutputBuffer* buffer, Ast_Type* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	Write(buffer, *type);
}

void Write(OutputBuffer* buffer, Ast_Type type)
{
	for (Ast_Specifier* specifier = type.specifiers; specifier < type.specifiers.End(); specifier++)
	{
		switch (specifier->kind)
		{
			case AST_SPECIFIER_POINTER:   Write(buffer, "*"); break;
			case AST_SPECIFIER_OPTIONAL:  Write(buffer, "?"); break;
			case AST_SPECIFIER_ARRAY:
			{
				Write(buffer, "[");
				Write(buffer, specifier->size_expression);
				Write(buffer, "]");
			} break;
		}
	}

	switch (type.basetype.kind)
	{
		case AST_BASETYPE_PRIMITIVE: Write(buffer, type.basetype.token); break;
		case AST_BASETYPE_USERTYPE:  Write(buffer, type.basetype.token); break;

		case AST_BASETYPE_TUPLE:
		{
			Write(buffer, "(");

			for (Ast_Type* t = type.basetype.tuple; t < type.basetype.tuple.End(); t++)
			{
				if (t != type.basetype.tuple) Write(buffer, ", ");
				Write(buffer, t);
			}

			Write(buffer, ")");
		} break;

		case AST_BASETYPE_FUNCTION:
		{
			Write(buffer, "(");
			Write(buffer, type.basetype.function.input);
			Write(buffer, ") -> (");
			Write(buffer, type.basetype.function.output);
			Write(buffer, ")");
		} break;
	}
}

void Write(OutputBuffer* buffer, Ast_Expression* expression)
{
	if (!expression)
	{
		Write(buffer, "null");
		return;
	}

	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* variable = (Ast_Expression_Variable*)expression;
			Write(buffer, "(Variable: ");
			Write(buffer, variable->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_FUNCTION:
		{
			Ast_Expression_Function* function = (Ast_Expression_Function*)expression;
			Write(buffer, "(Function: ");
			Write(buffer, function->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION:
		{
			Ast_Expression_Intrinsic_Function* function = (Ast_Expression_Intrinsic_Function*)expression;
			Write(buffer, "(Intrinsic Function: ");
			Write(buffer, function->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		{
			Ast_Expression_Struct* structure = (Ast_Expression_Struct*)expression;
			Write(buffer, "(Struct: ");
			Write(buffer, structure->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM:
		{
			Ast_Expression_Enum* enumeration = (Ast_Expression_Enum*)expression;
			Write(buffer, "(Enum: ");
			Write(buffer, enumeration->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		{
			Ast_Expression_Struct_Member* member = (Ast_Expression_Struct_Member*)expression;
			Write(buffer, "(Struct_Member: ");
			Write(buffer, member->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		{
			Ast_Expression_Enum_Member* member = (Ast_Expression_Enum_Member*)expression;
			Write(buffer, "(Enum_Member: ");
			Write(buffer, member->token);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_DYNAMIC_ARRAY:
		{
			Ast_Expression_Dynamic_Array* array = (Ast_Expression_Dynamic_Array*)expression;
			Write(buffer, "[ ");
			Write(buffer, array->left);
			Write(buffer, " .. ");
			Write(buffer, array->right);
			Write(buffer, " ]");
		}
		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Ast_Expression_Fixed_Array* fixed_array = (Ast_Expression_Fixed_Array*)expression;

			Write(buffer, "{ ");

			for (u32 i = 0; i < fixed_array->elements.count; i++)
			{
				if (!i) Write(buffer, ", ");

				Write(buffer, fixed_array->elements[i]);
			}

			Write(buffer, " }");
		} break;

		case AST_EXPRESSION_TERMINAL_NAME:
		case AST_EXPRESSION_TERMINAL_LITERAL:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_ARRAY_DATA:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;
			Write(buffer, literal->token);
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		case AST_EXPRESSION_BINARY_DOT:
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
			Write(buffer, "(");
			Write(buffer, binary->left);
			Write(buffer, " ");
			Write(buffer, binary->op);
			Write(buffer, " ");
			Write(buffer, binary->right);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			Write(buffer, "(");
			Write(buffer, unary->op);
			Write(buffer, " ");
			Write(buffer, unary->subexpression);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			Write(buffer, subscript->array);
			Write(buffer, "[");
			Write(buffer, subscript->index);
			Write(buffer, "]");
		} break;

		case AST_EXPRESSION_DOT_CALL:
		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;
			Write(buffer, call->function);
			if (call->parameters->kind != AST_EXPRESSION_TUPLE)
			{
				Write(buffer, "(");
				Write(buffer, call->parameters);
				Write(buffer, ")");
			}
			else
			{
				Write(buffer, call->parameters);
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;
			Write(buffer, "(");
			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				if (i) Write(buffer, ", ");
				Write(buffer, tuple->elements[i]);
			}
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			Write(buffer, "(");
			Write(buffer, ternary->left);
			Write(buffer, " if ");
			Write(buffer, ternary->middle);
			Write(buffer, " else ");
			Write(buffer, ternary->right);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;

			Write(buffer, "(");
			Write(buffer, as->expression);
			Write(buffer, " as ");
			Write(buffer, as->type);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			Write(buffer, "(Implicit_Cast: ");
			Write(buffer, cast->type);
			Write(buffer, ", ");
			Write(buffer, cast->subexpression);
			Write(buffer, ")");
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			Write(buffer, "(LAMBDA)");
		} break;
	}
}

void Write(OutputBuffer* buffer, IrLogicKind kind)
{
	Write(buffer, ToString(kind));
}

void Write(OutputBuffer* buffer, IrLogicIndex index, IrFunctionInfo* function)
{
	if (index == IR_NONE)
	{
		Write(buffer, "IR_NONE");
		return;
	}

	IrLogic* logic = &function->logic[index];

	if (logic->kind == LOGOS_CONSTANT)
	{
		if (IsInteger(logic->type))
		{
			Write(buffer, logic->constant_int64);
		}
		else if (logic->type == &type_bool)
		{
			Write(buffer, logic->constant_bool);
		}
		else if (logic->type == &type_float32)
		{
			Write(buffer, logic->constant_float32);
		}
		else if (logic->type == &type_float64)
		{
			Write(buffer, logic->constant_float32);
		}
		else if (IsPointer(logic->type))
		{
			Write(buffer, logic->constant_int64);
		}
		else
		{
			Assert();
		}
	}
	else if (logic->kind == LOGOS_BLOCK)
	{
		IrBlockInfo* block = GetBlockInfo(logic->block, function);

		Write(buffer, "block");
		Write(buffer, logic->block);
	}
	else
	{
		Write(buffer, "i");
		Write(buffer, index);
	}
}

void Write(OutputBuffer* buffer, IrFunctionInfo* function)
{
	Write(buffer, function->function->name);
	Write(buffer, " ");
	Write(buffer, function->function->type->input);
	Write(buffer, " -> ");
	Write(buffer, function->function->type->output);
	Write(buffer, ":\n");

	for (IrBlockInfoIndex block_index = 0; block_index < function->blocks.count; block_index++)
	{
		IrBlockInfo* block = &function->blocks[block_index];

		if (block_index)
		{
			Write(buffer, "\n");
		}

		Write(buffer, "block");
		Write(buffer, block_index);

		bool verbose = false;

		if (verbose)
		{
			Write(buffer, "(i");
			Write(buffer, block->representation);
			Write(buffer, ")");
		}

		Write(buffer, ":\n");

		for (u32 i = 0; i < block->logic.count; i++)
		{
			IrLogicIndex logic_index = block->logic[i];
			IrLogic* logic = &function->logic[logic_index];

			if (!verbose)
			{
				if (logic->kind == LOGOS_CONSTANT) continue;
				if (logic->kind == LOGOS_BLOCK) continue;
				if (logic->kind == LOGOS_FUNCTION) continue;
			}

			Write(buffer, "\ti");
			Write(buffer, logic_index);
			Write(buffer, " = ");

			Write(buffer, logic->kind);

			Write(buffer, "(");
			bool o0 = logic->operands[0] != IR_NONE;
			bool o1 = logic->operands[1] != IR_NONE;
			bool o2 = logic->operands[2] != IR_NONE;
			if (o0 || o1) {                      Write(buffer, logic->operands[0], function); }
			if (o1 || o2) { Write(buffer, ", "); Write(buffer, logic->operands[1], function); }
			if (o2)       { Write(buffer, ", "); Write(buffer, logic->operands[2], function); }
			Write(buffer, ")");

			if (logic->type)
			{
				Write(buffer, " :: ");
				Write(buffer, logic->type);
			}

			if (logic->users.count)
			{

				Write(buffer, " :: {");

				for (u32 j = 0; j < logic->users.count; j++)
				{
					if (j) Write(buffer, ",");
					Write(buffer, " i");
					Write(buffer, logic->users[j]);
				}

				Write(buffer, " }");
			}

			Write(buffer, "\n");
		}
	}
}


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
		Write(buffer, token.info.string);
	}
	else if (token.kind == TOKEN_STRING_LITERAL)
	{
		buffer->Write('"');
		Write(buffer, token.info.span);
		buffer->Write('"');
	}
	else if (token.kind == TOKEN_INTEGER_LITERAL)
	{
		Write(buffer, token.info.integer.value);
	}
	else if (token.kind == TOKEN_FLOAT_LITERAL)
	{
		Write(buffer, token.info.floating_point.value);
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

void Write(OutputBuffer* buffer, IrInstruction_Kind kind)
{
	switch (kind)
	{
		case IR_INSTRUCTION_NOP:                 buffer->Write("nop");     break;

		case IR_INSTRUCTION_PHI:                 buffer->Write("phi");     break;
		case IR_INSTRUCTION_STACK_ALLOCATE:      buffer->Write("stack");   break;
		case IR_INSTRUCTION_PARAMETER:           buffer->Write("param");   break;
		case IR_INSTRUCTION_MEMBER:              buffer->Write("member");  break;
		case IR_INSTRUCTION_ELEMENT:             buffer->Write("element"); break;
		case IR_INSTRUCTION_SELECT:              buffer->Write("select");  break;

		case IR_INSTRUCTION_LOAD:                buffer->Write("load");     break;
		case IR_INSTRUCTION_STORE:               buffer->Write("store");    break;
		case IR_INSTRUCTION_COPY:                buffer->Write("copy");     break;
		case IR_INSTRUCTION_CALL:                buffer->Write("call");     break;

		case IR_INSTRUCTION_BRANCH:              buffer->Write("branch");   break;
		case IR_INSTRUCTION_JUMP:                buffer->Write("jump");     break;
		case IR_INSTRUCTION_RETURN:              buffer->Write("return");   break;

		case IR_INSTRUCTION_ADD:                 buffer->Write("add");      break;
		case IR_INSTRUCTION_SUBTRACT:            buffer->Write("subtract"); break;
		case IR_INSTRUCTION_MULTIPLY:            buffer->Write("multiply"); break;
		case IR_INSTRUCTION_DIVIDE:              buffer->Write("divide");   break;
		case IR_INSTRUCTION_MODULO:              buffer->Write("modulo");   break;
		case IR_INSTRUCTION_EXPONENTIAL:         buffer->Write("exponential"); break;

		case IR_INSTRUCTION_NOT:                 buffer->Write("not");  break;
		case IR_INSTRUCTION_POSITIVE:            buffer->Write("pos");  break;

		case IR_INSTRUCTION_SIGN_EXTEND:         buffer->Write("sign_extend"); break;
		case IR_INSTRUCTION_ZERO_EXTEND:         buffer->Write("zero_extend"); break;

		case IR_INSTRUCTION_NARROW:              buffer->Write("narrow"); break;

		case IR_INSTRUCTION_INT_TO_FLOAT:        buffer->Write("to_float"); break;
		case IR_INSTRUCTION_FLOAT_TO_INT:        buffer->Write("to_int");   break;

		case IR_INSTRUCTION_FLOAT_CONVERT:       buffer->Write("float_convert"); break;

		case IR_INSTRUCTION_BITWISE_NOT:         buffer->Write("NOT"); break;
		case IR_INSTRUCTION_BITWISE_OR:          buffer->Write("OR");  break;
		case IR_INSTRUCTION_BITWISE_AND:         buffer->Write("AND"); break;
		case IR_INSTRUCTION_BITWISE_XOR:         buffer->Write("XOR"); break;

		case IR_INSTRUCTION_BITWISE_LEFT_SHIFT:  buffer->Write("left_shift");  break;
		case IR_INSTRUCTION_BITWISE_RIGHT_SHIFT: buffer->Write("right_shift"); break;

		case IR_INSTRUCTION_COMPARE_EQUAL:            buffer->Write("compare_equal");            break;
		case IR_INSTRUCTION_COMPARE_NOT_EQUAL:        buffer->Write("compare_not_equal");        break;
		case IR_INSTRUCTION_COMPARE_LESS:             buffer->Write("compare_less");             break;
		case IR_INSTRUCTION_COMPARE_LESS_OR_EQUAL:    buffer->Write("compare_less_or_equal");    break;
		case IR_INSTRUCTION_COMPARE_GREATER:          buffer->Write("compare_greater");          break;
		case IR_INSTRUCTION_COMPARE_GREATER_OR_EQUAL: buffer->Write("compare_greater_or_equal"); break;

		case IR_INSTRUCTION_AND: buffer->Write("and"); break;
		case IR_INSTRUCTION_OR:  buffer->Write("or");  break;
	}
}

void Write(OutputBuffer* buffer, IrValue value)
{
	if (value.kind == IR_VALUE_NONE)
	{
		Write(buffer, "NONE");
	}
	else if (value.kind == IR_VALUE_INSTRUCTION)
	{
		Write(buffer, value.type);
		Write(buffer, " %");
		Write(buffer, value.instruction->id);
	}
	else if (value.kind == IR_VALUE_BLOCK)
	{
		Write(buffer, "block");
		Write(buffer, value.block->id);
	}
	else if (value.kind == IR_VALUE_FUNCTION)
	{
		Write(buffer, value.function->function->name);
	}
	else if (value.kind == IR_VALUE_CONSTANT)
	{
		Write(buffer, value.type);
		Write(buffer, ' ');

		switch (value.type->kind)
		{
			case TYPE_BASETYPE_BYTE:           Write(buffer, value.value_byte);               break;
			case TYPE_BASETYPE_BOOL:           Write(buffer, value.value_bool);               break;
			case TYPE_BASETYPE_UINT8:          Write(buffer, value.value_uint8);              break;
			case TYPE_BASETYPE_UINT16:         Write(buffer, value.value_uint16);             break;
			case TYPE_BASETYPE_UINT32:         Write(buffer, value.value_uint32);             break;
			case TYPE_BASETYPE_UINT64:         Write(buffer, value.value_uint64);             break;
			case TYPE_BASETYPE_INT8:           Write(buffer, value.value_int8);               break;
			case TYPE_BASETYPE_INT16:          Write(buffer, value.value_int16);              break;
			case TYPE_BASETYPE_INT32:          Write(buffer, value.value_int32);              break;
			case TYPE_BASETYPE_INT64:          Write(buffer, value.value_int64);              break;
			case TYPE_BASETYPE_FLOAT16:        Write(buffer, "TYPE_BASETYPE_FLOAT16");        break;
			case TYPE_BASETYPE_FLOAT32:        Write(buffer, value.value_float32);            break;
			case TYPE_BASETYPE_FLOAT64:        Write(buffer, value.value_float64);            break;
			case TYPE_BASETYPE_STRUCT:         Write(buffer, "TYPE_BASETYPE_STRUCT");         break;
			case TYPE_BASETYPE_ENUM:           Write(buffer, value.value_int64);              break;
			case TYPE_BASETYPE_TUPLE:          Write(buffer, "TYPE_BASETYPE_TUPLE");          break;
			case TYPE_BASETYPE_FUNCTION:       Write(buffer, "TYPE_BASETYPE_FUNCTION");       break;
			case TYPE_SPECIFIER_POINTER:       Write(buffer, (u64)value.value_pointer);       break;
			case TYPE_SPECIFIER_OPTIONAL:      Write(buffer, "TYPE_SPECIFIER_OPTIONAL");      break;
			case TYPE_SPECIFIER_DYNAMIC_ARRAY: Write(buffer, "TYPE_SPECIFIER_DYNAMIC_ARRAY"); break;
			case TYPE_SPECIFIER_FIXED_ARRAY:   Write(buffer, "TYPE_SPECIFIER_FIXED_ARRAY");   break;

			default:
				Write(buffer, value.type);
				break;
		}
	}
	else if (value.kind == IR_VALUE_TUPLE)
	{
		Write(buffer, "(");
		for (u32 i = 0; i < value.tuple.count; i++)
		{
			if (i)
			{
				Write(buffer, ", ");
			}

			Write(buffer, value.tuple[i]);
		}
		Write(buffer, ")");
	}
	else
	{
		Assert();
	}
}

void Write(OutputBuffer* buffer, List<IrValue> values)
{
	Write(buffer, '(');

	for (u32 i = 0; i < values.count; i++)
	{
		if (i)
		{
			Write(buffer, ", ");
		}

		Write(buffer, values[i]);
	}

	Write(buffer, ')');
}

void Write(OutputBuffer* buffer, IrPhi phi)
{
	Write(buffer, "(");
	Write(buffer, "block");
	Write(buffer, phi.block->id);
	Write(buffer, " -> ");
	Write(buffer, phi.value);
	Write(buffer, ")");
}

void Write(OutputBuffer* buffer, IrInstruction* instruction)
{
	if (instruction->id != u16_max)
	{
		Write(buffer, '%');
		Write(buffer, instruction->id);
		Write(buffer, " = ");
	}

	Write(buffer, instruction->kind);

	if (instruction->kind == IR_INSTRUCTION_PHI)
	{
		Write(buffer, " (");
		for (u32 i = 0; i < instruction->phis.count; i++)
		{
			if (i)
			{
				Write(buffer, ", ");
			}

			Write(buffer, instruction->phis[i]);
		}
		Write(buffer, ")");
	}
	else if (instruction->a.kind != IR_VALUE_NONE)
	{
		Write(buffer, "(");
		Write(buffer, instruction->a);

		if (instruction->b.kind != IR_VALUE_NONE)
		{
			Write(buffer, ", ");
			Write(buffer, instruction->b);

			if (instruction->c.kind != IR_VALUE_NONE)
			{
				Write(buffer, ", ");
				Write(buffer, instruction->c);
			}
		}

		Write(buffer, ")");
	}

	if (instruction->type)
	{
		Write(buffer, " -> ");
		Write(buffer, instruction->type);
	}
}

void Write(OutputBuffer* buffer, List<IrBlock*> blocks)
{
	for (u32 i = 0; i < blocks.count; i++)
	{
		if (i) Write(buffer, '\n');
		IrBlock* block = blocks[i];
		Write(buffer, block);
	}
}

void Write(OutputBuffer* buffer, List<IrBlock> blocks)
{
	for (u32 i = 0; i < blocks.count; i++)
	{
		if (i) Write(buffer, '\n');
		IrBlock* block = &blocks[i];
		Write(buffer, block);
	}
}

void Write(OutputBuffer* buffer, IrFunction* function)
{
	Write(buffer, function->function->name);
	Write(buffer, " {\n");
	Write(buffer, function->blocks);
	Write(buffer, "}\n");
}

void Write(OutputBuffer* buffer, IrBlock* block)
{
	Write(buffer, "block");
	Write(buffer, block->id);
	Write(buffer, ":\n");

	for (u32 i = 0; i < block->buckets.count; i++)
	{
		IrInstruction_Bucket* bucket = block->buckets[i];

		for (u32 j = 0; j < IR_INSTRUCTION_BUCKET_COUNT; j++)
		{
			IrInstruction* instruction = bucket->instructions + j;
			if (instruction->kind != IR_INSTRUCTION_NOP)
			{
				Write(buffer, '\t');
				Write(buffer, instruction);
				Write(buffer, '\n');
			}
		}
	}
}


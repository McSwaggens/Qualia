#include "print.h"
#include "token.h"
#include "assert.h"
#include "parser.h"
#include "ir.h"

static void Write(OutputBuffer* buffer, char c) { BufferWriteByte(buffer, c); }

static void Write(OutputBuffer* buffer, int8  n)  { Write(buffer, (int64)n); }
static void Write(OutputBuffer* buffer, int16 n)  { Write(buffer, (int64)n); }
static void Write(OutputBuffer* buffer, int32 n)  { Write(buffer, (int64)n); }
static void Write(OutputBuffer* buffer, uint8  n) { Write(buffer, (uint64)n); }
static void Write(OutputBuffer* buffer, uint16 n) { Write(buffer, (uint64)n); }
static void Write(OutputBuffer* buffer, uint32 n) { Write(buffer, (uint64)n); }
static void Write(OutputBuffer* buffer, unsigned long int n) { Write(buffer, (uint64)n); }

// 3 5 10 20
// Lut for n < 256?
static void Write(OutputBuffer* buffer, uint64 n)
{
	const int max = 20; // ceil(log10(pow(2, sizeof(n)*8-1)))
	char digits[max];
	int count = 0;

	do
	{
		digits[max - count - 1] = '0' + n % 10;
	} while (++count < max && (n /= 10));

	BufferWriteData(buffer, digits + (max - count), count);
}

static void Write(OutputBuffer* buffer, int64 n)
{
	if (n < 0) { BufferWriteByte(buffer, '-'); n = -n; }
	Write(buffer, (uint64)n);
}

static void Write(OutputBuffer* buffer, void* p) { Write(buffer, Hex((uint64)p)); }

static void GenericWriteHex(OutputBuffer* buffer, uint64 n)
{
	const uint8 length_table[65] = {
		16, 16, 16, 16,
		15, 15, 15, 15,
		14, 14, 14, 14,
		13, 13, 13, 13,
		12, 12, 12, 12,
		11, 11, 11, 11,
		10, 10, 10, 10,
		9,  9,  9,  9,
		8,  8,  8,  8,
		7,  7,  7,  7,
		6,  6,  6,  6,
		5,  5,  5,  5,
		4,  4,  4,  4,
		3,  3,  3,  3,
		2,  2,  2,  2,
		1,  1,  1,  1, 1
	};

	char character_buffer[17];

	uint64 digits = length_table[CountLeadingZeroes64(n)];
	uint64 k = digits << 2;

	for (uint64 i = 0; i < digits; i++)
	{
		k -= 4;
		character_buffer[i] = "0123456789ABCDEF"[(n >> k) & 0xF];
	}

	character_buffer[digits] = 'h';
	BufferWriteData(buffer, character_buffer, digits+1);
}

static void GenericWriteBin(OutputBuffer* buffer, uint64 n)
{
	if (!n)
	{
		BufferWriteString(buffer, "0b");
		return;
	}

	const uint32 table[16] = {
		0x30303030,
		0x31303030,
		0x30313030,
		0x31313030,
		0x30303130,
		0x31303130,
		0x30313130,
		0x31313130,
		0x30303031,
		0x31303031,
		0x30313031,
		0x31313031,
		0x30303131,
		0x31303131,
		0x30313131,
		0x31313131,
	};

	char character_buffer[65];
	int64 lz = CountLeadingZeroes64(n);

	for (int64 i = 0; i < 16; i++)
	{
		((uint32*)character_buffer)[i] = table[(n >> (60-(i*4))) & 0x0f];
	}

	character_buffer[64] = 'b';
	BufferWriteData(buffer, character_buffer+lz, 65-lz);
}

static void Write(OutputBuffer* buffer, IntFormat format)
{
	switch (format.base)
	{
		case BASE_DECIMAL: Write(buffer,    format.value); break;
		case BASE_HEX:     GenericWriteHex(buffer, format.value); break;
		case BASE_BINARY:  GenericWriteBin(buffer, format.value); break;
	}
}

static void Write(OutputBuffer* buffer, String str)
{
	if (!str.data) COLD
	{
		BufferWriteString(buffer, "<null-string>");
	}

	BufferWriteData(buffer, str.data, str.length);
}

static void Write(OutputBuffer* buffer, float32 f)
{
	Write(buffer, (float64)f);
}

static void Write(OutputBuffer* buffer, float64 f)
{
	// @FixMe: This really isn't that great, but it's good enough for now.
	Write(buffer, (int64)f);
	BufferWriteByte(buffer, '.');
	Write(buffer, (int64)Abs((f-(int64)f) * Pow(10, 9)));
}

static void Write(OutputBuffer* buffer, Token_Kind kind)
{
	Write(buffer, ToString(kind));
}

static void Write(OutputBuffer* buffer, Token* token)
{
	if (!token)
	{
		BufferWriteString(buffer, "null");
		return;
	}

	Write(buffer, *token);
}

static void Write(OutputBuffer* buffer, Token token)
{
	switch (token.kind)
	{
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL:
		{
			Write(buffer, token.identifier_string);
		} break;

		case TOKEN_LITERAL_STRING:
		{
			BufferWriteByte(buffer, '"');
			Write(buffer, token.literal_string);
			BufferWriteByte(buffer, '"');
		} break;

		case TOKEN_LITERAL_INT:
		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT:
		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
		{
			Write(buffer, token.literal_int);
		} break;

		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT16:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		{
			Write(buffer, token.literal_float);
		} break;

		default:
		{
			Write(buffer, token.kind);
		} break;
	}
}

static void Write(OutputBuffer* buffer, TypeID type)
{
	TypeInfo* info = GetTypeInfo(type);

	if (!type)
	{
		BufferWriteString(buffer, "TYPE_NULL");
		return;
	}

	switch (GetTypeKind(type))
	{
		case TYPE_PRIMITIVE:
		{
			switch (type)
			{
				case TYPE_BYTE:    BufferWriteString(buffer, "byte");    break;
				case TYPE_BOOL:    BufferWriteString(buffer, "bool");    break;
				case TYPE_UINT8:   BufferWriteString(buffer, "uint8");   break;
				case TYPE_UINT16:  BufferWriteString(buffer, "uint16");  break;
				case TYPE_UINT32:  BufferWriteString(buffer, "uint32");  break;
				case TYPE_UINT64:  BufferWriteString(buffer, "uint64");  break;
				case TYPE_INT8:    BufferWriteString(buffer, "int8");    break;
				case TYPE_INT16:   BufferWriteString(buffer, "int16");   break;
				case TYPE_INT32:   BufferWriteString(buffer, "int32");   break;
				case TYPE_INT64:   BufferWriteString(buffer, "int64");   break;
				case TYPE_FLOAT16: BufferWriteString(buffer, "float16"); break;
				case TYPE_FLOAT32: BufferWriteString(buffer, "float32"); break;
				case TYPE_FLOAT64: BufferWriteString(buffer, "float64"); break;
				default: break;
			}
		} return;

		case TYPE_TUPLE:
		{
			TupleTypeInfo tuple_info = info->tuple_info;
			BufferWriteByte(buffer, '(');

			for (int32 i = 0; i < tuple_info.count; i++)
			{
				if (i) BufferWriteString(buffer, ", ");
				Write(buffer, tuple_info.elements[i]);
			}

			BufferWriteByte(buffer, ')');
		} return;

		case TYPE_FUNCTION:
		{
			FunctionTypeInfo function_info = info->function_info;

			bool is_input_type_tuple = GetTypeKind(function_info.input) == TYPE_TUPLE;

			if (!is_input_type_tuple)
				BufferWriteByte(buffer, '(');

			Write(buffer, function_info.input);

			if (!is_input_type_tuple)
				BufferWriteByte(buffer, ')');

			BufferWriteString(buffer, " -> ");

			Write(buffer, function_info.output);
		} return;

		case TYPE_STRUCT:
		{
			StructTypeInfo struct_info = info->struct_info;
			Ast_Struct* ast = struct_info.ast;
			BufferWriteString(buffer, ast->name);
		} return;

		case TYPE_ENUM:
		{
			EnumTypeInfo enum_info = info->enum_info;
			Ast_Enum* ast = enum_info.ast;
			BufferWriteString(buffer, ast->name);
		} return;

		case TYPE_POINTER:
		{
			PointerTypeInfo* ptr_info = &info->pointer_info;
			BufferWriteByte(buffer, '*');
			Write(buffer, ptr_info->subtype);
		} return;

		case TYPE_OPTIONAL:
		{
			BufferWriteByte(buffer, '?');
			Write(buffer, info->optional_info.subtype);
		} return;

		case TYPE_ARRAY:
		{
			BufferWriteString(buffer, "[]");
			Write(buffer, info->array_info.subtype);
		} return;

		case TYPE_FIXED_ARRAY:
		{
			BufferWriteByte(buffer, '[');
			Write(buffer, info->fixed_info.length);
			BufferWriteByte(buffer, ']');
			Write(buffer, info->fixed_info.subtype);
		} return;
	}
}

static void Write(OutputBuffer* buffer, Ast_Type& type)
{
	Write(buffer, &type);
}

static void Write(OutputBuffer* buffer, Ast_Type* type)
{
	if (!type)
	{
		BufferWriteString(buffer, "null");
		return;
	}

	for (Ast_Specifier* specifier = type->specifiers; specifier < type->specifiers.End(); specifier++)
	{
		switch (specifier->kind)
		{
			case AST_SPECIFIER_POINTER:   BufferWriteString(buffer, "*"); break;
			case AST_SPECIFIER_OPTIONAL:  BufferWriteString(buffer, "?"); break;
			case AST_SPECIFIER_ARRAY:     BufferWriteString(buffer, "[]");
			case AST_SPECIFIER_FIXED_ARRAY:
			{
				BufferWriteString(buffer, "[");
				Write(buffer, specifier->size_expression);
				BufferWriteString(buffer, "]");
			} break;
		}
	}

	switch (type->basetype.kind)
	{
		case AST_BASETYPE_PRIMITIVE: Write(buffer, type->basetype.token); break;
		case AST_BASETYPE_USERTYPE:  Write(buffer, type->basetype.token); break;

		case AST_BASETYPE_TUPLE:
		{
			BufferWriteString(buffer, "(");

			for (Ast_Type* t = type->basetype.tuple; t < type->basetype.tuple.End(); t++)
			{
				if (t != type->basetype.tuple) BufferWriteString(buffer, ", ");
				Write(buffer, t);
			}

			BufferWriteString(buffer, ")");
		} break;

		case AST_BASETYPE_FUNCTION:
		{
			BufferWriteString(buffer, "(");
			Write(buffer, type->basetype.function.input);
			BufferWriteString(buffer, ") -> (");
			Write(buffer, type->basetype.function.output);
			BufferWriteString(buffer, ")");
		} break;
	}
}

static void Write(OutputBuffer* buffer, Ast_Expression* expression)
{
	if (!expression)
	{
		BufferWriteString(buffer, "null");
		return;
	}

	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* variable = (Ast_Expression_Variable*)expression;
			BufferWriteString(buffer, "(Variable: ");
			Write(buffer, variable->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_FUNCTION:
		{
			Ast_Expression_Function* function = (Ast_Expression_Function*)expression;
			BufferWriteString(buffer, "(Function: ");
			Write(buffer, function->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_INTRINSIC:
		{
			Ast_Expression_Intrinsic* intrinsic = (Ast_Expression_Intrinsic*)expression;
			BufferWriteString(buffer, "(Intrinsic: ");
			Write(buffer, intrinsic->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		{
			Ast_Expression_Struct* structure = (Ast_Expression_Struct*)expression;
			BufferWriteString(buffer, "(Struct: ");
			Write(buffer, structure->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM:
		{
			Ast_Expression_Enum* enumeration = (Ast_Expression_Enum*)expression;
			BufferWriteString(buffer, "(Enum: ");
			Write(buffer, enumeration->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		{
			Ast_Expression_Struct_Member* member = (Ast_Expression_Struct_Member*)expression;
			BufferWriteString(buffer, "(Struct_Member: ");
			Write(buffer, member->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		{
			Ast_Expression_Enum_Member* member = (Ast_Expression_Enum_Member*)expression;
			BufferWriteString(buffer, "(Enum_Member: ");
			Write(buffer, member->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_ARRAY:
		{
			Ast_Expression_Array* array = (Ast_Expression_Array*)expression;
			BufferWriteString(buffer, "[ ");
			Write(buffer, array->left);
			BufferWriteString(buffer, " .. ");
			Write(buffer, array->right);
			BufferWriteString(buffer, " ]");
		}
		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Ast_Expression_Fixed_Array* fixed_array = (Ast_Expression_Fixed_Array*)expression;

			BufferWriteString(buffer, "{ ");

			for (uint32 i = 0; i < fixed_array->elements.count; i++)
			{
				if (!i) BufferWriteString(buffer, ", ");

				Write(buffer, fixed_array->elements[i]);
			}

			BufferWriteString(buffer, " }");
		} break;

		case AST_EXPRESSION_TERMINAL_NAME:
		case AST_EXPRESSION_TERMINAL_LITERAL:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_ARRAY_BEGIN:
		case AST_EXPRESSION_TERMINAL_ARRAY_END:
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
			BufferWriteString(buffer, "(");
			Write(buffer, binary->left);
			BufferWriteString(buffer, " ");
			Write(buffer, binary->op);
			BufferWriteString(buffer, " ");
			Write(buffer, binary->right);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			BufferWriteString(buffer, "(");
			Write(buffer, unary->op);
			BufferWriteString(buffer, " ");
			Write(buffer, unary->subexpression);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			Write(buffer, subscript->array);
			BufferWriteString(buffer, "[");
			Write(buffer, subscript->index);
			BufferWriteString(buffer, "]");
		} break;

		case AST_EXPRESSION_DOT_CALL:
		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;
			Write(buffer, call->function);
			if (call->parameters->kind != AST_EXPRESSION_TUPLE)
			{
				BufferWriteString(buffer, "(");
				Write(buffer, call->parameters);
				BufferWriteString(buffer, ")");
			}
			else
			{
				Write(buffer, call->parameters);
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;
			BufferWriteString(buffer, "(");
			for (uint32 i = 0; i < tuple->elements.count; i++)
			{
				if (i) BufferWriteString(buffer, ", ");
				Write(buffer, tuple->elements[i]);
			}
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			BufferWriteString(buffer, "(");
			Write(buffer, ternary->left);
			BufferWriteString(buffer, " if ");
			Write(buffer, ternary->middle);
			BufferWriteString(buffer, " else ");
			Write(buffer, ternary->right);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;

			BufferWriteString(buffer, "(");
			Write(buffer, as->expression);
			BufferWriteString(buffer, " as ");
			Write(buffer, as->type);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			BufferWriteString(buffer, "(Implicit_Cast: ");
			Write(buffer, cast->type);
			BufferWriteString(buffer, ", ");
			Write(buffer, cast->subexpression);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			BufferWriteString(buffer, "(LAMBDA)");
		} break;
	}
}

static void GenericWriteName(OutputBuffer* buffer, Instruction* instruction)
{
	BufferWriteString(buffer, "i");
	Write(buffer, instruction->id);

	Write(buffer, " ");

	if (instruction->DoesReturn())
	{
		Write(buffer, instruction->opcode);
	}
}

static void Write(OutputBuffer* buffer, Block* block)
{
	BufferWriteString(buffer, "b");
	Write(buffer, block->id);
}

static void Write(OutputBuffer* buffer, OpCode kind)
{
	Write(buffer, ToString(kind));
}

static void Write(OutputBuffer* buffer, Value value)
{
	if (value.kind == IR_NONE)
	{
		BufferWriteString(buffer, "IR_NONE");
		return;
	}

	if (value.kind == IR_INSTRUCTION)
	{
		GenericWriteName(buffer, value.instruction);
	}
	else if (value.kind == IR_CONST_INT)
	{
		Write(buffer, value.const_int);
	}
	else if (value.kind == IR_CONST_FLOAT32)
	{
		Write(buffer, value.const_f32);
	}
	else if (value.kind == IR_CONST_FLOAT64)
	{
		Write(buffer, value.const_f64);
	}
}

static void Write(OutputBuffer* buffer, Instruction* instruction)
{
	if (!instruction)
	{
		BufferWriteString(buffer, "null\n");
		return;
	}

	BufferWriteString(buffer, "\t");
	Write(buffer, ToString(instruction->opcode));

	uint32 num_ops = instruction->GetOperandCount();

	switch (instruction->opcode)
	{
		default:
		{
			for (uint32 i = 0; i < num_ops; i++)
			{
				Write(buffer, ' ');
				Write(buffer, &instruction->ops[i]);
			}
		} break;

		case IR_PHI:
		{
			for (uint32 i = 0; i < num_ops; i++)
			{
				PhiEntry* entry = &instruction->entries[i];
				if (i) BufferWriteString(buffer, ", ");
				Print(buffer, "[%: %]", entry->block, entry->value);
			}
		} break;
	}

	bool verbose = false;

	if (verbose)
	{
		if (instruction->type)
		{
			BufferWriteString(buffer, " | ");
			Write(buffer, instruction->type);
		}

		if (instruction->users.count)
		{
			BufferWriteString(buffer, " |  ");

			for (uint32 j = 0; j < instruction->users.count; j++)
			{
				if (j) BufferWriteString(buffer, ", ");
				Write(buffer, instruction->users[j]);
			}
		}
	}

	BufferWriteString(buffer, "\n");
}

static void Write(OutputBuffer* buffer, Procedure* function)
{
	// @Todo: Rewrite
}


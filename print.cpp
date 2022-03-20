#include "print.h"
#include "token.h"
#include "assert.h"
#include "parser.h"
#include "ir.h"

static void GenericWrite(OutputBuffer* buffer, char c) { BufferWriteByte(buffer, c); }

static void GenericWrite(OutputBuffer* buffer, int8  n)  { GenericWrite(buffer, (int64)n); }
static void GenericWrite(OutputBuffer* buffer, int16 n)  { GenericWrite(buffer, (int64)n); }
static void GenericWrite(OutputBuffer* buffer, int32 n)  { GenericWrite(buffer, (int64)n); }
static void GenericWrite(OutputBuffer* buffer, uint8  n) { GenericWrite(buffer, (uint64)n); }
static void GenericWrite(OutputBuffer* buffer, uint16 n) { GenericWrite(buffer, (uint64)n); }
static void GenericWrite(OutputBuffer* buffer, uint32 n) { GenericWrite(buffer, (uint64)n); }
static void GenericWrite(OutputBuffer* buffer, unsigned long int n) { GenericWrite(buffer, (uint64)n); }

// 3 5 10 20
// Lut for n < 256?
static void GenericWrite(OutputBuffer* buffer, uint64 n)
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

static void GenericWrite(OutputBuffer* buffer, int64 n)
{
	if (n < 0) { BufferWriteByte(buffer, '-'); n = -n; }
	GenericWrite(buffer, (uint64)n);
}

static void GenericWrite(OutputBuffer* buffer, void* p) { GenericWrite(buffer, Hex((uint64)p)); }

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

static void GenericWrite(OutputBuffer* buffer, IntFormat format)
{
	switch (format.base)
	{
		case BASE_DECIMAL: GenericWrite(buffer,    format.value); break;
		case BASE_HEX:     GenericWriteHex(buffer, format.value); break;
		case BASE_BINARY:  GenericWriteBin(buffer, format.value); break;
	}
}

static void GenericWrite(OutputBuffer* buffer, String str)
{
	if (!str.data) COLD
	{
		BufferWriteString(buffer, "<null-string>");
	}

	BufferWriteData(buffer, str.data, str.length);
}

static void GenericWrite(OutputBuffer* buffer, float32 f)
{
	GenericWrite(buffer, (float64)f);
}

static void GenericWrite(OutputBuffer* buffer, float64 f)
{
	// @FixMe: This really isn't that great, but it's good enough for now.
	GenericWrite(buffer, (int64)f);
	BufferWriteByte(buffer, '.');
	GenericWrite(buffer, (int64)Abs((f-(int64)f) * Pow(10, 9)));
}

static void GenericWrite(OutputBuffer* buffer, Token_Kind kind)
{
	GenericWrite(buffer, ToString(kind));
}

static void GenericWrite(OutputBuffer* buffer, Token* token)
{
	if (!token)
	{
		BufferWriteString(buffer, "null");
		return;
	}

	GenericWrite(buffer, *token);
}

static void GenericWrite(OutputBuffer* buffer, Token token)
{
	switch (token.kind)
	{
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_GENERIC:
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL:
		{
			GenericWrite(buffer, token.identifier_string);
		} break;

		case TOKEN_LITERAL_STRING:
		{
			BufferWriteByte(buffer, '"');
			GenericWrite(buffer, token.literal_string);
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
			GenericWrite(buffer, token.literal_int);
		} break;

		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT16:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		{
			GenericWrite(buffer, token.literal_float);
		} break;

		default:
		{
			GenericWrite(buffer, token.kind);
		} break;
	}
}

static void GenericWrite(OutputBuffer* buffer, Type* type)
{
	if (!type)
	{
		BufferWriteString(buffer, "null");
		return;
	}

	switch (type->kind)
	{
		case TYPE_SPECIFIER_POINTER:
			BufferWriteString(buffer, "*");
			GenericWrite(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_OPTIONAL:
			BufferWriteString(buffer, "?");
			GenericWrite(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_FIXED_ARRAY:
			BufferWriteByte(buffer, '[');
			GenericWrite(buffer, type->length);
			BufferWriteByte(buffer, ']');
			GenericWrite(buffer, type->subtype);
			break;

		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
			BufferWriteString(buffer, "[]");
			GenericWrite(buffer, type->subtype);
			break;

		case TYPE_BASETYPE_BYTE:    GenericWrite(buffer, TOKEN_BYTE);    break;
		case TYPE_BASETYPE_BOOL:    GenericWrite(buffer, TOKEN_BOOL);    break; 
		case TYPE_BASETYPE_INT8:    GenericWrite(buffer, TOKEN_INT8);    break; 
		case TYPE_BASETYPE_INT16:   GenericWrite(buffer, TOKEN_INT16);   break; 
		case TYPE_BASETYPE_INT32:   GenericWrite(buffer, TOKEN_INT32);   break; 
		case TYPE_BASETYPE_INT64:   GenericWrite(buffer, TOKEN_INT64);   break; 
		case TYPE_BASETYPE_UINT8:   GenericWrite(buffer, TOKEN_UINT8);   break; 
		case TYPE_BASETYPE_UINT16:  GenericWrite(buffer, TOKEN_UINT16);  break; 
		case TYPE_BASETYPE_UINT32:  GenericWrite(buffer, TOKEN_UINT32);  break; 
		case TYPE_BASETYPE_UINT64:  GenericWrite(buffer, TOKEN_UINT64);  break; 
		case TYPE_BASETYPE_FLOAT16: GenericWrite(buffer, TOKEN_FLOAT16); break; 
		case TYPE_BASETYPE_FLOAT32: GenericWrite(buffer, TOKEN_FLOAT32); break; 
		case TYPE_BASETYPE_FLOAT64: GenericWrite(buffer, TOKEN_FLOAT64); break; 

		case TYPE_BASETYPE_FUNCTION:
			GenericWrite(buffer, type->input);
			BufferWriteString(buffer, " -> ");
			GenericWrite(buffer, type->output);
			break;

		case TYPE_BASETYPE_TUPLE:
			BufferWriteByte(buffer, '(');
			for (uint32 i = 0; i < type->tuple.count; i++)
			{
				if (i) BufferWriteString(buffer, ", ");
				GenericWrite(buffer, type->tuple[i]);
			}
			BufferWriteByte(buffer, ')');
			break;

		case TYPE_BASETYPE_STRUCT:
			GenericWrite(buffer, type->structure->name);
			break;

		case TYPE_BASETYPE_ENUM:
			GenericWrite(buffer, type->enumeration->name);
			break;

	}
}

static void GenericWrite(OutputBuffer* buffer, Ast_Type& type)
{
	GenericWrite(buffer, &type);
}

static void GenericWrite(OutputBuffer* buffer, Ast_Type* type)
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
			case AST_SPECIFIER_ARRAY:
			{
				BufferWriteString(buffer, "[");
				GenericWrite(buffer, specifier->size_expression);
				BufferWriteString(buffer, "]");
			} break;
		}
	}

	switch (type->basetype.kind)
	{
		case AST_BASETYPE_PRIMITIVE: GenericWrite(buffer, type->basetype.token); break;
		case AST_BASETYPE_USERTYPE:  GenericWrite(buffer, type->basetype.token); break;

		case AST_BASETYPE_TUPLE:
		{
			BufferWriteString(buffer, "(");

			for (Ast_Type* t = type->basetype.tuple; t < type->basetype.tuple.End(); t++)
			{
				if (t != type->basetype.tuple) BufferWriteString(buffer, ", ");
				GenericWrite(buffer, t);
			}

			BufferWriteString(buffer, ")");
		} break;

		case AST_BASETYPE_FUNCTION:
		{
			BufferWriteString(buffer, "(");
			GenericWrite(buffer, type->basetype.function.input);
			BufferWriteString(buffer, ") -> (");
			GenericWrite(buffer, type->basetype.function.output);
			BufferWriteString(buffer, ")");
		} break;
	}
}

static void GenericWrite(OutputBuffer* buffer, Ast_Expression* expression)
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
			GenericWrite(buffer, variable->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_FUNCTION:
		{
			Ast_Expression_Function* function = (Ast_Expression_Function*)expression;
			BufferWriteString(buffer, "(Function: ");
			GenericWrite(buffer, function->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_INTRINSIC:
		{
			Ast_Expression_Intrinsic* intrinsic = (Ast_Expression_Intrinsic*)expression;
			BufferWriteString(buffer, "(Intrinsic: ");
			GenericWrite(buffer, intrinsic->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		{
			Ast_Expression_Struct* structure = (Ast_Expression_Struct*)expression;
			BufferWriteString(buffer, "(Struct: ");
			GenericWrite(buffer, structure->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM:
		{
			Ast_Expression_Enum* enumeration = (Ast_Expression_Enum*)expression;
			BufferWriteString(buffer, "(Enum: ");
			GenericWrite(buffer, enumeration->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		{
			Ast_Expression_Struct_Member* member = (Ast_Expression_Struct_Member*)expression;
			BufferWriteString(buffer, "(Struct_Member: ");
			GenericWrite(buffer, member->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		{
			Ast_Expression_Enum_Member* member = (Ast_Expression_Enum_Member*)expression;
			BufferWriteString(buffer, "(Enum_Member: ");
			GenericWrite(buffer, member->token);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_DYNAMIC_ARRAY:
		{
			Ast_Expression_Dynamic_Array* array = (Ast_Expression_Dynamic_Array*)expression;
			BufferWriteString(buffer, "[ ");
			GenericWrite(buffer, array->left);
			BufferWriteString(buffer, " .. ");
			GenericWrite(buffer, array->right);
			BufferWriteString(buffer, " ]");
		}
		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Ast_Expression_Fixed_Array* fixed_array = (Ast_Expression_Fixed_Array*)expression;

			BufferWriteString(buffer, "{ ");

			for (uint32 i = 0; i < fixed_array->elements.count; i++)
			{
				if (!i) BufferWriteString(buffer, ", ");

				GenericWrite(buffer, fixed_array->elements[i]);
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
			GenericWrite(buffer, literal->token);
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
			GenericWrite(buffer, binary->left);
			BufferWriteString(buffer, " ");
			GenericWrite(buffer, binary->op);
			BufferWriteString(buffer, " ");
			GenericWrite(buffer, binary->right);
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
			GenericWrite(buffer, unary->op);
			BufferWriteString(buffer, " ");
			GenericWrite(buffer, unary->subexpression);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			GenericWrite(buffer, subscript->array);
			BufferWriteString(buffer, "[");
			GenericWrite(buffer, subscript->index);
			BufferWriteString(buffer, "]");
		} break;

		case AST_EXPRESSION_DOT_CALL:
		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;
			GenericWrite(buffer, call->function);
			if (call->parameters->kind != AST_EXPRESSION_TUPLE)
			{
				BufferWriteString(buffer, "(");
				GenericWrite(buffer, call->parameters);
				BufferWriteString(buffer, ")");
			}
			else
			{
				GenericWrite(buffer, call->parameters);
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;
			BufferWriteString(buffer, "(");
			for (uint32 i = 0; i < tuple->elements.count; i++)
			{
				if (i) BufferWriteString(buffer, ", ");
				GenericWrite(buffer, tuple->elements[i]);
			}
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			BufferWriteString(buffer, "(");
			GenericWrite(buffer, ternary->left);
			BufferWriteString(buffer, " if ");
			GenericWrite(buffer, ternary->middle);
			BufferWriteString(buffer, " else ");
			GenericWrite(buffer, ternary->right);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;

			BufferWriteString(buffer, "(");
			GenericWrite(buffer, as->expression);
			BufferWriteString(buffer, " as ");
			GenericWrite(buffer, as->type);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			BufferWriteString(buffer, "(Implicit_Cast: ");
			GenericWrite(buffer, cast->type);
			BufferWriteString(buffer, ", ");
			GenericWrite(buffer, cast->subexpression);
			BufferWriteString(buffer, ")");
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			BufferWriteString(buffer, "(LAMBDA)");
		} break;
	}
}

static void GenericWriteName(OutputBuffer* buffer, IrInstructionID id)
{
	BufferWriteString(buffer, "%");
	GenericWrite(buffer, id);
}

static void GenericWrite(OutputBuffer* buffer, IrBlockID id)
{
	BufferWriteString(buffer, "B");
	GenericWrite(buffer, id.index);
}

static void GenericWrite(OutputBuffer* buffer, IrInstructionKind kind)
{
	GenericWrite(buffer, ToString(kind));
}

static void GenericWrite(OutputBuffer* buffer, IrValue value)
{
	if (!IsValid(value))
	{
		BufferWriteString(buffer, "IR_NONE");
		return;
	}

	if (value.kind == IR_VALUE_INSTRUCTION)
	{
		GenericWriteName(buffer, value.instruction);
	}
	else if (value.kind == IR_VALUE_CONSTANT_INT)
	{
		GenericWrite(buffer, value.constant_int);
	}
	else if (value.kind == IR_VALUE_CONSTANT_FLOAT32)
	{
		GenericWrite(buffer, value.constant_float32);
	}
	else if (value.kind == IR_VALUE_CONSTANT_FLOAT64)
	{
		GenericWrite(buffer, value.constant_float64);
	}
}

static void GenericWrite(OutputBuffer* buffer, IrFunction* function, IrInstructionID id)
{
	if (id == IR_NONE)
	{
		BufferWriteString(buffer, "IR_NONE\n");
		return;
	}

	IrInstruction* instruction = GetInstruction(function, id);

	BufferWriteString(buffer, "\t");

	IrValue iv = GetValue(id);

	IrValue op0 = instruction->operands[0];
	IrValue op1 = instruction->operands[1];
	IrValue op2 = instruction->operands[2];

	switch (instruction->kind)
	{
		case IR_NOP:          Print(buffer, "nop", iv); break;
		case IR_STACK:        Print(buffer, "% = stack",              iv); break;
		case IR_PARAMETER:    Print(buffer, "% = param %",            iv, op0); break;
		case IR_NOT:          Print(buffer, "% = not %",              iv, op0); break;
		case IR_SIGN_EXTEND:  Print(buffer, "% = sign extend %",      iv, op0); break;
		case IR_ZERO_EXTEND:  Print(buffer, "% = zero extend %",      iv, op0); break;
		case IR_TRUNCATE:     Print(buffer, "% = truncate %",         iv, op0); break;
		case IR_INT_TO_FLOAT: Print(buffer, "% = int_to_float %",     iv, op0); break;
		case IR_FLOAT_TO_INT: Print(buffer, "% = float_to_int %",     iv, op0); break;
		case IR_FLOAT_CAST:   Print(buffer, "% = float_cast %",       iv, op0); break;
		case IR_LOAD:         Print(buffer, "% = load %",             iv, op0); break;
		case IR_BRANCH:       Print(buffer, "branch %",               op0); break;
		case IR_CALL:         Print(buffer, "% = call %",             iv, op0); break;
		case IR_INT_ADD:              Print(buffer, "% = add %, %",  iv, op0, op1); break;
		case IR_INT_SUBTRACT:         Print(buffer, "% = sub %, %",  iv, op0, op1); break;
		case IR_INT_MULTIPLY:         Print(buffer, "% = mul %, %",  iv, op0, op1); break;
		case IR_SIGNED_DIVIDE:        Print(buffer, "% = sdiv %, %", iv, op0, op1); break;
		case IR_UNSIGNED_DIVIDE:      Print(buffer, "% = udiv %, %", iv, op0, op1); break;
		case IR_SIGNED_MODULO:        Print(buffer, "% = smod %, %",  iv, op0, op1); break;
		case IR_UNSIGNED_MODULO:      Print(buffer, "% = umod %, %",  iv, op0, op1); break;
		case IR_SIGNED_LEFT_SHIFT:    Print(buffer, "% = sls %, %", iv, op0, op1); break;
		case IR_SIGNED_RIGHT_SHIFT:   Print(buffer, "% = srs %, %", iv, op0, op1); break;
		case IR_UNSIGNED_LEFT_SHIFT:  Print(buffer, "% = uls %, %", iv, op0, op1); break;
		case IR_UNSIGNED_RIGHT_SHIFT: Print(buffer, "% = urs %, %", iv, op0, op1); break;
		case IR_FLOAT_ADD:            Print(buffer, "% = fadd %, %", iv, op0, op1); break;
		case IR_FLOAT_SUBTRACT:       Print(buffer, "% = fsub %, %", iv, op0, op1); break;
		case IR_FLOAT_MULTIPLY:       Print(buffer, "% = fmul %, %", iv, op0, op1); break;
		case IR_FLOAT_DIVIDE:         Print(buffer, "% = fdiv %, %",  iv, op0, op1); break;
		case IR_OR:                   Print(buffer, "% = or %, %",    iv, op0, op1); break;
		case IR_AND:                  Print(buffer, "% = and %, %",   iv, op0, op1); break;
		case IR_XOR:                  Print(buffer, "% = xor %, %",   iv, op0, op1); break;
		case IR_COMPARE_EQUAL:                     Print(buffer, "% = cmp % = %",   iv, op0, op1); break;
		case IR_COMPARE_NOT_EQUAL:                 Print(buffer, "% = cmp % != %",  iv, op0, op1); break;
		case IR_SIGNED_COMPARE_LESS:               Print(buffer, "% = scmp % < %",  iv, op0, op1); break;
		case IR_SIGNED_COMPARE_LESS_OR_EQUAL:      Print(buffer, "% = scmp % <= %", iv, op0, op1); break;
		case IR_SIGNED_COMPARE_GREATER:            Print(buffer, "% = scmp % > %",  iv, op0, op1); break;
		case IR_SIGNED_COMPARE_GREATER_OR_EQUAL:   Print(buffer, "% = scmp % >= %", iv, op0, op1); break;
		case IR_UNSIGNED_COMPARE_LESS:             Print(buffer, "% = ucmp % < %",  iv, op0, op1); break;
		case IR_UNSIGNED_COMPARE_LESS_OR_EQUAL:    Print(buffer, "% = ucmp % <= %", iv, op0, op1); break;
		case IR_UNSIGNED_COMPARE_GREATER:          Print(buffer, "% = ucmp % > %",  iv, op0, op1); break;
		case IR_UNSIGNED_COMPARE_GREATER_OR_EQUAL: Print(buffer, "% = ucmp % >= %", iv, op0, op1); break;
		case IR_FLOAT_COMPARE_LESS:             Print(buffer, "% = fcmp % < %",  iv, op0, op1); break;
		case IR_FLOAT_COMPARE_LESS_OR_EQUAL:    Print(buffer, "% = fcmp % <= %", iv, op0, op1); break;
		case IR_FLOAT_COMPARE_GREATER:          Print(buffer, "% = fcmp % > %",  iv, op0, op1); break;
		case IR_FLOAT_COMPARE_GREATER_OR_EQUAL: Print(buffer, "% = fcmp % >= %", iv, op0, op1); break;
		case IR_MEMBER: Print(buffer, "% = member %, %",        iv, op0, op1); break;
		case IR_INDEX:  Print(buffer, "% = index %[%]",         iv, op0, op1); break;
		case IR_COPY:   Print(buffer, "copy %, %",              op0, op1); break;
		case IR_STORE:  Print(buffer, "store %, %",             op0, op1); break;
		case IR_SELECT: Print(buffer, "% = select %, % else %", iv, op0, op1, op2); break;
		case IR_RETURN:
		{
			if (IsValid(op0)) Print(buffer, "return %", op0);
			else              Print(buffer, "return");
		} break;

		case IR_PHI:
		{
			Print(buffer, "% = phi ", iv);
			for (uint32 i = 0; i < instruction->phi_entries.count; i++)
			{
				IrPhiEntry* entry = &instruction->phi_entries[i];
				if (i) BufferWriteString(buffer, ", ");
				Print(buffer, "[%: %]", entry->block, entry->value);
			}
		} break;

		case IR_TUPLE:
		{
			BufferWriteString(buffer, "(");
			for (uint32 i = 0; i < instruction->tuple.count; i++)
			{
				if (i) BufferWriteString(buffer, ", ");
				GenericWrite(buffer, instruction->tuple[i]);
			}
			BufferWriteString(buffer, ")");
		} break;
	}

	bool verbose = false;

	if (verbose)
	{
		if (instruction->type)
		{
			BufferWriteString(buffer, " :: ");
			GenericWrite(buffer, instruction->type);
		}

		if (instruction->users.count)
		{
			BufferWriteString(buffer, " :: { ");

			for (uint32 j = 0; j < instruction->users.count; j++)
			{
				if (j) BufferWriteString(buffer, ", ");
				GenericWrite(buffer, function, instruction->users[j]);
			}

			BufferWriteString(buffer, " }");
		}
	}

	BufferWriteString(buffer, "\n");
}

static void GenericWrite(OutputBuffer* buffer, IrFunction* function)
{
	Ast_Function* ast = function->function;
	Print(buffer, "% % -> %:\n", ast->name, ast->type->input, ast->type->output);

	for (uint32 b = 0; b < function->blocks.count; b++)
	{
		IrBlock* block = &function->blocks[b];

		if (block->kind == IR_BLOCK_FREE)
		{
			continue;
		}

		GenericWrite(buffer, IrBlockID { b });
		BufferWriteString(buffer, ":\n");

		for (uint32 i = 0; i < block->phis.count; i++)
		{
			IrInstructionID id = block->phis[i];
			GenericWrite(buffer, function, id);
		}

		for (uint32 i = 0; i < block->instructions.count; i++)
		{
			IrInstructionID id = block->instructions[i];
			IrInstruction* instruction = &function->instructions[id];

			if (!IsControlFlowInstruction(instruction->kind) && instruction->kind != IR_PHI)
			{
				GenericWrite(buffer, function, id);
			}
		}

		if (block->kind == IR_BLOCK_JUMP)
		{
			BufferWriteString(buffer, "\tjump ");
			GenericWrite(buffer, block->jump);
			BufferWriteString(buffer, "\n");
		}
		else if (block->kind == IR_BLOCK_BRANCH)
		{
			BufferWriteString(buffer, "\tbranch ");
			GenericWrite(buffer, GetInstruction(function, block->branch.branch_instruction)->operands[0]);
			BufferWriteString(buffer, ", ");
			GenericWrite(buffer, block->branch.true_branch);
			BufferWriteString(buffer, " else ");
			GenericWrite(buffer, block->branch.false_branch);
			BufferWriteString(buffer, "\n");
		}
		else if (block->kind == IR_BLOCK_RETURN)
		{
			if (block->return_instruction != IR_NONE)
			{
				GenericWrite(buffer, function, block->return_instruction);
			}
			else
			{
				BufferWriteString(buffer, "\tNO RETURN INSTRUCTION IN RETURN BLOCK REEEEEEEEEEEEEE\n");
			}
		}
		else if (block->kind == IR_BLOCK_UNSPECIFIED)
		{
			BufferWriteString(buffer, "\tUNSPECIFIED REEEEEEEEEEEEEEEEEEEEEEEEE\n");
		}
	}
}


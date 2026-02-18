#include "print.h"
#include "token.h"
#include "assert.h"
#include "parser.h"
#include "ir.h"
#include "math.h"

static void Write(OutputBuffer* buffer, char c) { buffer->Write(c); }

// 3 5 10 20
// Lut for n < 256?
static void Write(OutputBuffer* buffer, u64 n) {
	const int max = 20; // ceil(log10(pow(2, sizeof(n)*8-1)))
	char digits[max];
	int count = 0;

	do {
		digits[max - count - 1] = '0' + n % 10;
	} while (++count < max && (n /= 10));

	buffer->Write(digits + (max - count), count);
}

static void Write(OutputBuffer* buffer, s8  n) { Write(buffer, (s64)n); }
static void Write(OutputBuffer* buffer, s16 n) { Write(buffer, (s64)n); }
static void Write(OutputBuffer* buffer, s32 n) { Write(buffer, (s64)n); }
static void Write(OutputBuffer* buffer, u8  n) { Write(buffer, (u64)n); }
static void Write(OutputBuffer* buffer, u16 n) { Write(buffer, (u64)n); }
static void Write(OutputBuffer* buffer, u32 n) { Write(buffer, (u64)n); }
static void Write(OutputBuffer* buffer, unsigned long int n) { Write(buffer, (u64)n); }

static void Write(OutputBuffer* buffer, s64 n) {
	if (n < 0) { buffer->Write('-'); n = -n; }
	Write(buffer, (u64)n);
}

static void Write(OutputBuffer* buffer, void* p) { Write(buffer, Hex((u64)p)); }

static void GenericWriteHex(OutputBuffer* buffer, u64 n) {
	const u8 length_table[65] = {
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

	u64 digits = length_table[Ctz64(n)];
	u64 k = digits << 2;

	for (u64 i = 0; i < digits; i++) {
		k -= 4;
		character_buffer[i] = "0123456789ABCDEF"[(n >> k) & 0xF];
	}

	character_buffer[digits] = 'h';
	buffer->Write(character_buffer, digits+1);
}

static void GenericWriteBin(OutputBuffer* buffer, u64 n) {
	if (!n) {
		buffer->Write("0b");
		return;
	}

	const u32 table[16] = {
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
	s64 lz = Ctz64(n);

	for (s64 i = 0; i < 16; i++) {
		((u32*)character_buffer)[i] = table[(n >> (60-(i*4))) & 0x0f];
	}

	character_buffer[64] = 'b';
	buffer->Write(character_buffer+lz, 65-lz);
}

static void Write(OutputBuffer* buffer, IntFormat format) {
	switch (format.base) {
		case BASE_DECIMAL: Write(buffer,    format.value); break;
		case BASE_HEX:     GenericWriteHex(buffer, format.value); break;
		case BASE_BINARY:  GenericWriteBin(buffer, format.value); break;
	}
}

static void Write(OutputBuffer* buffer, String str) {
	if (!str.data) COLD
		buffer->Write("<null-string>");

	buffer->Write(str.data, str.length);
}

static void Write(OutputBuffer* buffer, float32 f) {
	Write(buffer, (float64)f);
}

static void Write(OutputBuffer* buffer, float64 f) {
	// @FixMe: This really isn't that great, but it's good enough for now.
	Write(buffer, (s64)f);
	buffer->Write('.');
	Write(buffer, (s64)Abs((f-(s64)f) * Pow(10, 9)));
}

static void Write(OutputBuffer* buffer, TokenKind kind) {
	Write(buffer, ToString(kind));
}

static void Write(OutputBuffer* buffer, Token* token) {
	if (!token) {
		buffer->Write("null");
		return;
	}

	Write(buffer, *token);
}

static void Write(OutputBuffer* buffer, Token token) {
	switch (token.kind) {
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL: {
			Write(buffer, token.identifier_string);
		} break;

		case TOKEN_LITERAL_STRING: {
			buffer->Write('"');
			Write(buffer, token.literal_string);
			buffer->Write('"');
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
		case TOKEN_LITERAL_UINT64: {
			Write(buffer, token.literal_int);
		} break;

		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64: {
			Write(buffer, token.literal_float);
		} break;

		default: {
			Write(buffer, token.kind);
		} break;
	}
}

static void Write(OutputBuffer* buffer, TypeID type) {
	TypeInfo* info = type.GetInfo();

	if (!type) {
		buffer->Write("TYPE_NULL");
		return;
	}

	switch (type.GetKind()) {
		case TYPE_PRIMITIVE: {
			switch (type) {
				case TYPE_BYTE:    buffer->Write("byte");    break;
				case TYPE_BOOL:    buffer->Write("bool");    break;
				case TYPE_UINT8:   buffer->Write("uint8");   break;
				case TYPE_UINT16:  buffer->Write("uint16");  break;
				case TYPE_UINT32:  buffer->Write("uint32");  break;
				case TYPE_UINT64:  buffer->Write("uint64");  break;
				case TYPE_INT8:    buffer->Write("int8");    break;
				case TYPE_INT16:   buffer->Write("int16");   break;
				case TYPE_INT32:   buffer->Write("int32");   break;
				case TYPE_INT64:   buffer->Write("int64");   break;
				case TYPE_FLOAT32: buffer->Write("float32"); break;
				case TYPE_FLOAT64: buffer->Write("float64"); break;
				default: break;
			}
		} return;

		case TYPE_TUPLE: {
			TypeInfo::Tuple tuple_info = info->tuple_info;
			buffer->Write('(');

			for (s32 i = 0; i < tuple_info.elements.length; i++) {
				if (i) buffer->Write(", ");
				Write(buffer, tuple_info.elements[i]);
			}

			buffer->Write(')');
		} return;

		case TYPE_FUNCTION: {
			TypeInfo::Function function_info = info->function_info;

			bool is_input_type_tuple = function_info.input.GetKind() == TYPE_TUPLE;

			if (!is_input_type_tuple)
				buffer->Write('(');

			Write(buffer, function_info.input);

			if (!is_input_type_tuple)
				buffer->Write(')');

			buffer->Write(" -> ");

			Write(buffer, function_info.output);
		} return;

		case TYPE_STRUCT: {
			TypeInfo::Struct struct_info = info->struct_info;
			Ast::Struct* ast = struct_info.ast;
			buffer->Write(ast->name);
		} return;

		case TYPE_ENUM: {
			TypeInfo::Enum enum_info = info->enum_info;
			Ast::Enum* ast = enum_info.ast;
			buffer->Write(ast->name);
		} return;

		case TYPE_POINTER: {
			TypeInfo::Pointer* ptr_info = &info->pointer_info;
			buffer->Write('*');
			Write(buffer, ptr_info->subtype);
		} return;

		case TYPE_OPTIONAL: {
			buffer->Write('?');
			Write(buffer, info->optional_info.subtype);
		} return;

		case TYPE_ARRAY: {
			buffer->Write("[]");
			Write(buffer, info->array_info.subtype);
		} return;

		case TYPE_FIXED_ARRAY: {
			buffer->Write('[');
			Write(buffer, info->fixed_info.length);
			buffer->Write(']');
			Write(buffer, info->fixed_info.subtype);
		} return;

		case TYPE_REFERENCE: {
			buffer->Write("(ref)");
			Write(buffer, info->reference_info.subtype);
		} return;
	}
}

static void Write(OutputBuffer* buffer, Ast::Type& type) {
	Write(buffer, &type);
}

static void Write(OutputBuffer* buffer, Ast::Type* type) {
	if (!type) {
		buffer->Write("null");
		return;
	}

	for (Ast::Specifier* specifier = type->specifiers; specifier < type->specifiers.End(); specifier++) {
		switch (specifier->kind) {
			case Ast::SPECIFIER_POINTER:   buffer->Write("*"); break;
			case Ast::SPECIFIER_OPTIONAL:  buffer->Write("?"); break;
			case Ast::SPECIFIER_ARRAY:     buffer->Write("[]");
			case Ast::SPECIFIER_FIXED_ARRAY: {
				buffer->Write("[");
				Write(buffer, specifier->size_expression);
				buffer->Write("]");
			} break;
		}
	}

	switch (type->basetype.kind) {
		case Ast::BASETYPE_PRIMITIVE: Write(buffer, type->basetype.token); break;
		case Ast::BASETYPE_USERTYPE:  Write(buffer, type->basetype.token); break;

		case Ast::BASETYPE_TUPLE: {
			buffer->Write("(");

			for (Ast::Type* t = type->basetype.tuple; t < type->basetype.tuple.End(); t++) {
				if (t != type->basetype.tuple) buffer->Write(", ");
				Write(buffer, t);
			}

			buffer->Write(")");
		} break;

		case Ast::BASETYPE_FUNCTION: {
			buffer->Write("(");
			Write(buffer, type->basetype.function.input);
			buffer->Write(") -> (");
			Write(buffer, type->basetype.function.output);
			buffer->Write(")");
		} break;
	}
}

static void Write(OutputBuffer* buffer, Ast::Expression* expression) {
	if (!expression) {
		buffer->Write("null");
		return;
	}

	switch (expression->kind) {
		case Ast::Expression::TERMINAL_VARIABLE: {
			Ast::Expression_Variable* variable = (Ast::Expression_Variable*)expression;
			buffer->Write("(Variable: ");
			Write(buffer, variable->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_FUNCTION: {
			Ast::Expression_Function* function = (Ast::Expression_Function*)expression;
			buffer->Write("(Function: ");
			Write(buffer, function->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_INTRINSIC: {
			Ast::Expression_Intrinsic* intrinsic = (Ast::Expression_Intrinsic*)expression;
			buffer->Write("(Intrinsic: ");
			Write(buffer, intrinsic->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_STRUCT: {
			Ast::Expression_Struct* structure = (Ast::Expression_Struct*)expression;
			buffer->Write("(Struct: ");
			Write(buffer, structure->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_ENUM: {
			Ast::Expression_Enum* enumeration = (Ast::Expression_Enum*)expression;
			buffer->Write("(Enum: ");
			Write(buffer, enumeration->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_STRUCT_MEMBER: {
			Ast::Expression_Struct_Member* member = (Ast::Expression_Struct_Member*)expression;
			buffer->Write("(Struct_Member: ");
			Write(buffer, member->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::TERMINAL_ENUM_MEMBER: {
			Ast::Expression_Enum_Member* member = (Ast::Expression_Enum_Member*)expression;
			buffer->Write("(Enum_Member: ");
			Write(buffer, member->token);
			buffer->Write(")");
		} break;

		case Ast::Expression::ARRAY: {
			Ast::Expression_Array* array = (Ast::Expression_Array*)expression;
			buffer->Write("[ ");
			Write(buffer, array->left);
			buffer->Write(" .. ");
			Write(buffer, array->right);
			buffer->Write(" ]");
		}
		case Ast::Expression::FIXED_ARRAY: {
			Ast::Expression_Fixed_Array* fixed_array = (Ast::Expression_Fixed_Array*)expression;

			buffer->Write("{ ");

			for (u32 i = 0; i < fixed_array->elements.length; i++) {
				if (!i) buffer->Write(", ");

				Write(buffer, fixed_array->elements[i]);
			}

			buffer->Write(" }");
		} break;

		case Ast::Expression::TERMINAL_NAME:
		case Ast::Expression::TERMINAL_LITERAL:
		case Ast::Expression::TERMINAL_PRIMITIVE:
		case Ast::Expression::TERMINAL_ARRAY_BEGIN:
		case Ast::Expression::TERMINAL_ARRAY_END:
		case Ast::Expression::TERMINAL_ARRAY_LENGTH: {
			Ast::Expression_Literal* literal = (Ast::Expression_Literal*)expression;
			Write(buffer, literal->token);
		} break;

		case Ast::Expression::BINARY_COMPARE_EQUAL:
		case Ast::Expression::BINARY_COMPARE_NOT_EQUAL:
		case Ast::Expression::BINARY_COMPARE_LESS:
		case Ast::Expression::BINARY_COMPARE_LESS_OR_EQUAL:
		case Ast::Expression::BINARY_COMPARE_GREATER:
		case Ast::Expression::BINARY_COMPARE_GREATER_OR_EQUAL:
		case Ast::Expression::BINARY_DOT:
		case Ast::Expression::BINARY_ADD:
		case Ast::Expression::BINARY_SUBTRACT:
		case Ast::Expression::BINARY_MULTIPLY:
		case Ast::Expression::BINARY_DIVIDE:
		case Ast::Expression::BINARY_MODULO:
		case Ast::Expression::BINARY_BITWISE_OR:
		case Ast::Expression::BINARY_BITWISE_XOR:
		case Ast::Expression::BINARY_BITWISE_AND:
		case Ast::Expression::BINARY_LEFT_SHIFT:
		case Ast::Expression::BINARY_RIGHT_SHIFT:
		case Ast::Expression::BINARY_AND:
		case Ast::Expression::BINARY_OR: {
			Ast::Expression_Binary* binary = (Ast::Expression_Binary*)expression;
			buffer->Write("(");
			Write(buffer, binary->left);
			buffer->Write(" ");
			Write(buffer, binary->op);
			buffer->Write(" ");
			Write(buffer, binary->right);
			buffer->Write(")");
		} break;

		case Ast::Expression::UNARY_REFERENCE_OF:
		case Ast::Expression::UNARY_ADDRESS_OF:
		case Ast::Expression::UNARY_MINUS:
		case Ast::Expression::UNARY_PLUS:
		case Ast::Expression::UNARY_BITWISE_NOT:
		case Ast::Expression::UNARY_NOT: {
			Ast::Expression_Unary* unary = (Ast::Expression_Unary*)expression;
			buffer->Write("(");
			Write(buffer, unary->op);
			buffer->Write(" ");
			Write(buffer, unary->subexpr);
			buffer->Write(")");
		} break;

		case Ast::Expression::SUBSCRIPT: {
			Ast::Expression_Subscript* subscript = (Ast::Expression_Subscript*)expression;
			Write(buffer, subscript->array);
			buffer->Write("[");
			Write(buffer, subscript->index);
			buffer->Write("]");
		} break;

		case Ast::Expression::DOT_CALL:
		case Ast::Expression::CALL: {
			Ast::Expression_Call* call = (Ast::Expression_Call*)expression;
			Write(buffer, call->function);
			if (call->params->kind != Ast::Expression::TUPLE) {
				buffer->Write("(");
				Write(buffer, call->params);
				buffer->Write(")");
			}
			else {
				Write(buffer, call->params);
			}
		} break;

		case Ast::Expression::TUPLE: {
			Ast::Expression_Tuple* tuple = (Ast::Expression_Tuple*)expression;
			buffer->Write("(");
			for (u32 i = 0; i < tuple->elements.length; i++) {
				if (i) buffer->Write(", ");
				Write(buffer, tuple->elements[i]);
			}
			buffer->Write(")");
		} break;

		case Ast::Expression::IF_ELSE: {
			Ast::Expression_Ternary* ternary = (Ast::Expression_Ternary*)expression;
			buffer->Write("(");
			Write(buffer, ternary->left);
			buffer->Write(" if ");
			Write(buffer, ternary->middle);
			buffer->Write(" else ");
			Write(buffer, ternary->right);
			buffer->Write(")");
		} break;

		case Ast::Expression::AS: {
			Ast::Expression_As* as = (Ast::Expression_As*)expression;

			buffer->Write("(");
			Write(buffer, as->expr);
			buffer->Write(" as ");
			Write(buffer, as->type);
			buffer->Write(")");
		} break;

		case Ast::Expression::IMPLICIT_CAST: {
			Ast::Expression_Implicit_Cast* cast = (Ast::Expression_Implicit_Cast*)expression;
			buffer->Write("(Implicit_Cast: ");
			Write(buffer, cast->type);
			buffer->Write(", ");
			Write(buffer, cast->subexpr);
			buffer->Write(")");
		} break;

		case Ast::Expression::LAMBDA: {
			buffer->Write("(LAMBDA)");
		} break;
	}
}


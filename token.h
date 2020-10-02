#pragma once

#include "int.h"
#include "memory.h"
#include "string.h"
#include "span.h"


enum Token_Kind
{
	TOKEN_EOF = 0,
	TOKEN_IDENTIFIER,
	TOKEN_INTEGER_LITERAL,
	TOKEN_FLOAT_LITERAL,
	TOKEN_STRING_LITERAL,
	TOKEN_IMPORT,
	TOKEN_STRUCT,
	TOKEN_ENUM,
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_THEN,
	TOKEN_WHERE,
	TOKEN_FOR,
	TOKEN_WHILE,
	TOKEN_RETURN,
	TOKEN_BREAK,
	TOKEN_CLAIM,
	TOKEN_DEFER,
	TOKEN_ALIAS,
	TOKEN_AS,
	TOKEN_IN,
	TOKEN_AND,
	TOKEN_OR,
	TOKEN_NOT,
	TOKEN_NULL,
	TOKEN_TRUE,
	TOKEN_FALSE,
	TOKEN_BOOL,
	TOKEN_INT,
	TOKEN_INT8,
	TOKEN_INT16,
	TOKEN_INT32,
	TOKEN_INT64,
	TOKEN_UINT,
	TOKEN_UINT8,
	TOKEN_UINT16,
	TOKEN_UINT32,
	TOKEN_UINT64,
	TOKEN_FLOAT16,
	TOKEN_FLOAT32,
	TOKEN_FLOAT64,
	TOKEN_BITWISE_OR,
	TOKEN_BITWISE_AND,
	TOKEN_BITWISE_XOR,
	TOKEN_BITWISE_NOT,
	TOKEN_MOD,
	TOKEN_LESS,
	TOKEN_LESS_OR_EQUAL,
	TOKEN_GREATER,
	TOKEN_GREATER_OR_EQUAL,
	TOKEN_LEFT_SHIFT,
	TOKEN_RIGHT_SHIFT,
	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_ASTERISK,
	TOKEN_DIVIDE,
	TOKEN_EXPONENT,
	TOKEN_AMPERSAND,
	TOKEN_BAR,
	TOKEN_EXCLAMATION_MARK,
	TOKEN_QUESTION_MARK,
	TOKEN_EQUAL,
	TOKEN_NOT_EQUAL,
	TOKEN_PLUS_EQUAL,
	TOKEN_MINUS_EQUAL,
	TOKEN_TIMES_EQUAL,
	TOKEN_DIVIDE_EQUAL,
	TOKEN_EXPONENTIAL_EQUAL,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_BRACE,
	TOKEN_CLOSE_BRACE,
	TOKEN_OPEN_BRACKET,
	TOKEN_CLOSE_BRACKET,
	TOKEN_DOT,
	TOKEN_DOT_DOT,
	TOKEN_COMMA,
	TOKEN_FAT_ARROW,
	TOKEN_ARROW,
	TOKEN_COLON,
	TOKEN_SEMICOLON,
};


static constexpr String ToString(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_EOF:               return "<Eof>";
		case TOKEN_IDENTIFIER:        return "<identifier>";
		case TOKEN_INTEGER_LITERAL:   return "<integer>";
		case TOKEN_FLOAT_LITERAL:     return "<float>";
		case TOKEN_STRING_LITERAL:    return "<string>";
		case TOKEN_IMPORT:            return "import";
		case TOKEN_STRUCT:            return "struct";
		case TOKEN_ENUM:              return "enum";
		case TOKEN_IF:                return "if";
		case TOKEN_ELSE:              return "else";
		case TOKEN_THEN:              return "then";
		case TOKEN_WHERE:             return "where";
		case TOKEN_FOR:               return "for";
		case TOKEN_WHILE:             return "while";
		case TOKEN_RETURN:            return "return";
		case TOKEN_BREAK:             return "break";
		case TOKEN_CLAIM:             return "claim";
		case TOKEN_DEFER:             return "defer";
		case TOKEN_ALIAS:             return "alias";
		case TOKEN_AS:                return "as";
		case TOKEN_IN:                return "in";
		case TOKEN_OR:                return "or";
		case TOKEN_AND:               return "and";
		case TOKEN_NOT:               return "not";
		case TOKEN_NULL:              return "null";
		case TOKEN_TRUE:              return "true";
		case TOKEN_FALSE:             return "false";
		case TOKEN_BOOL:              return "bool";
		case TOKEN_INT:               return "int";
		case TOKEN_INT8:              return "int8";
		case TOKEN_INT16:             return "int16";
		case TOKEN_INT32:             return "int32";
		case TOKEN_INT64:             return "int64";
		case TOKEN_UINT:              return "uint";
		case TOKEN_UINT8:             return "uint8";
		case TOKEN_UINT16:            return "uint16";
		case TOKEN_UINT32:            return "uint32";
		case TOKEN_UINT64:            return "uint64";
		case TOKEN_FLOAT16:           return "float16";
		case TOKEN_FLOAT32:           return "float32";
		case TOKEN_FLOAT64:           return "float64";
		case TOKEN_BITWISE_OR:        return "OR";
		case TOKEN_BITWISE_AND:       return "AND";
		case TOKEN_BITWISE_XOR:       return "XOR";
		case TOKEN_BITWISE_NOT:       return "NOT";
		case TOKEN_MOD:               return "MOD";
		case TOKEN_EQUAL:             return "=";
		case TOKEN_NOT_EQUAL:         return "!=";
		case TOKEN_LESS:              return "<";
		case TOKEN_LESS_OR_EQUAL:     return "<=";
		case TOKEN_GREATER:           return ">";
		case TOKEN_GREATER_OR_EQUAL:  return ">=";
		case TOKEN_LEFT_SHIFT:        return "<<";
		case TOKEN_RIGHT_SHIFT:       return ">>";
		case TOKEN_PLUS:              return "+";
		case TOKEN_MINUS:             return "-";
		case TOKEN_ASTERISK:          return "*";
		case TOKEN_DIVIDE:            return "/";
		case TOKEN_EXPONENT:          return "^";
		case TOKEN_AMPERSAND:         return "&";
		case TOKEN_BAR:               return "|";
		case TOKEN_EXCLAMATION_MARK:  return "!";
		case TOKEN_QUESTION_MARK:     return "?";
		case TOKEN_PLUS_EQUAL:        return "+=";
		case TOKEN_MINUS_EQUAL:       return "-=";
		case TOKEN_TIMES_EQUAL:       return "*=";
		case TOKEN_DIVIDE_EQUAL:      return "/=";
		case TOKEN_EXPONENTIAL_EQUAL: return "^=";
		case TOKEN_OPEN_PAREN:        return "(";
		case TOKEN_CLOSE_PAREN:       return ")";
		case TOKEN_OPEN_BRACE:        return "{";
		case TOKEN_CLOSE_BRACE:       return "}";
		case TOKEN_OPEN_BRACKET:      return "[";
		case TOKEN_CLOSE_BRACKET:     return "]";
		case TOKEN_DOT:               return ".";
		case TOKEN_DOT_DOT:           return "..";
		case TOKEN_COMMA:             return ",";
		case TOKEN_FAT_ARROW:         return "=>";
		case TOKEN_ARROW:             return "->";
		case TOKEN_COLON:             return ":";
		case TOKEN_SEMICOLON:         return ";";
	}
}


struct SourceLocation
{
	u32 line;
	u32 offset;
};

struct DynamicArray32_Value
{
	u32 pointer;
	u32 length;
};

struct DynamicArray64_Value
{
	u64 pointer;
	u64 length;
};

union Value
{
	bool value_bool;

	u8  value_uint8;
	u16 value_uint16;
	u32 value_uint32;
	u64 value_uint64;

	s8  value_int8;
	s16 value_int16;
	s32 value_int32;
	s64 value_int64;

	// @Todo: value_float16
	f32 value_float32;
	f64 value_float64;

	DynamicArray32_Value value_dynamic_array32;
	DynamicArray64_Value value_dynamic_array64;

	Value* pointer;

	char data[];
};

struct IntegerInfo
{
	bool is_unsigned;
	u16 explicit_bytes;
	u64 value;
};


struct FloatInfo
{
	u16 explicit_bytes;
	f64 value;
};


union TokenInfo
{
	Span<char> span;
	String string;
	IntegerInfo integer;
	FloatInfo floating_point;
	u64 next;
};


struct Token
{
	Token_Kind kind;
	TokenInfo info;
	SourceLocation location;
	u8 indent;
	bool newline;

	constexpr Token* GetClosure()
	{
		return this + info.next;
	}
};


void Write(struct OutputBuffer* buffer, SourceLocation location);
void Write(struct OutputBuffer* buffer, Token_Kind kind);
void Write(struct OutputBuffer* buffer, Token& token);
void Write(struct OutputBuffer* buffer, Token* token);



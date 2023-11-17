#pragma once

#include "string.h"
#include "span.h"

enum Token_Kind : uint8
{
	TOKEN_EOF = 0,
	TOKEN_IDENTIFIER_FORMAL,
	TOKEN_IDENTIFIER_CASUAL,
	TOKEN_IDENTIFIER_CONSTANT,
	TOKEN_LITERAL_INT,
	TOKEN_LITERAL_INT8,
	TOKEN_LITERAL_INT16,
	TOKEN_LITERAL_INT32,
	TOKEN_LITERAL_INT64,
	TOKEN_LITERAL_UINT,
	TOKEN_LITERAL_UINT8,
	TOKEN_LITERAL_UINT16,
	TOKEN_LITERAL_UINT32,
	TOKEN_LITERAL_UINT64,
	TOKEN_LITERAL_FLOAT,
	TOKEN_LITERAL_FLOAT16,
	TOKEN_LITERAL_FLOAT32,
	TOKEN_LITERAL_FLOAT64,
	TOKEN_LITERAL_STRING,
	TOKEN_IMPORT,
	TOKEN_STRUCT,
	TOKEN_ENUM,
	TOKEN_ASM,
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_THEN,
	TOKEN_INC,
	TOKEN_DEC,
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
	TOKEN_BYTE,
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
	TOKEN_QUESTION_MARK_DOT,
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
		case TOKEN_EOF:                 return "<eof>";
		case TOKEN_IDENTIFIER_FORMAL:   return "<formal_identifier>";
		case TOKEN_IDENTIFIER_CASUAL:   return "<casual_identifier>";
		case TOKEN_IDENTIFIER_CONSTANT: return "<constant_identifier>";
		case TOKEN_LITERAL_INT:         return "<literal_int>";
		case TOKEN_LITERAL_INT8:        return "<literal_int8>";
		case TOKEN_LITERAL_INT16:       return "<literal_int16>";
		case TOKEN_LITERAL_INT32:       return "<literal_int32>";
		case TOKEN_LITERAL_INT64:       return "<literal_int64>";
		case TOKEN_LITERAL_UINT:        return "<literal_uint>";
		case TOKEN_LITERAL_UINT8:       return "<literal_uint8>";
		case TOKEN_LITERAL_UINT16:      return "<literal_uint16>";
		case TOKEN_LITERAL_UINT32:      return "<literal_uint32>";
		case TOKEN_LITERAL_UINT64:      return "<literal_uint64>";
		case TOKEN_LITERAL_FLOAT:       return "<literal_float>";
		case TOKEN_LITERAL_FLOAT16:     return "<literal_float16>";
		case TOKEN_LITERAL_FLOAT32:     return "<literal_float32>";
		case TOKEN_LITERAL_FLOAT64:     return "<literal_float64>";
		case TOKEN_LITERAL_STRING:      return "<literal_string>";
		case TOKEN_IMPORT:              return "import";
		case TOKEN_STRUCT:              return "struct";
		case TOKEN_ENUM:                return "enum";
		case TOKEN_ASM:                 return "asm";
		case TOKEN_IF:                  return "if";
		case TOKEN_ELSE:                return "else";
		case TOKEN_THEN:                return "then";
		case TOKEN_INC:                 return "inc";
		case TOKEN_DEC:                 return "dec";
		case TOKEN_WHERE:               return "where";
		case TOKEN_FOR:                 return "for";
		case TOKEN_WHILE:               return "while";
		case TOKEN_RETURN:              return "return";
		case TOKEN_BREAK:               return "break";
		case TOKEN_CLAIM:               return "claim";
		case TOKEN_DEFER:               return "defer";
		case TOKEN_ALIAS:               return "alias";
		case TOKEN_AS:                  return "as";
		case TOKEN_IN:                  return "in";
		case TOKEN_OR:                  return "or";
		case TOKEN_AND:                 return "and";
		case TOKEN_NOT:                 return "not";
		case TOKEN_NULL:                return "null";
		case TOKEN_TRUE:                return "true";
		case TOKEN_FALSE:               return "false";
		case TOKEN_BYTE:                return "byte";
		case TOKEN_BOOL:                return "bool";
		case TOKEN_INT:                 return "int";
		case TOKEN_INT8:                return "int8";
		case TOKEN_INT16:               return "int16";
		case TOKEN_INT32:               return "int32";
		case TOKEN_INT64:               return "int64";
		case TOKEN_UINT:                return "uint";
		case TOKEN_UINT8:               return "uint8";
		case TOKEN_UINT16:              return "uint16";
		case TOKEN_UINT32:              return "uint32";
		case TOKEN_UINT64:              return "uint64";
		case TOKEN_FLOAT16:             return "float16";
		case TOKEN_FLOAT32:             return "float32";
		case TOKEN_FLOAT64:             return "float64";
		case TOKEN_BITWISE_OR:          return "OR";
		case TOKEN_BITWISE_AND:         return "AND";
		case TOKEN_BITWISE_XOR:         return "XOR";
		case TOKEN_BITWISE_NOT:         return "NOT";
		case TOKEN_MOD:                 return "MOD";
		case TOKEN_EQUAL:               return "=";
		case TOKEN_NOT_EQUAL:           return "!=";
		case TOKEN_LESS:                return "<";
		case TOKEN_LESS_OR_EQUAL:       return "<=";
		case TOKEN_GREATER:             return ">";
		case TOKEN_GREATER_OR_EQUAL:    return ">=";
		case TOKEN_LEFT_SHIFT:          return "<<";
		case TOKEN_RIGHT_SHIFT:         return ">>";
		case TOKEN_PLUS:                return "+";
		case TOKEN_MINUS:               return "-";
		case TOKEN_ASTERISK:            return "*";
		case TOKEN_DIVIDE:              return "/";
		case TOKEN_EXPONENT:            return "^";
		case TOKEN_AMPERSAND:           return "&";
		case TOKEN_BAR:                 return "|";
		case TOKEN_EXCLAMATION_MARK:    return "!";
		case TOKEN_QUESTION_MARK:       return "?";
		case TOKEN_QUESTION_MARK_DOT:   return "?.";
		case TOKEN_PLUS_EQUAL:          return "+=";
		case TOKEN_MINUS_EQUAL:         return "-=";
		case TOKEN_TIMES_EQUAL:         return "*=";
		case TOKEN_DIVIDE_EQUAL:        return "/=";
		case TOKEN_EXPONENTIAL_EQUAL:   return "^=";
		case TOKEN_OPEN_PAREN:          return "(";
		case TOKEN_CLOSE_PAREN:         return ")";
		case TOKEN_OPEN_BRACE:          return "{";
		case TOKEN_CLOSE_BRACE:         return "}";
		case TOKEN_OPEN_BRACKET:        return "[";
		case TOKEN_CLOSE_BRACKET:       return "]";
		case TOKEN_DOT:                 return ".";
		case TOKEN_DOT_DOT:             return "..";
		case TOKEN_COMMA:               return ",";
		case TOKEN_FAT_ARROW:           return "=>";
		case TOKEN_ARROW:               return "->";
		case TOKEN_COLON:               return ":";
		case TOKEN_SEMICOLON:           return ";";
	}
}

struct SourceLocation
{
	// Signed because of error printing. 64-bit is overkill anyways.
	int64 line;
	int64 offset;
	int64 extent;
};

// @Todo: SOA! (Make sure to measure!)

struct Token
{
	Token_Kind kind;
	bool newline;
	uint16 indent;

	union
	{
		int64   next;
		String  identifier_string;
		String  literal_string;
		int64   literal_int;
		float64 literal_float;
	};

	SourceLocation location;
};

static inline Token* GetClosure(Token* token)
{
	return token + token->next;
}

static void GenericWrite(OutputBuffer* buffer, Token_Kind kind);
static void GenericWrite(OutputBuffer* buffer, Token* token);
static void GenericWrite(OutputBuffer* buffer, Token token);


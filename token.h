#pragma once

#include "ascii.h"
#include "string.h"

enum TokenKind : u8 {
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
	TOKEN_FLOAT32,
	TOKEN_FLOAT64,
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
	TOKEN_CARET,
	TOKEN_AMPERSAND,
	TOKEN_BAR,
	TOKEN_TILDE,
	TOKEN_EXCLAMATION_MARK,
	TOKEN_QUESTION_MARK,
	TOKEN_QUESTION_MARK_DOT,
	TOKEN_EQUAL,
	TOKEN_NOT_EQUAL,
	TOKEN_PLUS_EQUAL,
	TOKEN_MINUS_EQUAL,
	TOKEN_TIMES_EQUAL,
	TOKEN_DIVIDE_EQUAL,
	TOKEN_CARET_EQUAL,
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

static constexpr String ToString(TokenKind kind) {
	switch (kind) {
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
		case TOKEN_OR:                  return "||";
		case TOKEN_AND:                 return "&&";
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
		case TOKEN_FLOAT32:             return "float32";
		case TOKEN_FLOAT64:             return "float64";
		case TOKEN_MOD:                 return "%";
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
		case TOKEN_CARET:               return "^";
		case TOKEN_AMPERSAND:           return "&";
		case TOKEN_BAR:                 return "|";
		case TOKEN_TILDE:               return "~";
		case TOKEN_EXCLAMATION_MARK:    return "!";
		case TOKEN_QUESTION_MARK:       return "?";
		case TOKEN_QUESTION_MARK_DOT:   return "?.";
		case TOKEN_PLUS_EQUAL:          return "+=";
		case TOKEN_MINUS_EQUAL:         return "-=";
		case TOKEN_TIMES_EQUAL:         return "*=";
		case TOKEN_DIVIDE_EQUAL:        return "/=";
		case TOKEN_CARET_EQUAL:         return "^=";
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

typedef u16 Indent16;

struct SourceLocation {
	// Signed because of error printing. 64-bit is overkill anyways.
	s64 line;
	s64 offset;
	s64 extent;
};

using TokenFlags = u8;
static const TokenFlags TOKEN_FLAG_NEWLINE           = 0x01;
static const TokenFlags TOKEN_FLAG_LEFT_SPACED       = 0x02;
static const TokenFlags TOKEN_FLAG_RIGHT_SPACED      = 0x04;
static const TokenFlags TOKEN_FLAG_KEYWORD_PRIMITIVE = 0x08;

struct Token {
	TokenKind  kind;
	TokenFlags flags;
	Indent16   indent;

	bool IsNewLine()     { return flags & TOKEN_FLAG_NEWLINE;      }
	bool IsLeftSpaced()  { return flags & TOKEN_FLAG_LEFT_SPACED;  }
	bool IsRightSpaced() { return flags & TOKEN_FLAG_RIGHT_SPACED; }

	union {
		struct {
			Token* closure; // Used as linked list during lexing
			u64 comma_count;
		};

		String  identifier_string;
		String  literal_string;
		s64     literal_int;
		float64 literal_float;
	};

	SourceLocation location;
};

static void Write(struct OutputBuffer* buffer, TokenKind kind);
static void Write(struct OutputBuffer* buffer, Token* token);
static void Write(struct OutputBuffer* buffer, Token token);


#pragma once

#include "int.h"
#include "memory.h"
#include "string.h"
#include "span.h"


enum Token_Kind
{
	Token_Eof = 0,
	Token_Identifier,
	Token_IntegerLiteral,
	Token_FloatLiteral,
	Token_StringLiteral,
	Token_Import,
	Token_Struct,
	Token_Enum,
	Token_If,
	Token_Else,
	Token_Then,
	Token_Where,
	Token_For,
	Token_While,
	Token_Return,
	Token_Break,
	Token_Claim,
	Token_Defer,
	Token_Alias,
	Token_As,
	Token_In,
	Token_And,
	Token_Or,
	Token_Null,
	Token_True,
	Token_False,
	Token_Bool,
	Token_Int,
	Token_Int8,
	Token_Int16,
	Token_Int32,
	Token_Int64,
	Token_Uint,
	Token_Uint8,
	Token_Uint16,
	Token_Uint32,
	Token_Uint64,
	Token_Float16,
	Token_Float32,
	Token_Float64,
	Token_OR,
	Token_AND,
	Token_XOR,
	Token_NOT,
	Token_Less,
	Token_LessOrEqual,
	Token_Greater,
	Token_GreaterOrEqual,
	Token_LeftShift,
	Token_RightShift,
	Token_Plus,
	Token_Minus,
	Token_Asterisk,
	Token_Divide,
	Token_Exponent,
	Token_Ampersand,
	Token_Bar,
	Token_WeakAnd = Token_Ampersand,
	Token_WeakOr = Token_Bar,
	Token_StrongAnd,
	Token_StrongOr,
	Token_ExclamationMark,
	Token_QuestionMark,
	Token_Equal,
	Token_NotEqual,
	Token_PlusEqual,
	Token_MinusEqual,
	Token_TimesEqual,
	Token_DivideEqual,
	Token_ExponentialEqual,
	Token_OpenParen,
	Token_CloseParen,
	Token_OpenBrace,
	Token_CloseBrace,
	Token_OpenBracket,
	Token_CloseBracket,
	Token_Dot,
	Token_DotDot,
	Token_Comma,
	Token_FatArrow,
	Token_Arrow,
	Token_Colon,
	Token_SemiColon,
};


static constexpr String ToString(Token_Kind kind)
{
	switch (kind)
	{
		case Token_Eof:              return "<Eof>";
		case Token_Identifier:       return "<identifier>";
		case Token_IntegerLiteral:   return "<integer>";
		case Token_FloatLiteral:     return "<float>";
		case Token_StringLiteral:    return "<string>";
		case Token_Import:           return "import";
		case Token_Struct:           return "struct";
		case Token_Enum:             return "enum";
		case Token_If:               return "if";
		case Token_Else:             return "else";
		case Token_Then:             return "then";
		case Token_Where:            return "where";
		case Token_For:              return "for";
		case Token_While:            return "while";
		case Token_Return:           return "return";
		case Token_Break:            return "break";
		case Token_Claim:            return "claim";
		case Token_Defer:            return "defer";
		case Token_Alias:            return "alias";
		case Token_As:               return "as";
		case Token_In:               return "in";
		case Token_Or:               return "or";
		case Token_And:              return "and";
		case Token_Null:             return "null";
		case Token_True:             return "true";
		case Token_False:            return "false";
		case Token_Bool:             return "bool";
		case Token_Int:              return "int";
		case Token_Int8:             return "int8";
		case Token_Int16:            return "int16";
		case Token_Int32:            return "int32";
		case Token_Int64:            return "int64";
		case Token_Uint:             return "uint";
		case Token_Uint8:            return "uint8";
		case Token_Uint16:           return "uint16";
		case Token_Uint32:           return "uint32";
		case Token_Uint64:           return "uint64";
		case Token_Float16:          return "float16";
		case Token_Float32:          return "float32";
		case Token_Float64:          return "float64";
		case Token_OR:               return "OR";
		case Token_AND:              return "AND";
		case Token_XOR:              return "XOR";
		case Token_NOT:              return "NOT";
		case Token_Equal:            return "=";
		case Token_NotEqual:         return "!=";
		case Token_Less:             return "<";
		case Token_LessOrEqual:      return "<=";
		case Token_Greater:          return ">";
		case Token_GreaterOrEqual:   return ">=";
		case Token_LeftShift:        return "<<";
		case Token_RightShift:       return ">>";
		case Token_Plus:             return "+";
		case Token_Minus:            return "-";
		case Token_Asterisk:         return "*";
		case Token_Divide:           return "/";
		case Token_Exponent:         return "^";
		case Token_Ampersand:        return "&";
		case Token_Bar:              return "|";
		case Token_StrongAnd:        return "&&";
		case Token_StrongOr:         return "||";
		case Token_ExclamationMark:  return "!";
		case Token_QuestionMark:     return "?";
		case Token_PlusEqual:        return "+=";
		case Token_MinusEqual:       return "-=";
		case Token_TimesEqual:       return "*=";
		case Token_DivideEqual:      return "/=";
		case Token_ExponentialEqual: return "^=";
		case Token_OpenParen:        return "(";
		case Token_CloseParen:       return ")";
		case Token_OpenBrace:        return "{";
		case Token_CloseBrace:       return "}";
		case Token_OpenBracket:      return "[";
		case Token_CloseBracket:     return "]";
		case Token_Dot:              return ".";
		case Token_DotDot:           return "..";
		case Token_Comma:            return ",";
		case Token_FatArrow:         return "=>";
		case Token_Arrow:            return "->";
		case Token_Colon:            return ":";
		case Token_SemiColon:        return ";";
	}
}


struct SourceLocation
{
	u32 line;
	u32 offset;
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



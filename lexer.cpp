#include "general.h"
#include "error.h"
#include "parser.h"
#include "list.h"
#include "memory.h"
#include "file_system.h"
#include "print.h"
#include "string.h"
#include "ascii.h"

using LiteralQualifier = uint64;

static const LiteralQualifier QUALIFIER_B = (1llu<<0);
static const LiteralQualifier QUALIFIER_H = (1llu<<1);
static const LiteralQualifier QUALIFIER_BASE_MASK = QUALIFIER_B | QUALIFIER_H;

static const LiteralQualifier QUALIFIER_F = (1llu<<2);

static const LiteralQualifier QUALIFIER_K = (1llu<<10);
static const LiteralQualifier QUALIFIER_M = (1llu<<20);
static const LiteralQualifier QUALIFIER_G = (1llu<<30);
static const LiteralQualifier QUALIFIER_T = (1llu<<40);
static const LiteralQualifier QUALIFIER_P = (1llu<<50);
static const LiteralQualifier QUALIFIER_E = (1llu<<60);
static const LiteralQualifier QUALIFIER_SCALER_MASK = QUALIFIER_K | QUALIFIER_M | QUALIFIER_G | QUALIFIER_T | QUALIFIER_P | QUALIFIER_E;

static const LiteralQualifier QUALIFIER_8  = (1llu<<3);
static const LiteralQualifier QUALIFIER_16 = (1llu<<4);
static const LiteralQualifier QUALIFIER_32 = (1llu<<5);
static const LiteralQualifier QUALIFIER_64 = (1llu<<6);
static const LiteralQualifier QUALIFIER_JUNK_BIT = (1llu<<7);
static const LiteralQualifier QUALIFIER_SIZE_MASK = QUALIFIER_8 | QUALIFIER_16 | QUALIFIER_32 | QUALIFIER_64;

static const LiteralQualifier QUALIFIER_U = (1llu<<8);
static const LiteralQualifier QUALIFIER_S = (1llu<<9);
static const LiteralQualifier QUALIFIER_SIGN_MASK = QUALIFIER_U | QUALIFIER_S;

static const LiteralQualifier QUALIFIER_CERTAIN_MASK = QUALIFIER_H | QUALIFIER_U | QUALIFIER_S | QUALIFIER_K | QUALIFIER_M | QUALIFIER_G | QUALIFIER_T | QUALIFIER_P;
static const LiteralQualifier QUALIFIER_INTEGER_MASK = QUALIFIER_SIGN_MASK | QUALIFIER_SCALER_MASK;

static inline Base GetBaseFromQualifier(LiteralQualifier qualifier)
{
	switch (qualifier & QUALIFIER_BASE_MASK)
	{
		case 0: return BASE_DECIMAL;
		case 1: return BASE_BINARY;
		case 2: return BASE_HEX;
		default: AssertUnreachable();
	}
}

static LiteralQualifier ParseLiteralQualifier(char** begin)
{
	LiteralQualifier qualifier = 0;
	char* p = *begin;

	if      (*p == 'h') { qualifier |= QUALIFIER_H; p++; }
	else if (*p == 'b') { qualifier |= QUALIFIER_B; p++; }

	if (*p == 'f')
	{
		qualifier |= QUALIFIER_F;
		p++;
	}
	else
	{
		switch (*p)
		{
			default: break;
			case 'k': qualifier |= QUALIFIER_K; p++; break;
			case 'm': qualifier |= QUALIFIER_M; p++; break;
			case 'g': qualifier |= QUALIFIER_G; p++; break;
			case 't': qualifier |= QUALIFIER_T; p++; break;
			case 'p': qualifier |= QUALIFIER_P; p++; break;
			case 'e': qualifier |= QUALIFIER_E; p++; break;
		}

		if      (*p == 'u') { qualifier |= QUALIFIER_U; p++; }
		else if (*p == 's') { qualifier |= QUALIFIER_S; p++; }
	}

	if (qualifier)
	{
		if      (CompareStringRaw(p, "64")) { qualifier |= QUALIFIER_64 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "32")) { qualifier |= QUALIFIER_32 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "16")) { qualifier |= QUALIFIER_16 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "8"))  { qualifier |= QUALIFIER_8;  p += 1; }
	}

	if ((qualifier & QUALIFIER_CERTAIN_MASK) == 0 && (IsHexLut(*p) || *p == '_' || *p == 'h'))
	{
		return 0;
	}

	*begin = p;
	return qualifier;
}

static float64 AsciiToFloat(char* p, uint64 count, Base base)
{
	float64 value = 0.0;

	for (uint32 i = 0; i < count; i++)
	{
		Assert(IsDigit(*p, base));
		value *= (float64)base;
		value += (float64)DecodeDigitLut(*p);
		if (*++p == '_') ++p;
	}

	return value;
}

static float64 AsciiToFloat_Fractional(char* p, uint64 count, Base base)
{
	float64 value = 0.0;
	float64 scale = 1.0;

	for (uint32 i = 0; i < count; i++)
	{
		// @OptimizeMe: Nasty dependency chain. Precompute base^-count?
		Assert(IsDigit(*p, base));
		scale *= 1.0/(float64)base;
		value += (float64)DecodeDigitLut(*p) * scale;
		if (*++p == '_') ++p;
	}

	return value;
}

static uint64 AsciiToInt(char* begin, uint64 count, Base base, bool* overflow)
{
	uint64 value = 0;

	if (base == BASE_BINARY)
	{
		char* p = begin;

		if (count > 64) COLD
		{
			*overflow = true;
			return value;
		}

		for (uint32 i = 0; i < count; i++)
		{
			Assert(IsDigit(*p, BASE_BINARY));
			value |= (uint64)DecodeDigit(*p, BASE_BINARY) << (uint64)(count-1-i);
			if (*++p == '_') ++p;
		}
	}
	else if (base == BASE_DECIMAL) HOT
	{
		if (count > 20) COLD
		{
			*overflow = true;
			return value;
		}
		else if (count == 20) COLD
		{
			char* p = begin;
			for (uint32 i = 0; i < count; i++)
			{
				if (*p > "18446744073709551615"[i])
				{
					*overflow = true;
					return value;
				}

				if (*++p == '_') ++p;
			}
		}

		const uint64 factors[20] = {
			10000000000000000000llu,
			1000000000000000000,
			100000000000000000,
			10000000000000000,
			1000000000000000,
			100000000000000,
			10000000000000,
			1000000000000,
			100000000000,
			10000000000,
			1000000000,
			100000000,
			10000000,
			1000000,
			100000,
			10000,
			1000,
			100,
			10,
			1,
		};

		uint64 base_factor_index = 20-count;
		char* p = begin;

		for (uint32 i = 0; i < count; i++)
		{
			Assert(IsDigit(*p, BASE_DECIMAL));
			value += factors[base_factor_index+i] * (uint64)DecodeDigit(*p, BASE_DECIMAL);
			if (*++p == '_') ++p;
		}
	}
	else if (base == BASE_HEX)
	{
		char* p = begin;
		if (count > 16) COLD
		{
			*overflow = true;
			return value;
		}

		for (uint32 i = 0; i < count; i++)
		{
			value |= (uint64)DecodeDigitLut(*p) << (uint64)(count-1-i)*4;
			if (*++p == '_') ++p;
		}
	}

	return value;
}

struct LiteralComponent
{
	char* begin;
	uint32 count;
	Base base;
	LiteralQualifier qualifier;
	char* end;
};

static LiteralComponent ParseLiteralComponent(char* begin)
{
	LiteralComponent component;
	component.count = 0;
	component.begin = begin;
	component.base = BASE_BINARY;

	char* p = begin;

	while (IsBinary(*p))  { component.base = BASE_BINARY;  component.count++; if (*++p == '_') p++; }
	while (IsDecimal(*p)) { component.base = BASE_DECIMAL; component.count++; if (*++p == '_') p++; }

	component.qualifier = ParseLiteralQualifier(&p);

	if (!component.qualifier && IsHex(*p))
	{
		while (IsHexLut(*p)) { component.count++; if (*++p == '_') p++; }
		component.base = BASE_HEX;
		component.qualifier = ParseLiteralQualifier(&p);
	}

	component.end = p;
	return component;
}

static void ParseLiteral(Ast_Module* module, char** master_cursor, Token* token)
{
	LiteralComponent whole;
	LiteralComponent fractional;

	char* p = *master_cursor;

	if (*p == '0')
	{
		while (*p == '0') if (*++p == '_') p++;
	}

	whole = ParseLiteralComponent(p);
	bool has_fractional = false;
	LiteralQualifier qualifier = whole.qualifier;
	Base base = whole.base;

	if (whole.end[0] == '.' && (whole.qualifier & QUALIFIER_CERTAIN_MASK) == 0)
	{
		fractional = ParseLiteralComponent(whole.end+1);

		if (IsDigit(whole.end[1], BASE_DECIMAL) ||
			!qualifier && fractional.qualifier && !IsAlpha(fractional.end[0]) ||
			qualifier && (fractional.qualifier & QUALIFIER_H) && !IsAlpha(fractional.end[0]))
		{
			qualifier = fractional.qualifier;
			base = (Base)Max(base, fractional.base);
			whole.count += CountBits64(whole.qualifier);
			has_fractional = true;
		}
	}

	p = has_fractional ? fractional.end : whole.end;

	Base qbase = GetBaseFromQualifier(qualifier);

	if (base == BASE_HEX && (qualifier & QUALIFIER_BASE_MASK) == 0)
	{
		LexerError(module, token->location, "Hexadecimal missing 'h' qualifier.\n");
	}
	else if ((qualifier & QUALIFIER_BASE_MASK) && qbase < base)
	{
		LexerError(module, token->location, "% literal with % digits.\n", ToString(qbase), ToString(base));
	}

	base = qbase;

	if ((qualifier & QUALIFIER_F) || has_fractional && (qualifier & QUALIFIER_INTEGER_MASK) == 0)
	{
		switch (qualifier & QUALIFIER_SIZE_MASK)
		{
			default:           token->kind = TOKEN_LITERAL_FLOAT;   break;
			case QUALIFIER_16: token->kind = TOKEN_LITERAL_FLOAT16; break;
			case QUALIFIER_32: token->kind = TOKEN_LITERAL_FLOAT32; break;
			case QUALIFIER_64: token->kind = TOKEN_LITERAL_FLOAT64; break;
			case QUALIFIER_8:
			{
				LexerError(module, token->location, "Invalid float literal qualifier: float8 is not a valid type.\n");
			}
		}

		float64 value = AsciiToFloat(whole.begin, whole.count, base);

		if (has_fractional)
		{
			float64 fractional_value = AsciiToFloat_Fractional(fractional.begin, fractional.count, base);
			value += fractional_value;
		}

		token->literal_float = value;
	}
	else
	{
		switch (qualifier & (QUALIFIER_SIZE_MASK | QUALIFIER_U))
		{
			default:           token->kind = TOKEN_LITERAL_INT;   break;
			case QUALIFIER_8:  token->kind = TOKEN_LITERAL_INT8;  break;
			case QUALIFIER_16: token->kind = TOKEN_LITERAL_INT16; break;
			case QUALIFIER_32: token->kind = TOKEN_LITERAL_INT32; break;
			case QUALIFIER_64: token->kind = TOKEN_LITERAL_INT64; break;

			case QUALIFIER_U:                token->kind = TOKEN_LITERAL_UINT;   break;
			case QUALIFIER_8  | QUALIFIER_U: token->kind = TOKEN_LITERAL_UINT8;  break;
			case QUALIFIER_16 | QUALIFIER_U: token->kind = TOKEN_LITERAL_UINT16; break;
			case QUALIFIER_32 | QUALIFIER_U: token->kind = TOKEN_LITERAL_UINT32; break;
			case QUALIFIER_64 | QUALIFIER_U: token->kind = TOKEN_LITERAL_UINT64; break;
		}

		bool overflow = false;
		uint64 scaler = (qualifier & QUALIFIER_SCALER_MASK);
		uint64 size = qualifier & QUALIFIER_SIZE_MASK;
		uint64 effective_size = size ? size : 64;
		uint64 value = AsciiToInt(whole.begin, whole.count, base, &overflow);
		uint64 fractional_scaled = 0;

		if (overflow)
		{
			LexerError(module, token->location, "Integer literal exceeds %-bits.\n", effective_size);
		}

		if (has_fractional)
		{
			if (!scaler)
			{
				LexerError(module, token->location, "Integer missing scaler qualifier.\n");
			}

			float64 fractional_value = AsciiToFloat_Fractional(fractional.begin, fractional.count, base);
			fractional_scaled = (uint64)(fractional_value * (float64)scaler);
		}

		if (scaler)
		{
			uint64 scaled_value = 0;

			if (CheckedMultiply(value, scaler, &scaled_value) || CheckedAdd(scaled_value, fractional_scaled, &scaled_value))
			{
				LexerError(module, token->location, "Integer literal with scaler exceeds %-bits.\n", effective_size);
			}

			value = scaled_value;
		}

		if (BitsOfInformation64(value) > effective_size)
		{
			LexerError(module, token->location, "Integer literal exceeds %-bits.\n", effective_size);
		}

		token->literal_int = value;
	}

	*master_cursor = p;

	if (IsAlpha(*p))
	{
		LexerError(module, token->location, "Unexpected character after literal: '%'.\n", *p);
	}
}

static bool CheckIfHexadecimal(char* p)
{
	while (IsHexLut(*p) || *p == '_') p++;

	if (*p == '.')
	{
		p += 1;
		while (IsHexLut(*p) || *p == '_') p++;
	}

	return *p == 'h' && ParseLiteralQualifier(&p);
}

// -------------------------------------------- //

// @Todo: Hash identifiers
// @Todo: Hash strings
// @Todo: Hash Literals
// @Todo: Error continuation

static inline bool IsKeyword(char* p, Token_Kind kind)
{
	char post = p[ToString(kind).length];

	if (!CompareMemory(p, ToString(kind).data, ToString(kind).length))
		return false;

	if (IsAlpha(post))
		return false;

	if (IsDigit(post, BASE_DECIMAL))
		return false;

	if (post == '_')
		return false;

	return true;
}

static Token_Kind GetMatchingParen(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_OPEN_PAREN:    return TOKEN_OPEN_PAREN;
		case TOKEN_OPEN_BRACE:    return TOKEN_OPEN_BRACE;
		case TOKEN_OPEN_BRACKET:  return TOKEN_OPEN_BRACKET;
		case TOKEN_CLOSE_PAREN:   return TOKEN_CLOSE_PAREN;
		case TOKEN_CLOSE_BRACE:   return TOKEN_CLOSE_BRACE;
		case TOKEN_CLOSE_BRACKET: return TOKEN_CLOSE_BRACKET;
		default: AssertUnreachable();
	}
}

static void LexerParse(Ast_Module* module)
{
	Array<char> data = FileLoad(module->file_path, 16, 64);
	String code = String(data.elements, data.count);

	Token* tokens = (Token*)AllocateMemory(sizeof(Token) * (data.count+1));
	List<Line> lines = AllocateList<Line>(Min(code.length, 4096u));

	int64 line_number = 0;
	int64 indent = 0;
	int64 prev_region_index = -1;

	char* cursor = code.data;
	char* end = code.data + code.length;
	char* line_begin = cursor;

	Token* tokens_begin = tokens;
	Token* token = tokens_begin;
	Token* line_begin_token = token;

	Token* open_token = null;

	while (cursor < end)
	{
		uint64 old_line = line_number;

		while (IsWhiteSpace(*cursor) || *cursor == '\n' || *cursor == '\r' || CompareStringRaw(cursor, "//"))
		{
			if (*cursor == '\t' && cursor == line_begin)
			{
				while (*++cursor == '\t');
				indent = cursor - line_begin;
			}
			else if (CompareStringRaw(cursor, "//"))
			{
				for (cursor += 2; cursor < end && *cursor != '\n'; cursor++);
			}
			else if (*cursor == '\n')
			{
				Line line;
				ZeroMemory(&line);

				line.indent = indent;
				line.string = String(line_begin, cursor-line_begin);
				line.tokens_begin = line_begin_token;
				line.tokens_end   = token;
				lines.Add(line);

				line_begin_token = token;
				line_number++;
				line_begin = cursor+1;
				cursor++;
				indent = 0;
			}
			else cursor++;
		}

		bool newline = line_number != old_line || token == tokens_begin;
		char* token_begin = cursor;

		ZeroMemory(token);
		token->indent = indent;
		token->location.line = line_number;
		token->location.offset = cursor - line_begin;
		token->newline = newline;

		if (cursor >= end) break;

		switch (*cursor)
		{
			case 'A':
				if (IsKeyword(cursor, TOKEN_BITWISE_AND)) { cursor += ToString(TOKEN_BITWISE_AND).length; token->kind = TOKEN_BITWISE_AND; break; }

			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			{
				if (!CheckIfHexadecimal(cursor))
					goto PARSE_IDENTIFIER;

				ParseLiteral(module, &cursor, token);
			} break;

			case 'M':
				if (IsKeyword(cursor, TOKEN_MOD))         { cursor += ToString(TOKEN_MOD).length;         token->kind = TOKEN_MOD;         break; }
				goto PARSE_IDENTIFIER;

			case 'N':
				if (IsKeyword(cursor, TOKEN_BITWISE_NOT)) { cursor += ToString(TOKEN_BITWISE_NOT).length; token->kind = TOKEN_BITWISE_NOT; break; }
				goto PARSE_IDENTIFIER;

			case 'O':
				if (IsKeyword(cursor, TOKEN_BITWISE_OR))  { cursor += ToString(TOKEN_BITWISE_OR).length;  token->kind = TOKEN_BITWISE_OR;  break; }
				goto PARSE_IDENTIFIER;

			case 'X':
				if (IsKeyword(cursor, TOKEN_BITWISE_XOR)) { cursor += ToString(TOKEN_BITWISE_XOR).length; token->kind = TOKEN_BITWISE_XOR; break; }
				goto PARSE_IDENTIFIER;

			case 'a':
				if (IsKeyword(cursor, TOKEN_ALIAS))       { cursor += ToString(TOKEN_ALIAS).length;       token->kind = TOKEN_ALIAS;       break; }
				if (IsKeyword(cursor, TOKEN_AND))         { cursor += ToString(TOKEN_AND).length;         token->kind = TOKEN_AND;         break; }
				if (IsKeyword(cursor, TOKEN_AS))          { cursor += ToString(TOKEN_AS).length;          token->kind = TOKEN_AS;          break; }
				if (IsKeyword(cursor, TOKEN_ASM))         { cursor += ToString(TOKEN_ASM).length;         token->kind = TOKEN_ASM;         break; }
				goto PARSE_IDENTIFIER;

			case 'b':
				if (IsKeyword(cursor, TOKEN_BOOL))        { cursor += ToString(TOKEN_BOOL).length;        token->kind = TOKEN_BOOL;        break; }
				if (IsKeyword(cursor, TOKEN_BREAK))       { cursor += ToString(TOKEN_BREAK).length;       token->kind = TOKEN_BREAK;       break; }
				if (IsKeyword(cursor, TOKEN_BYTE))        { cursor += ToString(TOKEN_BYTE).length;        token->kind = TOKEN_BYTE;        break; }
				goto PARSE_IDENTIFIER;

			case 'c':
				if (IsKeyword(cursor, TOKEN_CLAIM))       { cursor += ToString(TOKEN_CLAIM).length;       token->kind = TOKEN_CLAIM;       break; }
				goto PARSE_IDENTIFIER;

			case 'd':
				if (IsKeyword(cursor, TOKEN_DEC))         { cursor += ToString(TOKEN_DEC).length;         token->kind = TOKEN_DEC;         break; }
				if (IsKeyword(cursor, TOKEN_DEFER))       { cursor += ToString(TOKEN_DEFER).length;       token->kind = TOKEN_DEFER;       break; }
				goto PARSE_IDENTIFIER;

			case 'e':
				if (IsKeyword(cursor, TOKEN_ELSE))        { cursor += ToString(TOKEN_ELSE).length;        token->kind = TOKEN_ELSE;        break; }
				if (IsKeyword(cursor, TOKEN_ENUM))        { cursor += ToString(TOKEN_ENUM).length;        token->kind = TOKEN_ENUM;        break; }
				goto PARSE_IDENTIFIER;

			case 'f':
				if (IsKeyword(cursor, TOKEN_FALSE))       { cursor += ToString(TOKEN_FALSE).length;       token->kind = TOKEN_FALSE;       break; }
				if (IsKeyword(cursor, TOKEN_FLOAT16))     { cursor += ToString(TOKEN_FLOAT16).length;     token->kind = TOKEN_FLOAT16;     break; }
				if (IsKeyword(cursor, TOKEN_FLOAT32))     { cursor += ToString(TOKEN_FLOAT32).length;     token->kind = TOKEN_FLOAT32;     break; }
				if (IsKeyword(cursor, TOKEN_FLOAT64))     { cursor += ToString(TOKEN_FLOAT64).length;     token->kind = TOKEN_FLOAT64;     break; }
				if (IsKeyword(cursor, TOKEN_FOR))         { cursor += ToString(TOKEN_FOR).length;         token->kind = TOKEN_FOR;         break; }
				goto PARSE_IDENTIFIER;

			case 'i':
				if (IsKeyword(cursor, TOKEN_IF))          { cursor += ToString(TOKEN_IF).length;          token->kind = TOKEN_IF;          break; }
				if (IsKeyword(cursor, TOKEN_IMPORT))      { cursor += ToString(TOKEN_IMPORT).length;      token->kind = TOKEN_IMPORT;      break; }
				if (IsKeyword(cursor, TOKEN_IN))          { cursor += ToString(TOKEN_IN).length;          token->kind = TOKEN_IN;          break; }
				if (IsKeyword(cursor, TOKEN_INC))         { cursor += ToString(TOKEN_INC).length;         token->kind = TOKEN_INC;         break; }
				if (IsKeyword(cursor, TOKEN_INT))         { cursor += ToString(TOKEN_INT).length;         token->kind = TOKEN_INT;         break; }
				if (IsKeyword(cursor, TOKEN_INT16))       { cursor += ToString(TOKEN_INT16).length;       token->kind = TOKEN_INT16;       break; }
				if (IsKeyword(cursor, TOKEN_INT32))       { cursor += ToString(TOKEN_INT32).length;       token->kind = TOKEN_INT32;       break; }
				if (IsKeyword(cursor, TOKEN_INT64))       { cursor += ToString(TOKEN_INT64).length;       token->kind = TOKEN_INT64;       break; }
				if (IsKeyword(cursor, TOKEN_INT8))        { cursor += ToString(TOKEN_INT8).length;        token->kind = TOKEN_INT8;        break; }
				goto PARSE_IDENTIFIER;

			case 'n':
				if (IsKeyword(cursor, TOKEN_NOT))         { cursor += ToString(TOKEN_NOT).length;         token->kind = TOKEN_NOT;         break; }
				if (IsKeyword(cursor, TOKEN_NULL))        { cursor += ToString(TOKEN_NULL).length;        token->kind = TOKEN_NULL;        break; }
				goto PARSE_IDENTIFIER;

			case 'o':
				if (IsKeyword(cursor, TOKEN_OR))          { cursor += ToString(TOKEN_OR).length;          token->kind = TOKEN_OR;          break; }
				goto PARSE_IDENTIFIER;

			case 'r':
				if (IsKeyword(cursor, TOKEN_RETURN))      { cursor += ToString(TOKEN_RETURN).length;      token->kind = TOKEN_RETURN;      break; }
				goto PARSE_IDENTIFIER;

			case 's':
				if (IsKeyword(cursor, TOKEN_STRUCT))      { cursor += ToString(TOKEN_STRUCT).length;      token->kind = TOKEN_STRUCT;      break; }
				goto PARSE_IDENTIFIER;

			case 't':
				if (IsKeyword(cursor, TOKEN_THEN))        { cursor += ToString(TOKEN_THEN).length;        token->kind = TOKEN_THEN;        break; }
				if (IsKeyword(cursor, TOKEN_TRUE))        { cursor += ToString(TOKEN_TRUE).length;        token->kind = TOKEN_TRUE;        break; }
				goto PARSE_IDENTIFIER;

			case 'u':
				if (IsKeyword(cursor, TOKEN_UINT))        { cursor += ToString(TOKEN_UINT).length;        token->kind = TOKEN_UINT;        break; }
				if (IsKeyword(cursor, TOKEN_UINT16))      { cursor += ToString(TOKEN_UINT16).length;      token->kind = TOKEN_UINT16;      break; }
				if (IsKeyword(cursor, TOKEN_UINT32))      { cursor += ToString(TOKEN_UINT32).length;      token->kind = TOKEN_UINT32;      break; }
				if (IsKeyword(cursor, TOKEN_UINT64))      { cursor += ToString(TOKEN_UINT64).length;      token->kind = TOKEN_UINT64;      break; }
				if (IsKeyword(cursor, TOKEN_UINT8))       { cursor += ToString(TOKEN_UINT8).length;       token->kind = TOKEN_UINT8;       break; }
				goto PARSE_IDENTIFIER;

			case 'w':
				if (IsKeyword(cursor, TOKEN_WHERE))       { cursor += ToString(TOKEN_WHERE).length;       token->kind = TOKEN_WHERE;       break; }
				if (IsKeyword(cursor, TOKEN_WHILE))       { cursor += ToString(TOKEN_WHILE).length;       token->kind = TOKEN_WHILE;       break; }
				goto PARSE_IDENTIFIER;

			case 'G':
			case 'H':
			case 'I':
			case 'J':
			case 'K':
			case 'L':
			case 'P':
			case 'Q':
			case 'R':
			case 'S':
			case 'T':
			case 'U':
			case 'V':
			case 'W':
			case 'Y':
			case 'Z':
			case '_':
			case 'g':
			case 'h':
			case 'j':
			case 'k':
			case 'l':
			case 'm':
			case 'p':
			case 'q':
			case 'v':
			case 'x':
			case 'y':
			case 'z':
			PARSE_IDENTIFIER:
			{
				char* begin = cursor;

				uint32 upper = 0;
				uint32 lower = 0;

				for (; IsLowerCase(*cursor) || IsUpperCase(*cursor) || IsDecimal(*cursor) || *cursor == '_'; cursor++)
				{
					if      (IsLowerCase(*cursor)) lower++;
					else if (IsUpperCase(*cursor)) upper++;
					else if (CompareStringRaw(cursor, "__"))
					{
						LexerError(module, token->location, "Consecutive underscores aren't allowed.\n");
					}
				}

				uint64 size = cursor - begin;
				String str  = String(begin, size);

				if (*begin == '_') COLD
					LexerError(module, token->location, "Identifier beginning with an underscore.\n");

				if (cursor[-1] == '_') COLD
					LexerError(module, token->location, "Identifier with trailing underscore: '%'\n", str);

				if (CompareStringRaw(cursor-2, "_t") || CompareStringRaw(cursor-2, "_T")) COLD
					LexerError(module, token->location, "Identifier with '_t' is banned.\n", str);

				token->kind = TOKEN_IDENTIFIER_CASUAL;
				token->identifier_string = String(begin, size);

				if (IsHexAlphaLut(*begin) && CheckIfHexadecimal(begin)) COLD
				{
					cursor = begin;
					ParseLiteral(module, &cursor, token);
				}
				else if (!lower)
				{
					token->kind = TOKEN_IDENTIFIER_CONSTANT;
				}
				else if (IsUpperCase(*begin))
				{
					token->kind = TOKEN_IDENTIFIER_FORMAL;
				}
			} break;

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
			{
				ParseLiteral(module, &cursor, token);
			} break;

			case '"':
			{
				token->kind = TOKEN_LITERAL_STRING;

				char* start = ++cursor;
				uint64 escapes = 0;

				for (; cursor < end && *cursor != '"'; cursor++)
				{
					if (*cursor == '\n')
						LexerError(module, token->location, "String literal not completed before end of the line.\n");

					if (*cursor == '\\' && IsEscapeCharacter(cursor[1]))
					{
						escapes++;
						cursor++;
					}
				}

				if (cursor >= end || *cursor != '"')
					LexerError(module, token->location, "String literal not completed before end of the file.\n");

				char* end = cursor++;

				if (escapes)
				{
					uint64 length = end - start - escapes;
					token->literal_string = AllocateString(length, 0);

					char* c = start;
					char* s = token->literal_string.data;

					while (c < end)
					{
						if (*c != '\\')
						{
							*(s++) = *(c);
							continue;
						}

						switch (*(c++))
						{
							case '0':  *s = '\0'; break;
							case 'a':  *s = '\a'; break;
							case 'b':  *s = '\b'; break;
							case 't':  *s = '\t'; break;
							case 'n':  *s = '\n'; break;
							case 'v':  *s = '\v'; break;
							case 'f':  *s = '\f'; break;
							case 'r':  *s = '\r'; break;
							case '\\': *s = '\\'; break;
							case '\"': *s = '\"'; break;

							default:
								LexerError(module, token->location, "Invalid escape character: \\%\n", *c);
						}
					}
				}
				else token->literal_string = String(start, end - start);
			} break;


			case '(': token->kind = TOKEN_OPEN_PAREN;   goto PARSE_OPEN;
			case '{': token->kind = TOKEN_OPEN_BRACE;   goto PARSE_OPEN;
			case '[': token->kind = TOKEN_OPEN_BRACKET; goto PARSE_OPEN;
			PARSE_OPEN:
			{
				cursor++;
				token->closure = open_token;
				open_token = token;
			} break;

			Token_Kind doppelganger;
			case ')': doppelganger = TOKEN_OPEN_PAREN;   token->kind = TOKEN_CLOSE_PAREN;   goto PARSE_CLOSE;
			case '}': doppelganger = TOKEN_OPEN_BRACE;   token->kind = TOKEN_CLOSE_BRACE;   goto PARSE_CLOSE;
			case ']': doppelganger = TOKEN_OPEN_BRACKET; token->kind = TOKEN_CLOSE_BRACKET; goto PARSE_CLOSE;
			PARSE_CLOSE:
			{
				cursor++;

				if (!open_token)
					break;

				if (open_token->kind != doppelganger)
				{
					LexerError(
						module, token->location,
						"Expected closure '%', not: '%'\n",
						GetMatchingParen(open_token->kind), token->kind
					);
				}

				Token* prev = open_token->closure;
				open_token->closure = token;
				open_token = prev;
			} break;

			case '.':
			{
				token->kind = TOKEN_DOT;

				if (cursor[1] == '.')
				{
					token->kind = TOKEN_DOT_DOT;
					cursor += 2;
					break;
				}

				if (IsDecimal(cursor[1]) || IsHexAlpha(cursor[1]) && !IsAlpha(cursor[-1]) && CheckIfHexadecimal(cursor+1))
				{
					ParseLiteral(module, &cursor, token);
					break;
				}

				cursor += 1;
			} break;

			case '=':
				cursor++, token->kind = TOKEN_EQUAL;
				if (*cursor == '>') cursor++, token->kind = TOKEN_FAT_ARROW;
				break;

			case '!':
				cursor++, token->kind = TOKEN_EXCLAMATION_MARK;
				if (*cursor == '=') cursor++, token->kind = TOKEN_NOT_EQUAL;
				break;

			case '-':
				cursor++, token->kind = TOKEN_MINUS;
				if      (*cursor == '>') cursor++, token->kind = TOKEN_ARROW;
				else if (*cursor == '=') cursor++, token->kind = TOKEN_MINUS_EQUAL;
				break;


			case '<':
				cursor++, token->kind = TOKEN_LESS;
				if      (*cursor == '<') cursor++, token->kind = TOKEN_LEFT_SHIFT;
				else if (*cursor == '=') cursor++, token->kind = TOKEN_LESS_OR_EQUAL;
				break;

			case '>':
				cursor++, token->kind = TOKEN_GREATER;
				if      (*cursor == '>') cursor++, token->kind = TOKEN_RIGHT_SHIFT;
				else if (*cursor == '=') cursor++, token->kind = TOKEN_GREATER_OR_EQUAL;
				break;

			case '+':
				cursor++, token->kind = TOKEN_PLUS;
				if (*cursor == '=') cursor++, token->kind = TOKEN_PLUS_EQUAL;
				break;

			case '*':
				cursor++, token->kind = TOKEN_ASTERISK;
				if (*cursor == '=') cursor++, token->kind = TOKEN_TIMES_EQUAL;
				break;

			case '/':
				cursor++, token->kind = TOKEN_DIVIDE;
				if (*cursor == '=') cursor++, token->kind = TOKEN_DIVIDE_EQUAL;
				break;

			case '^':
				cursor++, token->kind = TOKEN_CARET;
				if (*cursor == '=') cursor++, token->kind = TOKEN_CARET_EQUAL;
				break;

			case '?':
				cursor++, token->kind = TOKEN_QUESTION_MARK;
				if (*cursor == '.') cursor++, token->kind = TOKEN_QUESTION_MARK_DOT;
				break;

			case ';':
				cursor++, token->kind = TOKEN_SEMICOLON;
				break;

			case ':':
				cursor++, token->kind = TOKEN_COLON;
				break;

			case ',':
				cursor++, token->kind = TOKEN_COMMA;

				if (open_token)
					open_token->comma_count++;

				break;

			case '&':
				cursor++, token->kind = TOKEN_AMPERSAND;
				break;

			case '|':
				cursor++, token->kind = TOKEN_BAR;
				break;

			default:
			{
				LexerError(module, token->location, "Invalid character: '%', %\n", *cursor, Hex(*cursor));
			} break;
		}

		token->location.extent = cursor-token_begin;

		token++;
	}

	Line line;
	line.indent = indent;
	line.string = String(line_begin, cursor-line_begin);
	line.tokens_begin = line_begin_token;
	line.tokens_end   = token;
	lines.Add(line);

	ZeroMemory(token);
	token->kind = TOKEN_EOF;
	token->location.line = line_number;
	token->location.offset = 0;
	token->indent = 0;
	token->newline = true;
	token++;

	Token* token_end = token;
	uint64 token_count = token_end - tokens_begin;

	module->tokens = Array(tokens, token_count);
	module->code   = code;
	module->lines  = lines.ToArray();

	while (open_token)
	{
		Token* next = open_token->closure;
		open_token->closure = null;
		open_token = next;
	}
}

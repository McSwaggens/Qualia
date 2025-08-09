#include "lexer.h"

#include "error.h"
#include "general.h"
#include "parser.h"
#include "memory.h"
#include "print.h"
#include "ascii.h"
#include "string.h"
#include "token.h"


using LiteralQualifier = u64;

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

static inline Base GetBaseFromQualifier(LiteralQualifier qualifier) {
	switch (qualifier & QUALIFIER_BASE_MASK) {
		case 0: return BASE_DECIMAL;
		case 1: return BASE_BINARY;
		case 2: return BASE_HEX;
		default: AssertUnreachable();
	}
}

static LiteralQualifier ParseLiteralQualifier(char** begin) {
	LiteralQualifier qualifier = 0;
	char* p = *begin;

	if      (*p == 'h') { qualifier |= QUALIFIER_H; p++; }
	else if (*p == 'b') { qualifier |= QUALIFIER_B; p++; }

	if (*p == 'f') {
		qualifier |= QUALIFIER_F;
		p++;
	}
	else {
		switch (*p) {
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

	if (qualifier) {
		if      (CompareStringRaw(p, "64")) { qualifier |= QUALIFIER_64 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "32")) { qualifier |= QUALIFIER_32 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "16")) { qualifier |= QUALIFIER_16 | QUALIFIER_JUNK_BIT; p += 2; }
		else if (CompareStringRaw(p, "8"))  { qualifier |= QUALIFIER_8;  p += 1; }
	}

	if ((qualifier & QUALIFIER_CERTAIN_MASK) == 0 && (IsHexLut(*p) || *p == '_' || *p == 'h'))
		return 0;

	*begin = p;
	return qualifier;
}

static float64 AsciiToFloat(char* p, u64 count, Base base) {
	float64 value = 0.0;

	for (u32 i = 0; i < count; i++) {
		Assert(IsDigit(*p, base));
		value *= (float64)base;
		value += (float64)DecodeDigitLut(*p);
		if (*++p == '_') ++p;
	}

	return value;
}

static float64 AsciiToFloat_Fractional(char* p, u64 count, Base base) {
	float64 value = 0.0;
	float64 scale = 1.0;

	for (u32 i = 0; i < count; i++) {
		// @OptimizeMe: Nasty dependency chain. Precompute base^-count?
		Assert(IsDigit(*p, base));
		scale *= 1.0/(float64)base;
		value += (float64)DecodeDigitLut(*p) * scale;
		if (*++p == '_') ++p;
	}

	return value;
}

static u64 AsciiToInt(char* begin, u64 count, Base base, bool* overflow) {
	u64 value = 0;

	if (base == BASE_BINARY) {
		char* p = begin;

		if (count > 64) COLD {
			*overflow = true;
			return value;
		}

		for (u32 i = 0; i < count; i++) {
			Assert(IsDigit(*p, BASE_BINARY));
			value |= (u64)DecodeDigit(*p, BASE_BINARY) << (u64)(count-1-i);
			if (*++p == '_') ++p;
		}
	}
	else if (base == BASE_DECIMAL) HOT {
		if (count > 20) COLD {
			*overflow = true;
			return value;
		}

		if (count == 20) COLD {
			char* p = begin;
			for (u32 i = 0; i < count; i++) {
				if (*p > "18446744073709551615"[i]) {
					*overflow = true;
					return value;
				}

				if (*++p == '_') ++p;
			}
		}

		static const u64 factors[20] = {
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

		u64 base_factor_index = 20-count;
		char* p = begin;

		for (u32 i = 0; i < count; i++) {
			Assert(IsDigit(*p, BASE_DECIMAL));
			value += factors[base_factor_index+i] * (u64)DecodeDigit(*p, BASE_DECIMAL);
			if (*++p == '_') ++p;
		}
	}
	else if (base == BASE_HEX) {
		char* p = begin;
		if (count > 16) COLD {
			*overflow = true;
			return value;
		}

		for (u32 i = 0; i < count; i++) {
			value |= (u64)DecodeDigitLut(*p) << (u64)(count-1-i)*4;
			if (*++p == '_') ++p;
		}
	}

	return value;
}

struct LiteralComponent {
	char* begin;
	u32 count;
	Base base;
	LiteralQualifier qualifier;
	char* end;
};

static LiteralComponent ParseLiteralComponent(char* begin) {
	LiteralComponent component;
	component.count = 0;
	component.begin = begin;
	component.base = BASE_BINARY;

	char* p = begin;

	while (IsBinary(*p))  { component.base = BASE_BINARY;  component.count++; if (*++p == '_') p++; }
	while (IsDecimal(*p)) { component.base = BASE_DECIMAL; component.count++; if (*++p == '_') p++; }

	component.qualifier = ParseLiteralQualifier(&p);

	if (!component.qualifier && IsHex(*p)) {
		while (IsHexLut(*p)) { component.count++; if (*++p == '_') p++; }
		component.base = BASE_HEX;
		component.qualifier = ParseLiteralQualifier(&p);
	}

	component.end = p;
	return component;
}

void Lexer::ParseLiteral() {
	LiteralComponent whole;
	LiteralComponent fractional;

	char* p = cursor;

	while (*p == '0')
		if (*++p == '_') p++;

	whole = ParseLiteralComponent(p);
	bool has_fractional = false;
	LiteralQualifier qualifier = whole.qualifier;
	Base base = whole.base;

	if (whole.end[0] == '.' && (whole.qualifier & QUALIFIER_CERTAIN_MASK) == 0) {
		fractional = ParseLiteralComponent(whole.end+1);

		if (IsDigit(whole.end[1], BASE_DECIMAL) ||
			!qualifier && fractional.qualifier && !IsAlpha(fractional.end[0]) ||
			qualifier && (fractional.qualifier & QUALIFIER_H) && !IsAlpha(fractional.end[0])) {
			qualifier = fractional.qualifier;
			base = (Base)Max(base, fractional.base);
			whole.count += CountBits64(whole.qualifier);
			has_fractional = true;
		}
	}

	p = has_fractional ? fractional.end : whole.end;

	Base qbase = GetBaseFromQualifier(qualifier);

	if (base == BASE_HEX && (qualifier & QUALIFIER_BASE_MASK) == 0)
		LexerError(module, location, "Hexadecimal missing 'h' qualifier.\n");

	if ((qualifier & QUALIFIER_BASE_MASK) && qbase < base)
		LexerError(module, location, "% literal with % digits.\n", ToString(qbase), ToString(base));

	base = qbase;

	if ((qualifier & QUALIFIER_F) || has_fractional && (qualifier & QUALIFIER_INTEGER_MASK) == 0) {
		switch (qualifier & QUALIFIER_SIZE_MASK) {
			default:           current_token->kind = TOKEN_LITERAL_FLOAT;   break;
			case QUALIFIER_32: current_token->kind = TOKEN_LITERAL_FLOAT32; break;
			case QUALIFIER_64: current_token->kind = TOKEN_LITERAL_FLOAT64; break;

			case QUALIFIER_16:
				LexerError(module, location, "Invalid float literal qualifier: float16 is not a valid type.\n");

			case QUALIFIER_8:
				LexerError(module, location, "Invalid float literal qualifier: float8 is not a valid type.\n");
		}

		float64 value = AsciiToFloat(whole.begin, whole.count, base);

		if (has_fractional) {
			float64 fractional_value = AsciiToFloat_Fractional(fractional.begin, fractional.count, base);
			value += fractional_value;
		}

		current_token->literal_float = value;
	}
	else {
		switch (qualifier & (QUALIFIER_SIZE_MASK | QUALIFIER_U)) {
			default:           current_token->kind = TOKEN_LITERAL_INT;   break;
			case QUALIFIER_8:  current_token->kind = TOKEN_LITERAL_INT8;  break;
			case QUALIFIER_16: current_token->kind = TOKEN_LITERAL_INT16; break;
			case QUALIFIER_32: current_token->kind = TOKEN_LITERAL_INT32; break;
			case QUALIFIER_64: current_token->kind = TOKEN_LITERAL_INT64; break;

			case QUALIFIER_U:                current_token->kind = TOKEN_LITERAL_UINT;   break;
			case QUALIFIER_8  | QUALIFIER_U: current_token->kind = TOKEN_LITERAL_UINT8;  break;
			case QUALIFIER_16 | QUALIFIER_U: current_token->kind = TOKEN_LITERAL_UINT16; break;
			case QUALIFIER_32 | QUALIFIER_U: current_token->kind = TOKEN_LITERAL_UINT32; break;
			case QUALIFIER_64 | QUALIFIER_U: current_token->kind = TOKEN_LITERAL_UINT64; break;
		}

		bool overflow = false;
		u64 scaler = (qualifier & QUALIFIER_SCALER_MASK);
		u64 size = qualifier & QUALIFIER_SIZE_MASK;
		u64 effective_size = size ? size : 64;
		u64 value = AsciiToInt(whole.begin, whole.count, base, &overflow);
		u64 fractional_scaled = 0;

		if (overflow)
			LexerError(module, location, "Integer literal exceeds %-bits.\n", effective_size);

		if (has_fractional) {
			if (!scaler)
				LexerError(module, location, "Integer missing scaler qualifier.\n");

			float64 fractional_value = AsciiToFloat_Fractional(fractional.begin, fractional.count, base);
			fractional_scaled = (u64)(fractional_value * (float64)scaler);
		}

		if (scaler) {
			u64 scaled_value = 0;

			if (CheckedMultiply(value, scaler, &scaled_value) || CheckedAdd(scaled_value, fractional_scaled, &scaled_value))
				LexerError(module, location, "Integer literal with scaler exceeds %-bits.\n", effective_size);

			value = scaled_value;
		}

		if (BitsOfInformation64(value) > effective_size)
			LexerError(module, location, "Integer literal exceeds %-bits.\n", effective_size);

		current_token->literal_int = value;
	}

	cursor = p;

	if (IsAlpha(*p))
		LexerError(module, location, "Unexpected character after literal: '%'.\n", *p);
}

static bool CheckIfHexadecimal(char* p) {
	while (IsHexLut(*p) || *p == '_') p++;

	if (*p == '.') {
		p += 1;
		while (IsHexLut(*p) || *p == '_') p++;
	}

	return *p == 'h' && ParseLiteralQualifier(&p);
}

// -------------------------------------------- //

static TokenKind GetMatchingParen(TokenKind kind) {
	switch (kind) {
		case TOKEN_OPEN_PAREN:    return TOKEN_OPEN_PAREN;
		case TOKEN_OPEN_BRACE:    return TOKEN_OPEN_BRACE;
		case TOKEN_OPEN_BRACKET:  return TOKEN_OPEN_BRACKET;
		case TOKEN_CLOSE_PAREN:   return TOKEN_CLOSE_PAREN;
		case TOKEN_CLOSE_BRACE:   return TOKEN_CLOSE_BRACE;
		case TOKEN_CLOSE_BRACKET: return TOKEN_CLOSE_BRACKET;
		default: AssertUnreachable();
	}
}

static TokenKind GetDoppleGanger(TokenKind kind) {
	switch (kind) {
		case TOKEN_OPEN_PAREN:    return TOKEN_CLOSE_PAREN;
		case TOKEN_OPEN_BRACE:    return TOKEN_CLOSE_BRACE;
		case TOKEN_OPEN_BRACKET:  return TOKEN_CLOSE_BRACKET;
		case TOKEN_CLOSE_PAREN:   return TOKEN_OPEN_PAREN;
		case TOKEN_CLOSE_BRACE:   return TOKEN_OPEN_BRACE;
		case TOKEN_CLOSE_BRACKET: return TOKEN_OPEN_BRACKET;
		default: AssertUnreachable();
	}
}

static inline bool IsKeyword(char* p, TokenKind kind) {
	return true;
}

bool Lexer::TestKeyword(TokenKind keyword) {
	String keyword_string = ToString(keyword);
	char post = cursor[keyword_string.length];

	if (!CompareMemory(cursor, ToString(keyword).data, ToString(keyword).length))
		return false;

	if (IsAlpha(post))
		return false;

	if (IsDigit(post, BASE_DECIMAL))
		return false;

	if (post == '_')
		return false;


	current_token->kind = keyword;
	cursor += keyword_string.length;

	return true;
}

void Lexer::Parse() {
	char* end = code.data + code.length;
	char* line_begin = cursor;

	Token* line_begin_token = current_token;
	Token* open_token = null;

	while (cursor < end) {
		u64 old_line = line_number;

		bool did_skip_whitespace = false;
		while (IsWhiteSpace(*cursor) || *cursor == '\n' || *cursor == '\r' || CompareStringRaw(cursor, "//")) {
			did_skip_whitespace = true;

			if (*cursor == '\t' && cursor == line_begin) {
				while (*++cursor == '\t');
				indent = cursor - line_begin;
				continue;
			}

			if (CompareStringRaw(cursor, "//")) {
				for (cursor += 2; cursor < end && *cursor != '\n'; cursor++);
			}

			if (*cursor == '\n') {
				lines.Add((Line){
					.indent = (Indent16)indent,
					.string = String(line_begin, cursor-line_begin),
					.tokens_begin = line_begin_token,
					.tokens_end   = current_token,
				});

				line_begin_token = current_token;
				line_number++;
				line_begin = cursor+1;
				cursor++;
				indent = 0;
				continue;

			}

			cursor++;
		}

		bool newline = line_number != old_line || current_token == tokens.Begin();
		char* token_begin = cursor;

		*current_token = (Token){
			.indent = (Indent16)indent,
			.location = {
				.line = line_number,
				.offset = cursor - line_begin,
			},
		};

		if (newline) current_token->flags |= TOKEN_FLAG_NEWLINE;

		if (did_skip_whitespace) {
			current_token->flags    |= TOKEN_FLAG_LEFT_SPACED;
			current_token[-1].flags |= TOKEN_FLAG_RIGHT_SPACED;
		}

		if (cursor >= end)
			break;

		switch (*cursor) {
			case 'B': case 'C': case 'D': case 'E':
			case 'F': {
				if (!CheckIfHexadecimal(cursor))
					goto PARSE_IDENTIFIER;

				ParseLiteral();
			} break;

			case 'a':
				if (TestKeyword(TOKEN_ALIAS))   break;
				if (TestKeyword(TOKEN_AND))     break;
				if (TestKeyword(TOKEN_AS))      break;
				if (TestKeyword(TOKEN_ASM))     break;
				goto PARSE_IDENTIFIER;

			case 'b':
				if (TestKeyword(TOKEN_BOOL))    break;
				if (TestKeyword(TOKEN_BREAK))   break;
				if (TestKeyword(TOKEN_BYTE))    break;
				goto PARSE_IDENTIFIER;

			case 'c':
				if (TestKeyword(TOKEN_CLAIM))   break;
				goto PARSE_IDENTIFIER;

			case 'd':
				if (TestKeyword(TOKEN_DEC))     break;
				if (TestKeyword(TOKEN_DEFER))   break;
				goto PARSE_IDENTIFIER;

			case 'e':
				if (TestKeyword(TOKEN_ELSE))    break;
				if (TestKeyword(TOKEN_ENUM))    break;
				goto PARSE_IDENTIFIER;

			case 'f':
				if (TestKeyword(TOKEN_FALSE))   break;
				if (TestKeyword(TOKEN_FLOAT32)) break;
				if (TestKeyword(TOKEN_FLOAT64)) break;
				if (TestKeyword(TOKEN_FOR))     break;
				goto PARSE_IDENTIFIER;

			case 'i':
				if (TestKeyword(TOKEN_IF))      break;
				if (TestKeyword(TOKEN_IMPORT))  break;
				if (TestKeyword(TOKEN_IN))      break;
				if (TestKeyword(TOKEN_INC))     break;
				if (TestKeyword(TOKEN_INT))     break;
				if (TestKeyword(TOKEN_INT16))   break;
				if (TestKeyword(TOKEN_INT32))   break;
				if (TestKeyword(TOKEN_INT64))   break;
				if (TestKeyword(TOKEN_INT8))    break;
				goto PARSE_IDENTIFIER;

			case 'n':
				if (TestKeyword(TOKEN_NOT))     break;
				if (TestKeyword(TOKEN_NULL))    break;
				goto PARSE_IDENTIFIER;

			case 'o':
				if (TestKeyword(TOKEN_OR))      break;
				goto PARSE_IDENTIFIER;

			case 'r':
				if (TestKeyword(TOKEN_RETURN))  break;
				goto PARSE_IDENTIFIER;

			case 's':
				if (TestKeyword(TOKEN_STRUCT))  break;
				goto PARSE_IDENTIFIER;

			case 't':
				if (TestKeyword(TOKEN_THEN))    break;
				if (TestKeyword(TOKEN_TRUE))    break;
				goto PARSE_IDENTIFIER;

			case 'u':
				if (TestKeyword(TOKEN_UINT))    break;
				if (TestKeyword(TOKEN_UINT16))  break;
				if (TestKeyword(TOKEN_UINT32))  break;
				if (TestKeyword(TOKEN_UINT64))  break;
				if (TestKeyword(TOKEN_UINT8))   break;
				goto PARSE_IDENTIFIER;

			case 'w':
				if (TestKeyword(TOKEN_WHERE))   break;
				if (TestKeyword(TOKEN_WHILE))   break;
				goto PARSE_IDENTIFIER;

			case '_':
			case 'A': case 'g': case 'G': case 'h':
			case 'H': case 'I': case 'j': case 'J':
			case 'k': case 'K': case 'l': case 'L':
			case 'm': case 'M': case 'N': case 'O':
			case 'p': case 'P': case 'q': case 'Q':
			case 'R': case 'S': case 'T': case 'U':
			case 'v': case 'V': case 'W': case 'x':
			case 'X': case 'y': case 'Y': case 'z':
			case 'Z':
			PARSE_IDENTIFIER: {
				char* begin = cursor;

				u32 upper = 0;
				u32 lower = 0;

				for (; IsLowerCase(*cursor) || IsUpperCase(*cursor) || IsDecimal(*cursor) || *cursor == '_'; cursor++) {
					if (IsLowerCase(*cursor)) lower++;
					if (IsUpperCase(*cursor)) upper++;

					if (CompareStringRaw(cursor, "__"))
						LexerError(module, location, "Consecutive underscores aren't allowed.\n");
				}

				u64 size = cursor - begin;
				String str = String(begin, size);

				if (*begin == '_') COLD
					LexerError(module, location, "Identifier beginning with an underscore.\n");

				if (cursor[-1] == '_') COLD
					LexerError(module, location, "Identifier with trailing underscore: '%'\n", str);

				if (CompareStringRaw(cursor-2, "_t") || CompareStringRaw(cursor-2, "_T")) COLD
					LexerError(module, location, "Identifier with '_t' is banned.\n", str);

				current_token->kind = TOKEN_IDENTIFIER_CASUAL;
				current_token->identifier_string = String(begin, size);

				if (IsHexAlphaLut(*begin) && CheckIfHexadecimal(begin)) COLD {
					cursor = begin;
					ParseLiteral();
				}
				else if (!lower) {
					current_token->kind = TOKEN_IDENTIFIER_CONSTANT;
				}
				else if (IsUpperCase(*begin)) {
					current_token->kind = TOKEN_IDENTIFIER_FORMAL;
				}
			} break;

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				ParseLiteral();
			} break;

			case '"': {
				current_token->kind = TOKEN_LITERAL_STRING;

				char* start = ++cursor;
				u64 escapes = 0;

				for (; cursor < end && *cursor != '"'; cursor++) {
					if (*cursor == '\n')
						LexerError(module, location, "String literal not completed before end of the line.\n");

					if (*cursor == '\\' && IsEscapeCharacter(cursor[1])) {
						escapes++;
						cursor++;
					}
				}

				if (cursor >= end || *cursor != '"')
					LexerError(module, location, "String literal not completed before end of the file.\n");

				char* end = cursor++;

				if (escapes) {
					u64 length = end - start - escapes;
					current_token->literal_string = AllocateString(length, 0);

					char* c = start;
					char* s = current_token->literal_string.data;

					while (c < end) {
						if (*c != '\\') {
							*(s++) = *(c);
							continue;
						}

						switch (*(c++)) {
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
								LexerError(module, location, "Invalid escape character: \\%\n", *c);
						}
					}
				}
				else current_token->literal_string = String(start, end - start);
			} break;


			case '(': current_token->kind = TOKEN_OPEN_PAREN;   goto PARSE_OPEN;
			case '{': current_token->kind = TOKEN_OPEN_BRACE;   goto PARSE_OPEN;
			case '[': current_token->kind = TOKEN_OPEN_BRACKET; goto PARSE_OPEN;
			PARSE_OPEN: {
				cursor++;
				current_token->closure = open_token;
				open_token = current_token;
			} break;

			TokenKind doppelganger;
			case ')': doppelganger = TOKEN_OPEN_PAREN;   current_token->kind = TOKEN_CLOSE_PAREN;   goto PARSE_CLOSE;
			case '}': doppelganger = TOKEN_OPEN_BRACE;   current_token->kind = TOKEN_CLOSE_BRACE;   goto PARSE_CLOSE;
			case ']': doppelganger = TOKEN_OPEN_BRACKET; current_token->kind = TOKEN_CLOSE_BRACKET; goto PARSE_CLOSE;
			PARSE_CLOSE: {
				cursor++;

				if (!open_token)
					break;

				if (open_token->kind != doppelganger)
					LexerError(
						module, location,
						"Expected closure '%', not: '%'\n",
						GetMatchingParen(open_token->kind), current_token->kind
					);

				Token* prev = open_token->closure;
				open_token->closure = current_token;
				open_token = prev;
			} break;

			case '.': {
				current_token->kind = TOKEN_DOT;

				if (cursor[1] == '.') {
					current_token->kind = TOKEN_DOT_DOT;
					cursor += 2;
					break;
				}

				if (IsDecimal(cursor[1]) || IsHexAlpha(cursor[1]) && !IsAlpha(cursor[-1]) && CheckIfHexadecimal(cursor+1)) {
					ParseLiteral();
					break;
				}

				cursor += 1;
			} break;

			case '=':
				cursor++, current_token->kind = TOKEN_EQUAL;
				if (*cursor == '>') cursor++, current_token->kind = TOKEN_FAT_ARROW;
				break;

			case '!':
				cursor++, current_token->kind = TOKEN_EXCLAMATION_MARK;
				if (*cursor == '=') cursor++, current_token->kind = TOKEN_NOT_EQUAL;
				break;

			case '-':
				cursor++, current_token->kind = TOKEN_MINUS;
				if      (*cursor == '>') cursor++, current_token->kind = TOKEN_ARROW;
				else if (*cursor == '=') cursor++, current_token->kind = TOKEN_MINUS_EQUAL;
				break;


			case '<':
				cursor++, current_token->kind = TOKEN_LESS;
				if      (*cursor == '<') cursor++, current_token->kind = TOKEN_LEFT_SHIFT;
				else if (*cursor == '=') cursor++, current_token->kind = TOKEN_LESS_OR_EQUAL;
				break;

			case '>':
				cursor++, current_token->kind = TOKEN_GREATER;
				if      (*cursor == '>') cursor++, current_token->kind = TOKEN_RIGHT_SHIFT;
				else if (*cursor == '=') cursor++, current_token->kind = TOKEN_GREATER_OR_EQUAL;
				break;

			case '+':
				cursor++, current_token->kind = TOKEN_PLUS;
				if (*cursor == '=') cursor++, current_token->kind = TOKEN_PLUS_EQUAL;
				break;

			case '*':
				cursor++, current_token->kind = TOKEN_ASTERISK;
				if (*cursor == '=') cursor++, current_token->kind = TOKEN_TIMES_EQUAL;
				break;

			case '/':
				cursor++, current_token->kind = TOKEN_DIVIDE;
				if (*cursor == '=') cursor++, current_token->kind = TOKEN_DIVIDE_EQUAL;
				break;

			case '^':
				cursor++, current_token->kind = TOKEN_CARET;
				if (*cursor == '=') cursor++, current_token->kind = TOKEN_CARET_EQUAL;
				break;

			case '?':
				cursor++, current_token->kind = TOKEN_QUESTION_MARK;
				if (*cursor == '.') cursor++, current_token->kind = TOKEN_QUESTION_MARK_DOT;
				break;

			case ';':
				cursor++, current_token->kind = TOKEN_SEMICOLON;
				break;

			case ':':
				cursor++, current_token->kind = TOKEN_COLON;
				break;

			case ',':
				cursor++, current_token->kind = TOKEN_COMMA;

				if (open_token)
					open_token->comma_count++;

				break;

			case '&':
				cursor++, current_token->kind = TOKEN_AMPERSAND;
				if (*cursor == '&') cursor++, current_token->kind = TOKEN_AND;
				break;

			case '|':
				cursor++, current_token->kind = TOKEN_BAR;
				break;

			default: {
				LexerError(module, location, "Invalid character: '%', %\n", *cursor, Hex(*cursor));
			} break;
		}

		current_token->location.extent = cursor-token_begin;
		current_token++;
	}

	lines.Add({
		.indent = (Indent16)indent,
		.string = String(line_begin, cursor-line_begin),
		.tokens_begin = line_begin_token,
		.tokens_end   = current_token,
	});

	*current_token++ = (Token){
		.kind = TOKEN_EOF,
		.flags = TOKEN_FLAG_NEWLINE,
		.indent = 0,
		.location = {
			.line = line_number,
			.offset = 0,
		},
	};

	tokens.length = current_token - tokens.Begin();

	module->tokens = tokens;
	module->code   = code;
	module->lines  = lines.ToArray();

	while (open_token) {
		Token* next = open_token->closure;
		open_token->closure = null;
		open_token = next;
	}
}

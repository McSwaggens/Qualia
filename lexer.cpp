#include "parser.h"
#include "list.h"
#include "memory.h"
#include "file.h"
#include "print.h"
#include "util.h"
#include "span.h"
#include "string.h"

Parse_Info LexicalParse(String file_path)
{
	Span<char> code = LoadFile(file_path, 16);
	char* cursor = code;
	List<Token> tokens = CreateList<Token>(1024);
	List<Span<char>> lines = CreateList<Span<char>>(1024);
	u32 line = 0;
	u32 indent = 0;
	u32 prev_region_index = -1;
	char* line_start = code;

	while (cursor < code.end)
	{
		u32 old_line = line;
		u32 old_indent = indent;

		while (IsWhiteSpace(*cursor) || *cursor == '\n' || *cursor == '\r' || (cursor[0] == '/' && cursor[1] == '/'))
		{
			if (*cursor == '\t' && cursor == line_start)
			{
				while (*++cursor == '\t');
				indent = cursor - line_start;
			}
			else if (cursor[0] == '/' && cursor[1] == '/')
			{
				for (cursor += 2; cursor < code.end && *cursor != '\n'; cursor++);
			}
			else if (*cursor == '\n')
			{
				lines.Add(Span(line_start, cursor));
				line++;
				line_start = ++cursor;
				indent = 0;
			}
			else cursor++;
		}

		Token token;
		token.indent = indent;
		token.location.line = line;
		token.location.offset = cursor - line_start;
		token.newline = line != old_line && tokens.count;

		if (cursor >= code.end) break;

		if (IsLetter(*cursor))
		{
			char* start = cursor++;
			while (IsLetter(*cursor) || IsDigit(*cursor) || *cursor == '_') cursor++;
			char* end = cursor;
			u64 size = end - start;
			String str = String(start, size);

			if      (CompareStrings(str, ToString(TOKEN_IMPORT)))      token.kind = TOKEN_IMPORT;
			else if (CompareStrings(str, ToString(TOKEN_STRUCT)))      token.kind = TOKEN_STRUCT;
			else if (CompareStrings(str, ToString(TOKEN_ENUM)))        token.kind = TOKEN_ENUM;
			else if (CompareStrings(str, ToString(TOKEN_IF)))          token.kind = TOKEN_IF;
			else if (CompareStrings(str, ToString(TOKEN_ELSE)))        token.kind = TOKEN_ELSE;
			else if (CompareStrings(str, ToString(TOKEN_THEN)))        token.kind = TOKEN_THEN;
			else if (CompareStrings(str, ToString(TOKEN_WHERE)))       token.kind = TOKEN_WHERE;
			else if (CompareStrings(str, ToString(TOKEN_FOR)))         token.kind = TOKEN_FOR;
			else if (CompareStrings(str, ToString(TOKEN_WHILE)))       token.kind = TOKEN_WHILE;
			else if (CompareStrings(str, ToString(TOKEN_RETURN)))      token.kind = TOKEN_RETURN;
			else if (CompareStrings(str, ToString(TOKEN_BREAK)))       token.kind = TOKEN_BREAK;
			else if (CompareStrings(str, ToString(TOKEN_CLAIM)))       token.kind = TOKEN_CLAIM;
			else if (CompareStrings(str, ToString(TOKEN_DEFER)))       token.kind = TOKEN_DEFER;
			else if (CompareStrings(str, ToString(TOKEN_ALIAS)))       token.kind = TOKEN_ALIAS;
			else if (CompareStrings(str, ToString(TOKEN_AS)))          token.kind = TOKEN_AS;
			else if (CompareStrings(str, ToString(TOKEN_IN)))          token.kind = TOKEN_IN;
			else if (CompareStrings(str, ToString(TOKEN_NULL)))        token.kind = TOKEN_NULL;
			else if (CompareStrings(str, ToString(TOKEN_TRUE)))        token.kind = TOKEN_TRUE;
			else if (CompareStrings(str, ToString(TOKEN_FALSE)))       token.kind = TOKEN_FALSE;
			else if (CompareStrings(str, ToString(TOKEN_BOOL)))        token.kind = TOKEN_BOOL;
			else if (CompareStrings(str, ToString(TOKEN_INT)))         token.kind = TOKEN_INT;
			else if (CompareStrings(str, ToString(TOKEN_INT8)))        token.kind = TOKEN_INT8;
			else if (CompareStrings(str, ToString(TOKEN_INT16)))       token.kind = TOKEN_INT16;
			else if (CompareStrings(str, ToString(TOKEN_INT32)))       token.kind = TOKEN_INT32;
			else if (CompareStrings(str, ToString(TOKEN_INT64)))       token.kind = TOKEN_INT64;
			else if (CompareStrings(str, ToString(TOKEN_UINT)))        token.kind = TOKEN_UINT;
			else if (CompareStrings(str, ToString(TOKEN_UINT8)))       token.kind = TOKEN_UINT8;
			else if (CompareStrings(str, ToString(TOKEN_UINT16)))      token.kind = TOKEN_UINT16;
			else if (CompareStrings(str, ToString(TOKEN_UINT32)))      token.kind = TOKEN_UINT32;
			else if (CompareStrings(str, ToString(TOKEN_UINT64)))      token.kind = TOKEN_UINT64;
			else if (CompareStrings(str, ToString(TOKEN_FLOAT16)))     token.kind = TOKEN_FLOAT16;
			else if (CompareStrings(str, ToString(TOKEN_FLOAT32)))     token.kind = TOKEN_FLOAT32;
			else if (CompareStrings(str, ToString(TOKEN_FLOAT64)))     token.kind = TOKEN_FLOAT64;
			else if (CompareStrings(str, ToString(TOKEN_OR)))          token.kind = TOKEN_OR;
			else if (CompareStrings(str, ToString(TOKEN_AND)))         token.kind = TOKEN_AND;
			else if (CompareStrings(str, ToString(TOKEN_NOT)))         token.kind = TOKEN_NOT;
			else if (CompareStrings(str, ToString(TOKEN_MOD)))         token.kind = TOKEN_MOD;
			else if (CompareStrings(str, ToString(TOKEN_BITWISE_OR)))  token.kind = TOKEN_BITWISE_OR;
			else if (CompareStrings(str, ToString(TOKEN_BITWISE_AND))) token.kind = TOKEN_BITWISE_AND;
			else if (CompareStrings(str, ToString(TOKEN_BITWISE_XOR))) token.kind = TOKEN_BITWISE_XOR;
			else if (CompareStrings(str, ToString(TOKEN_BITWISE_NOT))) token.kind = TOKEN_BITWISE_NOT;
			else
			{
				if (cursor[-1] == '_')
				{
					Print("error: Identifiers cannot end with an underscore: '%'\n", str);
					Fail();
				}

				token.kind = TOKEN_IDENTIFIER;
				token.info.string = String(start, size);
			}
		}
		else if (IsDigit(*cursor))
		{
			char* start = cursor;
			u64 value = 0;

			if (cursor[0] == '0' && cursor[1] == 'x')
			{
				cursor += 2;

				if (*cursor == '_')
				{
					Print("error: Hexadecimal literal cannot start with an underscore.\n");
					Fail();
				}

				if (!IsDigit(*cursor, Hexadecimal))
				{
					Print("error: Invalid hexadecimal digit: %\n", *cursor);
					Fail();
				}

				while (*cursor == '0') if (*++cursor == '_') cursor++;

				for (u32 i = 0; i < 16 && IsDigit(*cursor, Hexadecimal); i++)
				{
					value = (value << 4) | DigitToInt(*cursor++, Hexadecimal);
					if (*cursor == '_') ++cursor;
				}

				if (*cursor == '_')
				{
					Print("error: Only a single underscore is allowed.\n");
					Fail();
				}

				if (IsDigit(*cursor, Hexadecimal))
				{
					Print("error: Integer too large.\n");
					Fail();
				}
			}
			else if (cursor[0] == '0' && cursor[1] == 'b')
			{
				cursor += 2;

				if (*cursor == '_')
				{
					Print("error: Binary literal cannot start with an underscore.\n");
					Fail();
				}

				if (!IsDigit(*cursor, Binary))
				{
					Print("error: Invalid binary digit: %\n", *cursor);
					Fail();
				}

				while (*cursor == '0') if (*++cursor == '_') cursor++;

				for (u32 i = 0; i < 64 && IsDigit(*cursor, Binary); i++)
				{
					value = (value << 1) | DigitToInt(*cursor++, Binary);
					if (*cursor == '_') ++cursor;
				}

				if (*cursor == '_')
				{
					Print("error: Only a single underscore is allowed.\n");
					Fail();
				}

				if (IsDigit(*cursor, Binary))
				{
					Print("error: Integer too large.\n");
					Fail();
				}
			}
			else
			{
				while (*cursor == '0') if (*++cursor == '_') cursor++;
				while (IsDigit(*cursor, Decimal))
				{
					u64 new_value = value * 10 + DigitToInt(*cursor++, Decimal);
					if (*cursor == '_') ++cursor;
					if (new_value < value)
					{
						Print("error: Integer too large.\n");
						Fail();
					}
					value = new_value;
				}

				if (*cursor == '_')
				{
					Print("error: Only a single underscore is allowed.\n");
					Fail();
				}

				if (*cursor == '.')
				{
					Print("error: Float literals not implemented yet...\n");
					Fail();

					while (IsDigit(*++cursor, Decimal))
					{
						u64 new_value = value * 10 + DigitToInt(*cursor, Decimal);
					}
				}
			}

			token.kind = TOKEN_INTEGER_LITERAL;
			token.info.integer.value = value;
			token.info.integer.is_unsigned = false;

			s32 min_bytes = value <= u8_max ? 1 : value <= u16_max ? 2 : value <= u32_max ? 4 : 8;

			s32 explicit_bytes = 0;
			if (cursor[0] == 'u' || cursor[0] == 's')
			{
				char* end_of_digits = cursor;
				token.info.integer.is_unsigned = *cursor++ == 'u';

				if (cursor[0] == '8')
				{
					explicit_bytes = 1;
					cursor += 1;
				}
				else if (cursor[0] == '1' && cursor[1] == '6')
				{
					explicit_bytes = 2;
					cursor += 2;
				}
				else if (cursor[0] == '3' && cursor[1] == '2')
				{
					explicit_bytes = 4;
					cursor += 2;
				}
				else if (cursor[0] == '6' && cursor[1] == '4')
				{
					explicit_bytes = 8;
					cursor += 2;
				}

				if (IsDigit(*cursor))
				{
					Print("error: Unexpected digit in integer size suffix: %\n", *cursor);
					Fail();
				}

				if (min_bytes > explicit_bytes && explicit_bytes)
				{
					Print("error: Integer '%' size specifier is too small. Suggestion: use %%\n", Span(start, cursor), Span(start, end_of_digits+1), min_bytes * 8);
					Fail();
				}
			}
			else if (cursor[-1] == '_')
			{
				Print("error: Dangling underscore at the end of integer literal.\n");
				Fail();
			}

			token.info.integer.explicit_bytes = explicit_bytes;

			if (IsLetter(*cursor) || IsDigit(*cursor) || *cursor == '_')
			{
				Print("error: Unexpected character after integer literal: %\n", *cursor);
				Fail();
			}
		}
		else if (*cursor == '"')
		{
			char* start = ++cursor;

			for (; cursor <= code.end && *cursor != '"'; cursor++)
			{
				if (*cursor == '\n')
				{
					lines.Add(Span(line_start, cursor));
					line++;
					line_start = cursor + 1;
				}
			}

			if (cursor >= code.end)
			{
				Print("error: String not completed before end of the file.\n");
				Fail();
			}

			char* end = cursor++;
			u64 size = end - start;
			token.kind = TOKEN_STRING_LITERAL;
			token.info.span = Span(start, end);
		}
		else if (*cursor == '(')
		{
			cursor += 1;
			token.kind = TOKEN_OPEN_PAREN;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == '{')
		{
			cursor += 1;
			token.kind = TOKEN_OPEN_BRACE;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == '[')
		{
			cursor += 1;
			token.kind = TOKEN_OPEN_BRACKET;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == ')')
		{
			cursor += 1;
			token.kind = TOKEN_CLOSE_PAREN;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing parenthesis %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != TOKEN_OPEN_PAREN)
			{
				Print("%:%: error: Missmatch between % and %\n", file_path, token.location, open, token);
				Fail();
			}

			u64 offset = tokens.count - prev_region_index;
			prev_region_index = open.info.next;
			open.info.next = offset;
		}
		else if (*cursor == '}')
		{
			cursor += 1;
			token.kind = TOKEN_CLOSE_BRACE;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing brace: %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != TOKEN_OPEN_BRACE)
			{
				Print("%:%: error: Missmatch between % and %\n", file_path, token.location, open, token);
				Fail();
			}

			u64 offset = tokens.count - prev_region_index;
			prev_region_index = open.info.next;
			open.info.next = offset;
		}
		else if (*cursor == ']')
		{
			cursor += 1;
			token.kind = TOKEN_CLOSE_BRACKET;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing bracket %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != TOKEN_OPEN_BRACKET)
			{
				Print("%:%: error: Missmatch between % and %\n", file_path, token.location, open, token);
				Fail();
			}

			u64 offset = tokens.count - prev_region_index;
			prev_region_index = open.info.next;
			open.info.next = offset;
		}
		else if (*cursor == '=' && cursor[1] == '>') cursor += 2, token.kind = TOKEN_FAT_ARROW;
		else if (*cursor == '!' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_NOT_EQUAL;
		else if (*cursor == '-' && cursor[1] == '>') cursor += 2, token.kind = TOKEN_ARROW;
		else if (*cursor == '<' && cursor[1] == '<') cursor += 2, token.kind = TOKEN_LEFT_SHIFT;
		else if (*cursor == '>' && cursor[1] == '>') cursor += 2, token.kind = TOKEN_RIGHT_SHIFT;
		else if (*cursor == '<' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_LESS_OR_EQUAL;
		else if (*cursor == '>' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_GREATER_OR_EQUAL;
		else if (*cursor == '+' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_PLUS_EQUAL;
		else if (*cursor == '-' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_MINUS_EQUAL;
		else if (*cursor == '*' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_TIMES_EQUAL;
		else if (*cursor == '/' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_DIVIDE_EQUAL;
		else if (*cursor == '^' && cursor[1] == '=') cursor += 2, token.kind = TOKEN_EXPONENTIAL_EQUAL;
		else if (*cursor == '.' && cursor[1] == '.') cursor += 2, token.kind = TOKEN_DOT_DOT;
		else if (*cursor == '.') cursor += 1, token.kind = TOKEN_DOT;
		else if (*cursor == ';') cursor += 1, token.kind = TOKEN_SEMICOLON;
		else if (*cursor == ':') cursor += 1, token.kind = TOKEN_COLON;
		else if (*cursor == ',') cursor += 1, token.kind = TOKEN_COMMA;
		else if (*cursor == '<') cursor += 1, token.kind = TOKEN_LESS;
		else if (*cursor == '>') cursor += 1, token.kind = TOKEN_GREATER;
		else if (*cursor == '+') cursor += 1, token.kind = TOKEN_PLUS;
		else if (*cursor == '-') cursor += 1, token.kind = TOKEN_MINUS;
		else if (*cursor == '*') cursor += 1, token.kind = TOKEN_ASTERISK;
		else if (*cursor == '/') cursor += 1, token.kind = TOKEN_DIVIDE;
		else if (*cursor == '^') cursor += 1, token.kind = TOKEN_EXPONENT;
		else if (*cursor == '&') cursor += 1, token.kind = TOKEN_AMPERSAND;
		else if (*cursor == '|') cursor += 1, token.kind = TOKEN_BAR;
		else if (*cursor == '?') cursor += 1, token.kind = TOKEN_QUESTION_MARK;
		else if (*cursor == '!') cursor += 1, token.kind = TOKEN_EXCLAMATION_MARK;
		else if (*cursor == '=') cursor += 1, token.kind = TOKEN_EQUAL;
		else
		{
			if (IsPrintable(*cursor))
			{
				Print("error: Invalid character: %\n", *cursor);
			}
			else
			{
				Print("error Invalid character: (int)%\n", (int)*cursor);
			}

			Fail();
		}

		tokens.Add(token);
	}

	Token token;
	token.kind = TOKEN_EOF;
	token.location.line = line;
	// token.location.offset = cursor - line_start;
	token.location.offset = 0;
	token.indent = 0;
	token.newline = true;

	tokens.Pad(8, token);

	if (prev_region_index != -1)
	{
		while (prev_region_index != -1)
		{
			Token& token = tokens[prev_region_index];
			Print("%:%: error: % not closed.\n", file_path, token.location, token);
			prev_region_index = token.info.next;
		}

		Fail();
	}

	Parse_Info info;
	info.file_path = file_path;
	info.tokens = tokens;
	info.code = code;
	info.lines = lines;

	return info;
}

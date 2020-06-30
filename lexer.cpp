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

			if      (CompareStrings(str, ToString(Token_Import)))  token.kind = Token_Import;
			else if (CompareStrings(str, ToString(Token_Struct)))  token.kind = Token_Struct;
			else if (CompareStrings(str, ToString(Token_Enum)))    token.kind = Token_Enum;
			else if (CompareStrings(str, ToString(Token_If)))      token.kind = Token_If;
			else if (CompareStrings(str, ToString(Token_Else)))    token.kind = Token_Else;
			else if (CompareStrings(str, ToString(Token_Then)))    token.kind = Token_Then;
			else if (CompareStrings(str, ToString(Token_Where)))   token.kind = Token_Where;
			else if (CompareStrings(str, ToString(Token_For)))     token.kind = Token_For;
			else if (CompareStrings(str, ToString(Token_While)))   token.kind = Token_While;
			else if (CompareStrings(str, ToString(Token_Return)))  token.kind = Token_Return;
			else if (CompareStrings(str, ToString(Token_Claim)))   token.kind = Token_Claim;
			else if (CompareStrings(str, ToString(Token_Defer)))   token.kind = Token_Defer;
			else if (CompareStrings(str, ToString(Token_Alias)))   token.kind = Token_Alias;
			else if (CompareStrings(str, ToString(Token_As)))      token.kind = Token_As;
			else if (CompareStrings(str, ToString(Token_In)))      token.kind = Token_In;
			else if (CompareStrings(str, ToString(Token_Null)))    token.kind = Token_Null;
			else if (CompareStrings(str, ToString(Token_True)))    token.kind = Token_True;
			else if (CompareStrings(str, ToString(Token_False)))   token.kind = Token_False;
			else if (CompareStrings(str, ToString(Token_Bool)))    token.kind = Token_Bool;
			else if (CompareStrings(str, ToString(Token_Int)))     token.kind = Token_Int;
			else if (CompareStrings(str, ToString(Token_Int8)))    token.kind = Token_Int8;
			else if (CompareStrings(str, ToString(Token_Int16)))   token.kind = Token_Int16;
			else if (CompareStrings(str, ToString(Token_Int32)))   token.kind = Token_Int32;
			else if (CompareStrings(str, ToString(Token_Int64)))   token.kind = Token_Int64;
			else if (CompareStrings(str, ToString(Token_Uint)))    token.kind = Token_Uint;
			else if (CompareStrings(str, ToString(Token_Uint8)))   token.kind = Token_Uint8;
			else if (CompareStrings(str, ToString(Token_Uint16)))  token.kind = Token_Uint16;
			else if (CompareStrings(str, ToString(Token_Uint32)))  token.kind = Token_Uint32;
			else if (CompareStrings(str, ToString(Token_Uint64)))  token.kind = Token_Uint64;
			else if (CompareStrings(str, ToString(Token_Float16))) token.kind = Token_Float16;
			else if (CompareStrings(str, ToString(Token_Float32))) token.kind = Token_Float32;
			else if (CompareStrings(str, ToString(Token_Float64))) token.kind = Token_Float64;
			else if (CompareStrings(str, ToString(Token_Or)))      token.kind = Token_Or;
			else if (CompareStrings(str, ToString(Token_And)))     token.kind = Token_And;
			else if (CompareStrings(str, ToString(Token_OR)))      token.kind = Token_OR;
			else if (CompareStrings(str, ToString(Token_AND)))     token.kind = Token_AND;
			else if (CompareStrings(str, ToString(Token_XOR)))     token.kind = Token_XOR;
			else if (CompareStrings(str, ToString(Token_NOT)))     token.kind = Token_NOT;
			else
			{
				if (cursor[-1] == '_')
				{
					Print("error: Identifiers cannot end with an underscore: '%'\n", str);
					Fail();
				}

				token.kind = Token_Identifier;
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

			token.kind = Token_IntegerLiteral;
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

				if (min_bytes > explicit_bytes)
				{
					Print("error: Integer '%' size specifier is too small. Suggestion: use %%%\n", Span(start, cursor), Span(start, end_of_digits+1), min_bytes * 8);
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
			token.kind = Token_StringLiteral;
			token.info.span = Span(start, end);
		}
		else if (*cursor == '(')
		{
			cursor += 1;
			token.kind = Token_OpenParen;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == '{')
		{
			cursor += 1;
			token.kind = Token_OpenBrace;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == '[')
		{
			cursor += 1;
			token.kind = Token_OpenBracket;
			token.info.next = prev_region_index;
			prev_region_index = tokens.count;
		}
		else if (*cursor == ')')
		{
			cursor += 1;
			token.kind = Token_CloseParen;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing parenthesis %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != Token_OpenParen)
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
			token.kind = Token_CloseBrace;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing brace: %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != Token_OpenBrace)
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
			token.kind = Token_CloseBracket;

			if (prev_region_index == -1)
			{
				Print("%:%: error: Unmatched closing bracket %\n", file_path, token.location, token);
				Fail();
			}

			Token& open = tokens[prev_region_index];
			if (open.kind != Token_OpenBracket)
			{
				Print("%:%: error: Missmatch between % and %\n", file_path, token.location, open, token);
				Fail();
			}

			u64 offset = tokens.count - prev_region_index;
			prev_region_index = open.info.next;
			open.info.next = offset;
		}
		else if (*cursor == '&' && cursor[1] == '&') cursor += 2, token.kind = Token_StrongAnd;
		else if (*cursor == '|' && cursor[1] == '|') cursor += 2, token.kind = Token_StrongOr;
		else if (*cursor == '=' && cursor[1] == '>') cursor += 2, token.kind = Token_FatArrow;
		else if (*cursor == '!' && cursor[1] == '=') cursor += 2, token.kind = Token_NotEqual;
		else if (*cursor == '-' && cursor[1] == '>') cursor += 2, token.kind = Token_Arrow;
		else if (*cursor == '<' && cursor[1] == '<') cursor += 2, token.kind = Token_LeftShift;
		else if (*cursor == '>' && cursor[1] == '>') cursor += 2, token.kind = Token_RightShift;
		else if (*cursor == '<' && cursor[1] == '=') cursor += 2, token.kind = Token_LessOrEqual;
		else if (*cursor == '>' && cursor[1] == '=') cursor += 2, token.kind = Token_GreaterOrEqual;
		else if (*cursor == '+' && cursor[1] == '=') cursor += 2, token.kind = Token_PlusEqual;
		else if (*cursor == '-' && cursor[1] == '=') cursor += 2, token.kind = Token_MinusEqual;
		else if (*cursor == '*' && cursor[1] == '=') cursor += 2, token.kind = Token_TimesEqual;
		else if (*cursor == '/' && cursor[1] == '=') cursor += 2, token.kind = Token_DivideEqual;
		else if (*cursor == '^' && cursor[1] == '=') cursor += 2, token.kind = Token_ExponentialEqual;
		else if (*cursor == '.' && cursor[1] == '.') cursor += 2, token.kind = Token_DotDot;
		else if (*cursor == '.') cursor += 1, token.kind = Token_Dot;
		else if (*cursor == ';') cursor += 1, token.kind = Token_SemiColon;
		else if (*cursor == ':') cursor += 1, token.kind = Token_Colon;
		else if (*cursor == ',') cursor += 1, token.kind = Token_Comma;
		else if (*cursor == '<') cursor += 1, token.kind = Token_Less;
		else if (*cursor == '>') cursor += 1, token.kind = Token_Greater;
		else if (*cursor == '+') cursor += 1, token.kind = Token_Plus;
		else if (*cursor == '-') cursor += 1, token.kind = Token_Minus;
		else if (*cursor == '*') cursor += 1, token.kind = Token_Asterisk;
		else if (*cursor == '/') cursor += 1, token.kind = Token_Divide;
		else if (*cursor == '^') cursor += 1, token.kind = Token_Exponent;
		else if (*cursor == '&') cursor += 1, token.kind = Token_Ampersand;
		else if (*cursor == '|') cursor += 1, token.kind = Token_Bar;
		else if (*cursor == '?') cursor += 1, token.kind = Token_QuestionMark;
		else if (*cursor == '!') cursor += 1, token.kind = Token_ExclamationMark;
		else if (*cursor == '=') cursor += 1, token.kind = Token_Equal;
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
	token.kind = Token_Eof;
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

#include "parser.h"
#include "token.h"
#include "print.h"
#include "memory.h"
#include "assert.h"
#include "pooled_array.h"

static bool IsSpecifier(Token_Kind kind)
{
	return kind == TOKEN_ASTERISK
		|| kind == TOKEN_QUESTION_MARK
		|| kind == TOKEN_OPEN_BRACKET;
}

static bool IsTernaryOperator(Token_Kind kind)
{
	return kind == TOKEN_IF;
}

static bool IsIdentifier(Token_Kind kind)
{
	return kind == TOKEN_IDENTIFIER_CASUAL
		|| kind == TOKEN_IDENTIFIER_FORMAL;
}

static bool IsBinaryOperator(Token_Kind kind)
{
	return kind == TOKEN_DOT
		|| kind == TOKEN_EXPONENT
		|| kind == TOKEN_ASTERISK
		|| kind == TOKEN_DIVIDE
		|| kind == TOKEN_PLUS
		|| kind == TOKEN_MINUS
		|| kind == TOKEN_AND
		|| kind == TOKEN_OR
		|| kind == TOKEN_MOD
		|| kind == TOKEN_BITWISE_AND
		|| kind == TOKEN_BITWISE_OR
		|| kind == TOKEN_BITWISE_XOR
		|| kind == TOKEN_LEFT_SHIFT
		|| kind == TOKEN_RIGHT_SHIFT
		|| kind == TOKEN_AS
		|| kind == TOKEN_EQUAL
		|| kind == TOKEN_NOT_EQUAL
		|| kind == TOKEN_LESS
		|| kind == TOKEN_LESS_OR_EQUAL
		|| kind == TOKEN_GREATER
		|| kind == TOKEN_GREATER_OR_EQUAL;
}

static bool IsUnaryOperator(Token_Kind kind)
{
	return kind == TOKEN_ASTERISK
		|| kind == TOKEN_AMPERSAND
		|| kind == TOKEN_PLUS
		|| kind == TOKEN_MINUS
		|| kind == TOKEN_BITWISE_NOT
		|| kind == TOKEN_NOT
		|| kind == TOKEN_EXCLAMATION_MARK;
}

static bool IsAssignment(Token_Kind kind)
{
	return kind == TOKEN_EQUAL
		|| kind == TOKEN_PLUS_EQUAL
		|| kind == TOKEN_MINUS_EQUAL
		|| kind == TOKEN_TIMES_EQUAL
		|| kind == TOKEN_DIVIDE_EQUAL
		|| kind == TOKEN_EXPONENTIAL_EQUAL;
}

static bool IsPrimitive(Token_Kind kind)
{
	return kind == TOKEN_BYTE
		|| kind == TOKEN_BOOL
		|| kind == TOKEN_INT
		|| kind == TOKEN_INT8
		|| kind == TOKEN_INT16
		|| kind == TOKEN_INT32
		|| kind == TOKEN_INT64
		|| kind == TOKEN_UINT
		|| kind == TOKEN_UINT8
		|| kind == TOKEN_UINT16
		|| kind == TOKEN_UINT32
		|| kind == TOKEN_UINT64
		|| kind == TOKEN_FLOAT16
		|| kind == TOKEN_FLOAT32
		|| kind == TOKEN_FLOAT64;
}

static bool IsTerm(Token_Kind kind)
{
	return kind == TOKEN_IDENTIFIER_CASUAL
		|| kind == TOKEN_IDENTIFIER_FORMAL
		|| kind == TOKEN_INTEGER_LITERAL
		|| kind == TOKEN_FLOAT_LITERAL
		|| kind == TOKEN_STRING_LITERAL
		|| kind == TOKEN_TRUE
		|| kind == TOKEN_FALSE
		|| kind == TOKEN_NULL;
}

static bool IsLiteral(Token_Kind kind)
{
	return kind == TOKEN_INTEGER_LITERAL
		|| kind == TOKEN_FLOAT_LITERAL
		|| kind == TOKEN_STRING_LITERAL
		|| kind == TOKEN_TRUE
		|| kind == TOKEN_FALSE
		|| kind == TOKEN_NULL;
}

static bool IsExpressionStarter(Token_Kind kind)
{
	return IsUnaryOperator(kind)
		|| IsTerm(kind)
		|| kind == TOKEN_OPEN_PAREN
		|| kind == TOKEN_OPEN_BRACE;
}

static u32 GetTernaryPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_IF:
			return 6;

		default:
			Assert("Invalid ternary operator.");
			Unreachable();
	}
}

static u32 GetBinaryPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_DOT:
			return 0;

		case TOKEN_EXPONENT:
			return 1;

		case TOKEN_ASTERISK:
		case TOKEN_DIVIDE:
		case TOKEN_MOD:
			return 3;

		case TOKEN_PLUS:
		case TOKEN_MINUS:
			return 4;

		case TOKEN_BITWISE_AND:
		case TOKEN_BITWISE_OR:
		case TOKEN_BITWISE_XOR:
		case TOKEN_LEFT_SHIFT:
		case TOKEN_RIGHT_SHIFT:
			return 5;

		case TOKEN_AS:
			return 6;

		case TOKEN_EQUAL:
		case TOKEN_NOT_EQUAL:
		case TOKEN_LESS:
		case TOKEN_LESS_OR_EQUAL:
		case TOKEN_GREATER:
		case TOKEN_GREATER_OR_EQUAL:
			return 7;

		case TOKEN_AND:
			return 8;

		case TOKEN_OR:
			return 9;

		default:
			Assert("Invalid binary operator.");
			// Unreachable();
			return -1;
	}
}

static u32 GetUnaryPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_ASTERISK:
		case TOKEN_AMPERSAND:
		case TOKEN_EXCLAMATION_MARK:
			return 1;
			// Make sure *a^n = (*a)^n

		case TOKEN_PLUS:  // I totally forgot why these are on 2 and not 1...
		case TOKEN_MINUS: //    it isn't because of exponentials...
		case TOKEN_BITWISE_NOT:
			return 2;

		case TOKEN_NOT:
			return 8;

		default:
			Assert("Invalid unary operator.");
			// Unreachable();
			return -1;
	}
}

static u32 GetPostfixPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_OPEN_PAREN:
		case TOKEN_OPEN_BRACKET:
			return 0;

		default:
			Assert("Invalid postfix operator.");
			// Unreachable();
			return -1;
	}
}

static bool IsOperatorRightToLeft(Token_Kind kind)
{
	return kind == TOKEN_EXPONENT;
}

static bool IsPostfixOperator(Token_Kind kind)
{
	return kind == TOKEN_OPEN_PAREN
		|| kind == TOKEN_OPEN_BRACKET;
}

static bool IsOperator(Token_Kind kind)
{
	return IsBinaryOperator(kind)
		|| IsPostfixOperator(kind)
		|| IsTernaryOperator(kind);
}

static u32 GetOperatorPrecedence(Token_Kind kind)
{
	if (IsBinaryOperator(kind))  return GetBinaryPrecedence(kind);
	if (IsTernaryOperator(kind)) return GetTernaryPrecedence(kind);
	if (IsPostfixOperator(kind)) return GetPostfixPrecedence(kind);
	Unreachable();
}

template<typename ...Args>
[[noreturn]]
static void Error(Parse_Info* info, SourceLocation where, String format, Args&&... message_args)
{
	u32 margin = 2;
	u32 start = where.line;
	u32 number_of_lines = 1 + margin;

	Print("%:%:%: error: ", info->file_path, (where.line+1), (where.offset+1));
	Print(format, message_args...);

	for (u32 line = start; line < start + number_of_lines && line < info->lines.count; line++)
	{
		Print("%\n", String(info->lines[line], info->lines[line].Length()));
	}

	Fail();
}

static bool CanTakeNextOp(Token* token, bool assignment_break, u32 parent_precedence)
{
	return !(!IsOperator(token->kind) || (token->kind == TOKEN_EQUAL && assignment_break)) && GetOperatorPrecedence(token->kind) < parent_precedence + IsOperatorRightToLeft(token->kind);
}

static bool IsOnCorrectScope(Token* token, u32 indent)
{
	return !token->newline || token->indent == indent;
}

static void CheckScope(Token* token, u32 indent, Parse_Info* info)
{
	if (!IsOnCorrectScope(token, indent))
	{
		Error(info, token->location, "Invalid indentation.\n");
	}
}

static void SkipSemiColon(Token*& token, u32 indent, Parse_Info* info)
{
	if (token->kind == TOKEN_SEMICOLON && IsOnCorrectScope(token, indent))
	{
		token += 1;
	}
}

static Ast_Type ParseType(Token*& token, u32 indent, Parse_Info* info);
static Ast_Expression* ParseExpression(Token*& token, u32 indent, Parse_Info* info, bool assignment_break = false, u32 parent_precedence = -2);
static Ast_Function ParseFunction(Token*& token, u32 indent, Parse_Info* info);
static Ast_Code ParseCode(Token*& token, u32 indent, Parse_Info* info);

static Ast_Struct ParseStruct(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Struct structure;
	ZeroMemory(&structure);
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
	{
		if (token->kind == TOKEN_IDENTIFIER_CASUAL)
		{
			Error(info, token->location, "Struct name must start with an uppercase letter.\n");
		}
		else
		{
			Error(info, token->location, "Struct name missing\n");
		}
	}

	CheckScope(token, indent, info);
	structure.name = token;
	token += 1;

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid struct declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, indent, info);
	token += 1;

	Pooled_Array<Ast_Struct_Member> members = NewPooledArray<Ast_Struct_Member>();

 	while (IsOnCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER_CASUAL)
		{
			Ast_Struct_Member member;
			member.name = token;
			member.index = members.count;
			token += 1;

			if (token->kind != TOKEN_COLON)
			{
				Error(info, token->location, "Expected ':', not: %\n", token);
			}

			CheckScope(token, indent+1, info);
			token += 1;

			member.type = ParseType(token, indent+2, info);
			members.Add(member);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Unexpected token: % after struct member.\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, indent+1, info);
				token += 1;
			}

			if (token->kind == TOKEN_SEMICOLON && IsOnCorrectScope(token, indent+1))
			{
				token += 1;
			}
		}
		else if (token->kind == TOKEN_IDENTIFIER_FORMAL)
		{
			Error(info, token->location, "Struct member names must start with a lowercase letter.\n");
		}
		else
		{
			Error(info, token->location, "Unexpected token in struct: %\n", token);
		}
	}

	structure.members = members.Lock();

	return structure;
}

static Ast_Enum ParseEnum(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Enum enumeration;
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
	{
		if (token->kind == TOKEN_IDENTIFIER_CASUAL)
		{
			Error(info, token->location, "Enum names must start with an uppercase letter.\n");
		}
		else
		{
			Error(info, token->location, "Enum name missing.\n");
		}
	}

	CheckScope(token, indent, info);
	enumeration.name = token;
	token += 1;

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid enum declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, indent, info);
	token += 1;

	Pooled_Array<Ast_Enum_Member> members = NewPooledArray<Ast_Enum_Member>();

	while (IsOnCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER_FORMAL)
		{
			Ast_Enum_Member member;
			member.name = token;
			member.index = members.count;
			token += 1;

			if (token->kind != TOKEN_EQUAL)
			{
				Error(info, token->location, "Expected '=', not: %\n", token);
			}

			CheckScope(token, indent+2, info);
			token += 1;

			if (token->kind == TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Expected expression before ';'\n");
			}

			member.expression = ParseExpression(token, indent+2, info);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Unexpected token: % after enum member.\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, indent+1, info);
				token += 1;
			}

			members.Add(member);
		}
		else if (token->kind == TOKEN_IDENTIFIER_CASUAL)
		{
			Error(info, token->location, "Enum member names must start with a uppercase letter.\n");
		}
		else
		{
			Error(info, token->location, "Unexpected token in enum: %\n", token);
		}
	}

	enumeration.members = members.Lock();

	return enumeration;
}

static Ast_Expression* ParseExpression(Token*& token, u32 indent, Parse_Info* info, bool assignment_break, u32 parent_precedence)
{
	Ast_Expression* left;

	if (IsUnaryOperator(token->kind))
	{
		Ast_Expression_Unary* unary = info->stack.Allocate<Ast_Expression_Unary>();

		switch (token->kind)
		{
			case TOKEN_ASTERISK:         unary->kind = AST_EXPRESSION_UNARY_REFERENCE_OF; break;
			case TOKEN_AMPERSAND:        unary->kind = AST_EXPRESSION_UNARY_ADDRESS_OF;   break;
			case TOKEN_BITWISE_NOT:      unary->kind = AST_EXPRESSION_UNARY_BITWISE_NOT;  break;
			case TOKEN_NOT:              unary->kind = AST_EXPRESSION_UNARY_NOT;          break;
			case TOKEN_MINUS:            unary->kind = AST_EXPRESSION_UNARY_MINUS;        break;
			case TOKEN_PLUS:             unary->kind = AST_EXPRESSION_UNARY_PLUS;         break;
			case TOKEN_EXCLAMATION_MARK: unary->kind = AST_EXPRESSION_UNARY_NOT;          break;
			default: Assert();
		}

		unary->span.begin = token;
		unary->op = token++;
		CheckScope(token, indent, info);
		unary->subexpression = ParseExpression(token, indent, info, assignment_break, GetUnaryPrecedence(unary->op->kind));
		unary->span.end = token;
		left = unary;
	}
	else if (IsLiteral(token->kind))
	{
		Ast_Expression_Literal* literal = info->stack.Allocate<Ast_Expression_Literal>();
		literal->kind  = AST_EXPRESSION_TERMINAL_LITERAL;
		literal->can_constantly_evaluate = true;
		literal->is_pure = true;
		literal->span.begin = token;
		literal->token = token++;
		literal->span.end = token;
		left = literal;
	}
	else if (IsIdentifier(token->kind))
	{
		Ast_Expression_Terminal* term = info->stack.Allocate<Ast_Expression_Terminal>();
		term->kind = AST_EXPRESSION_TERMINAL_NAME;
		term->span.begin = token;
		term->token = token++;
		term->span.end = token;
		left = term;
	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		Token* closure = token->GetClosure();

		Ast_Expression_Tuple* tuple = info->stack.Allocate<Ast_Expression_Tuple>();
		ZeroMemory(tuple);

		tuple->kind  = AST_EXPRESSION_TUPLE;
		tuple->span.begin = token;
		tuple->span.end = closure+1;

		token += 1;

		Pooled_Array<Ast_Expression*> elements = NewPooledArray<Ast_Expression*>();

		if (closure[-1].kind == TOKEN_COMMA)
		{
			Error(info, token->location, "Expected expression after ','\n");
		}

		while (token < closure)
		{
			CheckScope(token, indent+1, info);
			elements.Add(ParseExpression(token, indent+1, info));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, info);
				token += 1;
			}
			else if (token < closure)
			{
				Error(info, token->location, "Invalid expression, unexpected token: %\n", token);
			}
		}

		tuple->elements = elements.Lock();

		CheckScope(token, indent, info);
		token = closure+1;
		left = tuple;
	}
	else if (token->kind == TOKEN_OPEN_BRACE)
	{
		Ast_Expression_Fixed_Array* fixed_array = info->stack.Allocate<Ast_Expression_Fixed_Array>();
		ZeroMemory(fixed_array);

		fixed_array->kind = AST_EXPRESSION_FIXED_ARRAY;
		fixed_array->span = Span(token, token->GetClosure());

		Token* closure = token->GetClosure();
		Pooled_Array<Ast_Expression*> elements = NewPooledArray<Ast_Expression*>();

		if (token+1 == closure)
		{
			Error(info, token->location, "Empty arrays literals aren't allowed.\n");
		}

		token += 1;

		while (token < closure)
		{
			CheckScope(token, indent+1, info);
			Ast_Expression* expression = ParseExpression(token, indent+1, info, false);
			elements.Add(expression);
			CheckScope(token, indent, info);

			if (token->kind == TOKEN_COMMA)
			{
				token += 1;

				if (token == closure)
				{
					Error(info, token->location, "Expected expression after ',', not: '%'\n", token);
				}
			}
		}

		fixed_array->elements = elements.Lock();

		token = closure + 1;
		left = fixed_array;
	}
	else
	{
		if (token->indent != indent)
		{
			CheckScope(token, indent, info);
		}

		Error(info, token->location, "Invalid expression, expected term, got: %\n", token);
	}

	while (CanTakeNextOp(token, assignment_break, parent_precedence) && IsOnCorrectScope(token, indent))
	{
		// @Indent does unary operators need to be treated differently? (Error check)
		if (token->kind == TOKEN_IF)
		{
			Ast_Expression_Ternary* if_else = info->stack.Allocate<Ast_Expression_Ternary>();
			ZeroMemory(if_else);

			if_else->kind   = AST_EXPRESSION_IF_ELSE;
			if_else->span.begin = left->span.begin;
			if_else->ops[0] = token++;
			if_else->left   = left;
			CheckScope(token, indent, info);
			if_else->middle = ParseExpression(token, indent, info, false);

			if (token->kind != TOKEN_ELSE)
			{
				Error(info, token->location, "Invalid 'if' expression, missing 'else' clause. Unexpected: %\n", token);
			}

			CheckScope(token, indent, info);

			if_else->ops[1] = token++;
			CheckScope(token, indent, info);
			if_else->right = ParseExpression(token, indent, info, assignment_break, GetTernaryPrecedence(TOKEN_IF));
			if_else->span.end = token;
			left = if_else;
		}
		else if (token->kind == TOKEN_AS)
		{
			Ast_Expression_As* as = info->stack.Allocate<Ast_Expression_As>();
			ZeroMemory(as);

			CheckScope(token, indent, info);
			as->op = token++;

			as->kind = AST_EXPRESSION_AS;
			as->span.begin = left->span.begin;
			as->ast_type = ParseType(token, indent, info);
			as->span.end = token;
			as->expression = left;

			left = as;
		}
		else if (token->kind == TOKEN_OPEN_PAREN)
		{
			Ast_Expression_Call* call = info->stack.Allocate<Ast_Expression_Call>();
			ZeroMemory(call);

			Token* open = token;
			Token* closure = open->GetClosure();
			List<Ast_Expression*> arguments = null;

			CheckScope(token, indent, info);
			call->parameters = (Ast_Expression_Tuple*)ParseExpression(token, indent, info, false, GetOperatorPrecedence(token->kind));

			token = closure + 1;

			call->kind  = AST_EXPRESSION_CALL;
			call->span.begin = left->span.begin;
			call->span.end = token;
			// call->token = open;
			call->function = left;
			left = call;
		}
		else if (token->kind == TOKEN_OPEN_BRACKET)
		{
			Token* open = token++;
			Token* closure = open->GetClosure();

			Ast_Expression_Subscript* subscript = info->stack.Allocate<Ast_Expression_Subscript>();
			ZeroMemory(subscript);

			subscript->kind  = AST_EXPRESSION_SUBSCRIPT;
			subscript->span.begin = left->span.begin;
			subscript->span.end = closure+1;
			// subscript->token = open;
			subscript->array = left;

			if (token != closure)
			{
				CheckScope(token, indent+1, info);
				subscript->index = ParseExpression(token, indent+1, info);

				if (token != closure)
				{
					Error(info, token->location, "Expected ']', not: %\n", token);
				}
			}
			else subscript->index = null;

			CheckScope(token, indent, info);
			token = closure + 1;

			left = subscript;
		}
		else
		{
			// @Indent I think the CheckScope needs to be here instead of at the start of ParseExpression (where we consume the term).
			Ast_Expression_Binary* binary = info->stack.Allocate<Ast_Expression_Binary>();
			ZeroMemory(binary);

			switch (token->kind)
			{
				case TOKEN_EQUAL:            binary->kind = AST_EXPRESSION_BINARY_COMPARE_EQUAL;            break;
				case TOKEN_NOT_EQUAL:        binary->kind = AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL;        break;
				case TOKEN_LESS:             binary->kind = AST_EXPRESSION_BINARY_COMPARE_LESS;             break;
				case TOKEN_LESS_OR_EQUAL:    binary->kind = AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL;    break;
				case TOKEN_GREATER:          binary->kind = AST_EXPRESSION_BINARY_COMPARE_GREATER;          break;
				case TOKEN_GREATER_OR_EQUAL: binary->kind = AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL; break;
				case TOKEN_AND:              binary->kind = AST_EXPRESSION_BINARY_AND;                      break;
				case TOKEN_OR:               binary->kind = AST_EXPRESSION_BINARY_OR;                       break;
				case TOKEN_DOT:              binary->kind = AST_EXPRESSION_BINARY_DOT;                      break;
				case TOKEN_PLUS:             binary->kind = AST_EXPRESSION_BINARY_ADD;                      break;
				case TOKEN_MINUS:            binary->kind = AST_EXPRESSION_BINARY_SUBTRACT;                 break;
				case TOKEN_ASTERISK:         binary->kind = AST_EXPRESSION_BINARY_MULTIPLY;                 break;
				case TOKEN_DIVIDE:           binary->kind = AST_EXPRESSION_BINARY_DIVIDE;                   break;
				case TOKEN_MOD:              binary->kind = AST_EXPRESSION_BINARY_MODULO;                   break;
				case TOKEN_EXPONENT:         binary->kind = AST_EXPRESSION_BINARY_EXPONENTIAL;              break;
				case TOKEN_BITWISE_OR:       binary->kind = AST_EXPRESSION_BINARY_BITWISE_OR;               break;
				case TOKEN_BITWISE_XOR:      binary->kind = AST_EXPRESSION_BINARY_BITWISE_XOR;              break;
				case TOKEN_BITWISE_AND:      binary->kind = AST_EXPRESSION_BINARY_BITWISE_AND;              break;
				case TOKEN_LEFT_SHIFT:       binary->kind = AST_EXPRESSION_BINARY_LEFT_SHIFT;               break;
				case TOKEN_RIGHT_SHIFT:      binary->kind = AST_EXPRESSION_BINARY_RIGHT_SHIFT;              break;
				default: Unreachable();
			}

			binary->span.begin = left->span.begin;
			binary->op = token++;
			binary->left  = left;
			CheckScope(token, indent, info);
			binary->right = ParseExpression(token, indent, info, assignment_break, GetBinaryPrecedence(binary->op->kind));
			binary->span.end = token;
			left = binary;
		}
	}

	return left;
}

static Ast_Type ParseType(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Type type;

	Pooled_Array<Ast_Specifier> specifiers = NewPooledArray<Ast_Specifier>();

	while (IsSpecifier(token->kind))
	{
		Ast_Specifier specifier;
		specifier.token = token;
		specifier.size_expression = null;
		CheckScope(token, indent, info);

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			specifier.kind = AST_SPECIFIER_ARRAY;
			Token* closure = token->GetClosure();
			CheckScope(closure, indent, info);
			token += 1;

			if (token != closure)
			{
				specifier.size_expression = ParseExpression(token, indent+1, info);
			}

			token = closure + 1;
		}
		else if (token->kind == TOKEN_ASTERISK)
		{
			specifier.kind = AST_SPECIFIER_POINTER;
			token += 1;
		}
		else if (token->kind == TOKEN_QUESTION_MARK)
		{
			specifier.kind = AST_SPECIFIER_OPTIONAL;
			token += 1;
		}

		specifiers.Add(specifier);
	}

	type.specifiers = specifiers.Lock();
	type.basetype.token = token;

	if (IsPrimitive(token->kind))
	{
		CheckScope(token, indent, info);
		type.basetype.kind = AST_BASETYPE_PRIMITIVE;
		token += 1;
	}
	else if (token->kind == TOKEN_IDENTIFIER_FORMAL)
	{
		CheckScope(token, indent, info);
		type.basetype.kind = AST_BASETYPE_USERTYPE;
		token += 1;
	}
	else if (token->kind == TOKEN_OPEN_PAREN && token->GetClosure()[1].kind == TOKEN_ARROW)
	{
		Token* closure = token->GetClosure();
		CheckScope(token, indent, info);
		CheckScope(closure, indent, info);
		CheckScope(closure+1, indent, info);
		type.basetype.kind = AST_BASETYPE_FUNCTION;
		type.basetype.function.input = null;
		type.basetype.function.output = null;
		token += 1;

		List<Ast_Type> params = null;

		while (token != closure)
		{
			Ast_Type param = ParseType(token, indent+1, info);
			params.Add(param);

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, info);
				token += 1;

				if (token == closure)
				{
					Error(info, token->location, "Expected type after ','\n");
				}
			}
		}

		token = closure+2;

		if (params.count == 0)
		{
			type.basetype.function.input = null;
		}
		else
		{
			type.basetype.function.input = info->stack.Allocate<Ast_Type>();
			ZeroMemory(type.basetype.function.input);
			type.basetype.function.input->basetype.kind = AST_BASETYPE_TUPLE;
			type.basetype.function.input->basetype.tuple = params.ToArray();
		}

		if (token->kind == TOKEN_OPEN_PAREN && token[1].kind == TOKEN_CLOSE_PAREN && token->GetClosure()[1].kind != TOKEN_ARROW)
		{
			type.basetype.function.output = null;
			token = token->GetClosure()+1;
		}
		else
		{
			type.basetype.function.output = info->stack.Allocate<Ast_Type>();
			*type.basetype.function.output = ParseType(token, indent, info);
		}

	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		Token* closure = token->GetClosure();
		CheckScope(token, indent, info);
		CheckScope(closure, indent, info);
		type.basetype.kind = AST_BASETYPE_TUPLE;
		type.basetype.tuple = null;
		token += 1;

		List<Ast_Type> members = null;

		while (token != closure)
		{
			members.Add(ParseType(token, indent+1, info));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, info);
				token += 1;

				if (token == closure)
				{
					Error(info, token->location, "Expected type after ','\n");
				}
			}
		}

		type.basetype.tuple = members.ToArray();

		token = closure+1;
	}
	else
	{
		Error(info, token->location, "Expected type, not: %\n", token);
	}

	return type;
}

static void ParseParameters(Ast_Function* function, Token* open_paren, u32 indent, Parse_Info* info)
{
	Token* closure = open_paren->GetClosure();
	Token* token = open_paren+1;
	Pooled_Array<Ast_VariableDeclaration> params = NewPooledArray<Ast_VariableDeclaration>();

	CheckScope(open_paren, indent, info);
	CheckScope(closure, indent, info);

	while (token < closure)
	{
		Ast_VariableDeclaration param;
		ZeroMemory(&param);
		param.type = null;
		param.explicit_type = null;
		param.is_parameter = true;

		if (token->kind == TOKEN_COMMA)
		{
			Error(info, token->location, "Empty parameters not allowed. (Remove redundant ',')\n");
		}

		if (token->kind != TOKEN_IDENTIFIER_CASUAL)
		{
			if (token->kind == TOKEN_IDENTIFIER_FORMAL)
			{
				Error(info, token->location, "Parameter names must start with a lowercase letter, not: %\n", token);
			}
			else
			{
				Error(info, token->location, "Parameter name missing, unexpected: %\n", token);
			}
		}

		CheckScope(token, indent+1, info);
		param.name = token++;

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Parameter type missing.\n");
		}

		CheckScope(token, indent+1, info);
		token += 1;

		param.explicit_type = info->stack.Allocate<Ast_Type>();
		*param.explicit_type = ParseType(token, indent+2, info);

		params.Add(param);

		if (token->kind != TOKEN_COMMA && token != closure)
		{
			Error(info, token->location, "Expected ',' or ')', not: %\n", token);
		}

		if (token->kind == TOKEN_COMMA)
		{
			CheckScope(token, indent, info);
			token += 1;
		}
	}

	function->parameters = params.Lock();
}

static Ast_BranchBlock ParseBranchBlock(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_BranchBlock branch_block;
	ZeroMemory(&branch_block);
	Pooled_Array<Ast_Branch> branches = NewPooledArray<Ast_Branch>();

	do
	{
		Ast_Branch branch;
		ZeroMemory(&branch);

		Token* clause_token = token;

		if (token->kind == TOKEN_ELSE)
		{
			branch.clause = AST_BRANCH_CLAUSE_ELSE;
			token += 1;
		}
		else if (token->kind == TOKEN_THEN)
		{
			branch.clause = AST_BRANCH_CLAUSE_THEN;
			token += 1;
		}
		else
		{
			branch.clause = AST_BRANCH_CLAUSE_INIT;
		}

		switch (token->kind)
		{
			case TOKEN_IF:
			{
				branch.kind = AST_BRANCH_IF;
				CheckScope(token, indent, info);
				branch.token = token;
				token += 1;
				branch.condition = ParseExpression(token, indent+1, info, false);
			} break;

			case TOKEN_WHILE:
			{
				branch.kind = AST_BRANCH_WHILE;
				CheckScope(token, indent, info);
				branch.token = token;
				token += 1;
				branch.condition = ParseExpression(token, indent+1, info, false);
			} break;

			case TOKEN_FOR:
			{
				CheckScope(token, indent, info);
				branch.token = token;
				token += 1;

				if (token[0].kind == TOKEN_IDENTIFIER_FORMAL && token[1].kind == TOKEN_IN)
				{
					Error(info, token->location, "Iterator names must start with a lowercase letter.\n");
				}

				if (token[0].kind == TOKEN_IDENTIFIER_CASUAL && token[1].kind == TOKEN_IN)
				{
					branch.kind = AST_BRANCH_FOR_IN;

					CheckScope(token, indent+1, info);
					CheckScope(token+1, indent, info);

					Ast_VariableDeclaration* iterator = info->stack.Allocate<Ast_VariableDeclaration>();
					ZeroMemory(iterator);
					iterator->name = token;
					iterator->is_iterator = true;
					iterator->is_global = false;
					branch.iterator = iterator;

					token += 2;

					branch.range = ParseExpression(token, indent+1, info, false);

					if (token->kind == TOKEN_WHERE)
					{
						CheckScope(token, indent, info);
						token += 1;
						branch.kind = AST_BRANCH_FOR_IN_WHERE;
						branch.condition = ParseExpression(token, indent+1, info, false);
					}
				}
				else
				{
					branch.range = ParseExpression(token, indent+1, info, false);
					branch.kind = AST_BRANCH_FOR;
				}
			} break;

			case TOKEN_COLON: branch.kind = AST_BRANCH_NAKED; break;

			default: Error(info, token->location, "Expected 'if', 'while', 'for' or ':' after %, not: %\n", clause_token, token);
		}

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Expected ':' after branch, not: %\n", token);
		}

		CheckScope(token, indent, info);

		token += 1;

		branch.code = ParseCode(token, indent+1, info);
		branches.Add(branch);
	} while ((token->kind == TOKEN_ELSE || token->kind == TOKEN_THEN) && IsOnCorrectScope(token, indent));

	branch_block.branches = branches.Lock();

	Ast_Branch* else_branch = null;
	Ast_Branch* then_branch = null;

	for (Ast_Branch* branch = branch_block.branches.End()-1; branch >= branch_block.branches.Begin(); branch--)
	{
		if (branch->kind != AST_BRANCH_NAKED)
		{
			branch->else_branch = else_branch;
			branch->then_branch = then_branch;
			if (else_branch) else_branch->user_count++;
			if (then_branch) then_branch->user_count++;
		}

		if (branch->clause == AST_BRANCH_CLAUSE_ELSE)
		{
			else_branch = branch;
		}
		else if (branch->clause == AST_BRANCH_CLAUSE_THEN)
		{
			then_branch = branch;
		}
	}

	return branch_block;
}

static Ast_Statement ParseStatement(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Statement statement;
	ZeroMemory(&statement);

	if (token[0].kind == TOKEN_IDENTIFIER_FORMAL && token[1].kind == TOKEN_COLON)
	{
		Error(info, token->location, "Variable names must start with a lowercase letter.\n");
	}

	if (token[0].kind == TOKEN_IDENTIFIER_CASUAL && token[1].kind == TOKEN_COLON)
	{
		CheckScope(token+1, indent, info);

		statement.kind = AST_STATEMENT_VARIABLE_DECLARATION;
		statement.variable_declaration.name = token;
		statement.variable_declaration.type = null;
		statement.variable_declaration.assignment = null;
		statement.variable_declaration.explicit_type = null;
		statement.variable_declaration.is_parameter = false;
		statement.variable_declaration.is_global = false;

		token += 2;

		if (token->kind != TOKEN_EQUAL)
		{
			CheckScope(token, indent+1, info);
			statement.variable_declaration.explicit_type = info->stack.Allocate<Ast_Type>();
			*statement.variable_declaration.explicit_type = ParseType(token, indent+1, info);
		}

		if (token->kind == TOKEN_EQUAL)
		{
			CheckScope(token, indent+1, info);
			token += 1;
			statement.variable_declaration.assignment = ParseExpression(token, indent+1, info);
		}

		SkipSemiColon(token, indent, info);
		return statement;
	}
	else if (IsExpressionStarter(token->kind))
	{
		Ast_Expression* expression = ParseExpression(token, indent+1, info, true);

		if (IsAssignment(token->kind))
		{
			if      (token->kind == TOKEN_EQUAL)             statement.kind = AST_STATEMENT_ASSIGNMENT;
			else if (token->kind == TOKEN_PLUS_EQUAL)        statement.kind = AST_STATEMENT_ASSIGNMENT_ADD;
			else if (token->kind == TOKEN_MINUS_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_SUBTRACT;
			else if (token->kind == TOKEN_TIMES_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_MULTIPLY;
			else if (token->kind == TOKEN_DIVIDE_EQUAL)      statement.kind = AST_STATEMENT_ASSIGNMENT_DIVIDE;
			else if (token->kind == TOKEN_EXPONENTIAL_EQUAL) statement.kind = AST_STATEMENT_ASSIGNMENT_POWER;

			CheckScope(token, indent+1, info);
			token += 1;
			CheckScope(token, indent+1, info);

			statement.assignment.left  = expression;
			statement.assignment.token = token;
			statement.assignment.right = ParseExpression(token, indent+1, info, false);
		}
		else
		{
			statement.kind = AST_STATEMENT_EXPRESSION;
			statement.expression = expression;
		}

		if (token->kind != TOKEN_SEMICOLON && IsOnCorrectScope(token, indent+1))
		{
			Error(info, token->location, "Invalid expression-statement, unexpected token: %, expected assignment operator or ';'.\n", token);
		}

		SkipSemiColon(token, indent, info);
		return statement;
	}
	else if (token->kind == TOKEN_IF || token->kind == TOKEN_FOR || token->kind == TOKEN_WHILE)
	{
		Ast_BranchBlock branch_block = ParseBranchBlock(token, indent, info);
		statement.kind = AST_STATEMENT_BRANCH_BLOCK;
		statement.branch_block = branch_block;
		return statement;
	}
	else if (token->kind == TOKEN_INC || token->kind == TOKEN_DEC)
	{
		statement.kind = token->kind == TOKEN_INC ? AST_STATEMENT_INCREMENT : AST_STATEMENT_DECREMENT;
		statement.increment.token = token++;

		if (!IsOnCorrectScope(token, indent+1) || token->kind == TOKEN_SEMICOLON)
		{
			Error(info, statement.increment.token->location, "Expected expression after % keyword\n", statement.increment.token);
		}

		statement.increment.expression = ParseExpression(token, indent+1, info);

		return statement;
	}
	else if (token->kind == TOKEN_DEFER)
	{
		statement.kind = AST_STATEMENT_DEFER;
		statement.defer.token = token++;

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Invalid 'defer' statement, Expected ':', not: %\n", token);
		}

		CheckScope(token, indent, info);
		token += 1;

		Ast_Code code = ParseCode(token, indent+1, info);
		statement.defer.code = code;

		return statement;
	}
	else if (token->kind == TOKEN_BREAK)
	{
		statement.kind = AST_STATEMENT_BREAK;
		statement.brk.token = token++;
		return statement;
	}
	else if (token->kind == TOKEN_RETURN)
	{
		statement.kind = AST_STATEMENT_RETURN;
		statement.ret.token = token++;
		statement.ret.expression = null;

		if (IsOnCorrectScope(token, indent+1))
		{
			statement.ret.expression = ParseExpression(token, indent+1, info, false);
		}

		return statement;
	}
	else if (token->kind == TOKEN_CLAIM)
	{
		statement.kind = AST_STATEMENT_CLAIM;
		statement.claim.token = token++;
		statement.claim.expression = ParseExpression(token, indent+1, info, false);

		return statement;
	}
	else if (token->kind == TOKEN_SEMICOLON)
	{
		Error(info, token->location, "Expected statement before ';'\n");
	}
	else
	{
		Error(info, token->location, "Invalid statement starting with %\n", token);
	}
}

static Ast_Attribute ParseAttribute(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Attribute attribute;
	Token* closure = token->GetClosure();
	CheckScope(token, indent, info);
	CheckScope(closure, indent, info);
	attribute.expression = ParseExpression(token, indent+1, info);
	token = closure+1;
	return attribute;
}

static Ast_Code ParseCode(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Code code;
	ZeroMemory(&code);

	Pooled_Array<Ast_Statement> statements = NewPooledArray<Ast_Statement>();
	Pooled_Array<Ast_Struct>    structs    = NewPooledArray<Ast_Struct>();
	Pooled_Array<Ast_Enum>      enums      = NewPooledArray<Ast_Enum>();
	Pooled_Array<Ast_Function>  functions  = NewPooledArray<Ast_Function>();

	while (IsOnCorrectScope(token, indent))
	{
		Ast_Attribute attribute;
		bool has_attribute = false;
		ZeroMemory(&attribute);

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			attribute = ParseAttribute(token, indent, info);
			has_attribute = true;
		}

		if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStruct(token, indent, info);
			structure.attribute = attribute;
			structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, indent, info);
			enumeration.attribute = attribute;
			enums.Add(enumeration);
		}
		else if (IsIdentifier(token->kind) && token[1].kind == TOKEN_OPEN_PAREN
			&&  (token[1].GetClosure()[1].kind == TOKEN_COLON || token[1].GetClosure()[1].kind == TOKEN_ARROW))
		{
			if (token->kind != TOKEN_IDENTIFIER_FORMAL)
			{
				Error(info, token->location, "Function names must start with an uppercase letter.\n", token);
			}

			Ast_Function function = ParseFunction(token, indent, info);
			function.attribute = attribute;
			function.is_global = false;
			functions.Add(function);
		}
		else
		{
			Ast_Statement statement = ParseStatement(token, indent, info);
			statements.Add(statement);

			if (token->kind != TOKEN_SEMICOLON && !token->newline)
			{
				Error(info, token->location, "Expected ';' at the end of statement. not: %\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				if (IsOnCorrectScope(token, indent))
				{
					token += 1;
				}
				else if (token->newline && token->indent > indent)
				{
					Error(info, token->location, "';' on wrong indentation.\n");
				}
			}
		}

		if (token->newline && token->indent < indent)
		{
			break;
		}
	}

	code.statements = statements.Lock();
	code.scope.enums = enums.Lock();
	code.scope.structs = structs.Lock();
	code.scope.functions = functions.Lock();

	return code;
}

static Ast_Function ParseFunction(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Function function;
	ZeroMemory(&function);
	function.name = token->info.string;
	function.name_token = token++;
	ParseParameters(&function, token, indent, info);
	token = token->GetClosure()+1;

	if (token->kind != TOKEN_ARROW && token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected '->' or ':', not %\n", token);
	}

	if (token->kind == TOKEN_ARROW)
	{
		CheckScope(token, indent+1, info);
		token += 1;
		function.does_have_return_type_appendage = true;
		function.ast_return_type = info->stack.Allocate<Ast_Type>();
		*function.ast_return_type = ParseType(token, indent, info);
	}

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected ':', not %\n", token);
	}

	CheckScope(token, indent, info);

	token += 1;
	function.code = ParseCode(token, indent+1, info);

	return function;
}

static Ast_Import ParseImport(Token*& token, Parse_Info* info)
{
	Ast_Import import;
	import.token = token;
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
	{
		Error(info, token->location, "Expected identifier after import token, instead got: %\n", token);
	}

	CheckScope(token, 1, info);
	import.module = token;
	token += 1;

	if (token->kind == TOKEN_SEMICOLON)
	{
		CheckScope(token, 1, info);
		token += 1;
	}
	else if (IsOnCorrectScope(token, 1))
	{
		Error(info, token->location, "Unexpected token after import statement: %\n", token);
	}

	CheckScope(token, 0, info);

	return import;
}

static void ParseGlobalScope(Ast_Root* root, Token* token, Parse_Info* info)
{
	Pooled_Array<Ast_Import>    imports    = NewPooledArray<Ast_Import>();
	Pooled_Array<Ast_Struct>    structs    = NewPooledArray<Ast_Struct>();
	Pooled_Array<Ast_Enum>      enums      = NewPooledArray<Ast_Enum>();
	Pooled_Array<Ast_Function>  functions  = NewPooledArray<Ast_Function>();

	while (token->kind != TOKEN_EOF)
	{
		Ast_Attribute attribute;
		ZeroMemory(&attribute);
		bool has_attribute = false;

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			attribute = ParseAttribute(token, 0, info);
			has_attribute = true;
		}

		if (token->kind == TOKEN_IMPORT)
		{
			Ast_Import import = ParseImport(token, info);
			imports.Add(import);
		}
		else if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStruct(token, 0, info);
			structure.attribute = attribute;
			structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, 0, info);
			enumeration.attribute = attribute;
			enums.Add(enumeration);
		}
		else if (IsIdentifier(token->kind) && token[1].kind == TOKEN_OPEN_PAREN)
		{
			if (token->kind != TOKEN_IDENTIFIER_FORMAL)
			{
				Error(info, token->location, "Function names must start with an uppercase letter.\n", token);
			}

			Ast_Function function = ParseFunction(token, 0, info);
			function.attribute = attribute;
			function.is_global = true;
			functions.Add(function);
		}
		else
		{
			Error(info, token->location, "Unexpected token in global scope: %\n", token);
		}
	}

	root->imports = imports.Lock();
	root->scope.functions = functions.Lock();
	root->scope.structs = structs.Lock();
	root->scope.enums = enums.Lock();
}

Parse_Info ParseFile(String file_path)
{
	Parse_Info info;
	ZeroMemory(&info);
	info.stack = NewStackAllocator();
	LexicalParse(file_path, &info);
	info.ast_root = info.stack.Allocate<Ast_Root>();
	ZeroMemory(info.ast_root);
	InitIntrinsicFunctions(&info);
	ParseGlobalScope(info.ast_root, info.tokens, &info);
	SemanticParse(&info);
	return info;
}


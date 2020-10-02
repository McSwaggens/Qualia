#include "parser.h"
#include "token.h"
#include "print.h"
#include "memory.h"
#include "assert.h"

void Write(OutputBuffer* buffer, Type* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	if (type->kind == TYPE_SPECIFIER_POINTER)
	{
		Write(buffer, "*");
		Write(buffer, type->subtype);
	}
	else if (type->kind == TYPE_SPECIFIER_OPTIONAL)
	{
		Write(buffer, "?");
		Write(buffer, type->subtype);
	}
	else if (type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
	{
		Write(buffer, '[');
		Write(buffer, type->length);
		Write(buffer, ']');
		Write(buffer, type->subtype);
	}
	else if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY)
	{
		Write(buffer, "[]");
		Write(buffer, type->subtype);
	}
	else if (type->kind == TYPE_BASETYPE_PRIMITIVE)
	{
		Write(buffer, type->primitive);
	}
	else if (type->kind == TYPE_BASETYPE_FUNCTION)
	{
		Write(buffer, type->function.input);
		Write(buffer, " -> ");
		Write(buffer, type->function.output);
	}
	else if (type->kind == TYPE_BASETYPE_TUPLE)
	{
		Write(buffer, '(');
		for (u32 i = 0; i < type->tuple.count; i++)
		{
			if (i) Write(buffer, ", ");
			Write(buffer, type->tuple[i]);
		}
		Write(buffer, ')');
	}
	else if (type->kind == TYPE_BASETYPE_STRUCT)
	{
		Write(buffer, type->structure->name);
	}
	else if (type->kind == TYPE_BASETYPE_ENUM)
	{
		Write(buffer, type->enumeration->name);
	}
}

void Write(OutputBuffer* buffer, Ast_Type* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	Write(buffer, *type);
}

void Write(OutputBuffer* buffer, Ast_Type type)
{
	for (Ast_Specifier* specifier = type.specifiers; specifier < type.specifiers.End(); specifier++)
	{
		if (specifier->kind == AST_SPECIFIER_POINTER)
		{
			Write(buffer, "*");
		}
		else if (specifier->kind == AST_SPECIFIER_OPTIONAL)
		{
			Write(buffer, "?");
		}
		else if (specifier->kind == AST_SPECIFIER_ARRAY)
		{
			Write(buffer, "[");
			Write(buffer, specifier->size_expression);
			Write(buffer, "]");
		}
	}

	if (type.basetype.kind == AST_BASETYPE_PRIMITIVE)
	{
		Write(buffer, type.basetype.token);
	}
	else if (type.basetype.kind == AST_BASETYPE_USERTYPE)
	{
		Write(buffer, type.basetype.token);
	}
	else if (type.basetype.kind == AST_BASETYPE_TUPLE)
	{
		Write(buffer, "(");

		for (Ast_Type* t = type.basetype.tuple; t < type.basetype.tuple; t++)
		{
			if (t != type.basetype.tuple) Write(buffer, ", ");
			Write(buffer, t);
		}

		Write(buffer, ")");
	}
	else if (type.basetype.kind == AST_BASETYPE_FUNCTION)
	{
		Write(buffer, "(");

		for (Ast_Type* t = type.basetype.function.input; t < type.basetype.function.input; t++)
		{
			if (t != type.basetype.function.input) Write(buffer, ", ");
			Write(buffer, t);
		}

		Write(buffer, ") -> (");
		Write(buffer, type.basetype.function.output);
		Write(buffer, ")");
	}
}

void Write(OutputBuffer* buffer, Ast_Expression* expression)
{
	if (!expression)
	{
		Write(buffer, "null");
		return;
	}

	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_VARIABLE:
			Write(buffer, "(Variable: ");
			Write(buffer, expression->variable->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_FUNCTION:
			Write(buffer, "(Function: ");
			Write(buffer, expression->function->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
			Write(buffer, "(Struct: ");
			Write(buffer, expression->structure->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_ENUM:
			Write(buffer, "(Enum: ");
			Write(buffer, expression->enumeration->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
			Write(buffer, "(Struct_Member: ");
			Write(buffer, expression->struct_member->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
			Write(buffer, "(Enum_Member: ");
			Write(buffer, expression->enum_member->name);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL:
			Write(buffer, expression->token);
			break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		case AST_EXPRESSION_BINARY_DOT:
		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
			Write(buffer, "(");
			Write(buffer, expression->left);
			Write(buffer, " ");
			Write(buffer, expression->token);
			Write(buffer, " ");
			Write(buffer, expression->right);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_UNARY_VALUE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_BINARY_NOT:
		case AST_EXPRESSION_UNARY_NOT:
			Write(buffer, "(");
			Write(buffer, expression->token);
			Write(buffer, " ");
			Write(buffer, expression->right);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_SUBSCRIPT:
			Write(buffer, expression->left);
			Write(buffer, "[");
			Write(buffer, expression->right);
			Write(buffer, "]");
			break;

		case AST_EXPRESSION_CALL:
			Write(buffer, expression->left);
			Write(buffer, "(");
			for (Ast_Expression** argument = expression->begin; argument < expression->end; argument++)
			{
				if (argument != expression->begin) Write(buffer, ", ");
				Write(buffer, *argument);
			}
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_TUPLE:
			Write(buffer, "(");
			for (Ast_Expression** param = expression->begin; param < expression->end; param++)
			{
				if (param != expression->begin) Write(buffer, ", ");
				Write(buffer, *param);
			}
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_IF_ELSE:
			Write(buffer, "(");
			Write(buffer, expression->left);
			Write(buffer, " if ");
			Write(buffer, expression->middle);
			Write(buffer, " else ");
			Write(buffer, expression->right);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_AS:
			Write(buffer, "(AS)");
			break;

		case AST_EXPRESSION_LAMBDA:
			Write(buffer, "(LAMBDA)");
			break;
	}
}

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
	return kind == TOKEN_BOOL
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
	return kind == TOKEN_IDENTIFIER
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
		|| kind == TOKEN_OPEN_PAREN;
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
			Unreachable();
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

		case TOKEN_NOT:
			return 8;

		case TOKEN_PLUS:  // I totally forgot why these are on 2 and not 1...
		case TOKEN_MINUS: //    it isn't because of exponentials...
			return 2;

		// @Bug: Not all unary operators are handled here.

		default:
			Assert("Invalid unary operator.");
			Unreachable();
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
			Unreachable();
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
	u32 start = 0;
	u32 number_of_lines = 1 + margin;

	// u32 number_of_lines = 1 + margin * 2;
	// if (margin < where.line)
	// {
	// 	start = where.line - margin;
	// }

	start = where.line;

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
		token++;
	}
}

static Ast_Type ParseType(Token*& token, u32 indent, Parse_Info* info);
static Ast_Expression* ParseExpression(Token*& token, u32 indent, Parse_Info* info, bool assignment_break = false, u32 parent_precedence = -1);
static Ast_Function ParseFunction(Token*& token, u32 indent, Parse_Info* info);
static Ast_Code ParseCode(Token*& token, u32 indent, Parse_Info* info);

static Ast_Struct ParseStruct(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Struct structure;
	ZeroMemory(&structure);
	token++;

	if (token->kind != TOKEN_IDENTIFIER)
	{
		Error(info, token->location, "Struct name missing\n");
	}

	CheckScope(token, indent, info);
	structure.name = token;
	token++;

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid struct declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, indent, info);
	token++;

	List<Ast_Struct_Member> members = null;

 	while (IsOnCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER)
		{
			Ast_Struct_Member member;
			member.name = token;
			token++;

			if (token->kind != TOKEN_COLON)
			{
				Error(info, token->location, "Expected ':', not: %\n", token);
			}

			CheckScope(token, indent+1, info);
			token++;

			member.type = ParseType(token, indent+2, info);
			members.Add(member);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Unexpected token: % after struct member.\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, indent+1, info);
				token++;
			}

			if (token->kind == TOKEN_SEMICOLON && IsOnCorrectScope(token, indent+1))
			{
				token++;
			}
		}
		else
		{
			Error(info, token->location, "Unexpected token in struct: %\n", token);
		}
	}

	structure.members = members;

	return structure;
}

static Ast_Enum ParseEnum(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Enum enumeration;
	token++;

	if (token->kind != TOKEN_IDENTIFIER)
	{
		Error(info, token->location, "Enum name missing\n");
	}

	CheckScope(token, indent, info);
	enumeration.name = token;
	token++;

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid enum declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, indent, info);
	token++;

	List<Ast_Enum_Member> members = null;

	while (IsOnCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER)
		{
			Ast_Enum_Member member;
			member.name = token;
			token++;

			if (token->kind != TOKEN_EQUAL)
			{
				Error(info, token->location, "Expected '=', not: %\n", token);
			}

			CheckScope(token, indent+2, info);
			token++;

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
				token++;
			}

			members.Add(member);
		}
		else
		{
			Error(info, token->location, "Unexpected token in enum: %\n", token);
		}
	}

	enumeration.members = members;

	return enumeration;
}

static Ast_Expression* ParseExpression(Token*& token, u32 indent, Parse_Info* info, bool assignment_break, u32 parent_precedence)
{
	Ast_Expression* left;

	if (IsUnaryOperator(token->kind) && IsOnCorrectScope(token, indent))
	{
		left = info->stack.Allocate<Ast_Expression>();
		if      (token->kind == TOKEN_ASTERISK)    left->kind = AST_EXPRESSION_UNARY_VALUE_OF;
		else if (token->kind == TOKEN_AMPERSAND)   left->kind = AST_EXPRESSION_UNARY_ADDRESS_OF;
		else if (token->kind == TOKEN_BITWISE_NOT) left->kind = AST_EXPRESSION_UNARY_BINARY_NOT;
		else if (token->kind == TOKEN_NOT)         left->kind = AST_EXPRESSION_UNARY_NOT;
		else if (token->kind == TOKEN_MINUS)       left->kind = AST_EXPRESSION_UNARY_MINUS;
		else if (token->kind == TOKEN_PLUS)        left->kind = AST_EXPRESSION_UNARY_PLUS;
		left->span.begin = token;
		left->token = token++;
		left->right = ParseExpression(token, indent, info, assignment_break, GetUnaryPrecedence(left->token->kind));
		left->span.end = token;
	}
	else if (IsLiteral(token->kind) && IsOnCorrectScope(token, indent))
	{
		left = info->stack.Allocate<Ast_Expression>();
		left->kind  = AST_EXPRESSION_TERMINAL_LITERAL;
		left->can_constantly_evaluate = true;
		left->is_pure = true;
		left->span.begin = token;
		left->token = token++;
		left->span.end = token;
	}
	else if (IsTerm(token->kind) && IsOnCorrectScope(token, indent))
	{
		left = info->stack.Allocate<Ast_Expression>();
		left->kind = AST_EXPRESSION_TERMINAL;
		left->span.begin = token;
		left->token = token++;
		left->span.end = token;
	}
	else if (token->kind == TOKEN_OPEN_PAREN && IsOnCorrectScope(token, indent))
	{
		Token* open = token++;
		Token* closure = open->GetClosure();
		List<Ast_Expression*> elements = null;

		if (token == closure)
		{
			Error(info, open->location, "Empty tuples aren't allowed.\n");
		}

		while (token < closure)
		{
			Ast_Expression* element = ParseExpression(token, indent+1, info);

			if (token->kind == TOKEN_COMMA)
			{
				elements.Add(element);
				CheckScope(token, indent, info);
				token++;

				if (token == closure)
				{
					Error(info, token->location, "Expected expression after ','\n");
				}
			}
			else if (token == closure)
			{
				if (elements)
				{
					elements.Add(element);
					left = info->stack.Allocate<Ast_Expression>();
					left->kind  = AST_EXPRESSION_TUPLE;
					left->token = open;
					left->begin = elements;
					left->end   = elements.End();
				}
				else left = element;
			}
			else
			{
				Error(info, token->location, "Invalid expression, unexpected token: %\n", token);
			}
		}

		CheckScope(token, indent, info);
		token = closure+1;
		left->span.end = token;
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
			Ast_Expression* if_else = info->stack.Allocate<Ast_Expression>();
			if_else->kind   = AST_EXPRESSION_IF_ELSE;
			if_else->span.begin = left->span.begin;
			if_else->token  = token++;
			if_else->left   = left;
			if_else->middle = ParseExpression(token, indent, info, false);

			if (token->kind != TOKEN_ELSE)
			{
				Error(info, token->location, "Invalid 'if' expression, missing 'else' clause. Unexpected: %\n", token);
			}

			CheckScope(token, indent, info);

			token++;
			if_else->right = ParseExpression(token, indent, info, assignment_break, GetTernaryPrecedence(TOKEN_IF));
			if_else->span.end = token;
			left = if_else;
		}
		else if (token->kind == TOKEN_OPEN_PAREN)
		{
			Token* open = token++;
			Token* closure = open->GetClosure();
			List<Ast_Expression*> arguments = null;

			while (token != closure)
			{
				arguments.Add(ParseExpression(token, indent+1, info));

				if (token->kind == TOKEN_COMMA)
				{
					CheckScope(token, indent, info);
					token++;

					if (token == closure)
					{
						Error(info, token->location, "Expected expression after ','\n");
					}
				}
				else if (token == closure)
				{
					CheckScope(token, indent, info);
					token++;
					break;
				}
				else
				{
					Error(info, token->location, "Invalid function call expression, unexpected token: %\n", token);
				}
			}

			token = closure + 1;

			Ast_Expression* call = info->stack.Allocate<Ast_Expression>();
			call->kind  = AST_EXPRESSION_CALL;
			call->span.begin = left->span.begin;
			call->span.end = token;
			call->token = open;
			call->left  = left;
			call->begin = arguments;
			call->end   = arguments.End();
			left = call;
		}
		else if (token->kind == TOKEN_OPEN_BRACKET)
		{
			Token* open = token++;
			Token* closure = open->GetClosure();

			Ast_Expression* subscript = info->stack.Allocate<Ast_Expression>();
			subscript->kind  = AST_EXPRESSION_SUBSCRIPT;
			subscript->span.begin = left->span.begin;
			subscript->span.end = closure+1;
			subscript->token = open;
			subscript->left  = left;

			if (token != closure)
			{
				subscript->right = ParseExpression(token, indent+1, info);

				if (token != closure)
				{
					Error(info, token->location, "Expected ']', not: %\n", token);
				}
			}
			else subscript->right = null;

			CheckScope(token, indent, info);
			token = closure + 1;

			left = subscript;
		}
		else
		{
			// @Indent I think the CheckScope needs to be here instead of at the start of ParseExpression (where we consume the term).
			Ast_Expression* binary = info->stack.Allocate<Ast_Expression>();

			if      (token->kind == TOKEN_EQUAL)            binary->kind = AST_EXPRESSION_BINARY_COMPARE_EQUAL;
			else if (token->kind == TOKEN_NOT_EQUAL)        binary->kind = AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL;
			else if (token->kind == TOKEN_LESS)             binary->kind = AST_EXPRESSION_BINARY_COMPARE_LESS;
			else if (token->kind == TOKEN_LESS_OR_EQUAL)    binary->kind = AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL;
			else if (token->kind == TOKEN_GREATER)          binary->kind = AST_EXPRESSION_BINARY_COMPARE_GREATER;
			else if (token->kind == TOKEN_GREATER_OR_EQUAL) binary->kind = AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL;
			else if (token->kind == TOKEN_AND)              binary->kind = AST_EXPRESSION_BINARY_AND;
			else if (token->kind == TOKEN_OR)               binary->kind = AST_EXPRESSION_BINARY_OR;
			else if (token->kind == TOKEN_DOT)              binary->kind = AST_EXPRESSION_BINARY_DOT;
			else if (token->kind == TOKEN_PLUS)             binary->kind = AST_EXPRESSION_BINARY_ADD;
			else if (token->kind == TOKEN_MINUS)            binary->kind = AST_EXPRESSION_BINARY_SUBTRACT;
			else if (token->kind == TOKEN_ASTERISK)         binary->kind = AST_EXPRESSION_BINARY_MULTIPLY;
			else if (token->kind == TOKEN_DIVIDE)           binary->kind = AST_EXPRESSION_BINARY_DIVIDE;
			else if (token->kind == TOKEN_MOD)              binary->kind = AST_EXPRESSION_BINARY_MODULO;
			else if (token->kind == TOKEN_EXPONENT)         binary->kind = AST_EXPRESSION_BINARY_EXPONENTIAL;
			else if (token->kind == TOKEN_BITWISE_OR)       binary->kind = AST_EXPRESSION_BINARY_BITWISE_OR;
			else if (token->kind == TOKEN_BITWISE_XOR)      binary->kind = AST_EXPRESSION_BINARY_BITWISE_XOR;
			else if (token->kind == TOKEN_BITWISE_AND)      binary->kind = AST_EXPRESSION_BINARY_BITWISE_AND;
			else if (token->kind == TOKEN_LEFT_SHIFT)       binary->kind = AST_EXPRESSION_BINARY_LEFT_SHIFT;
			else if (token->kind == TOKEN_RIGHT_SHIFT)      binary->kind = AST_EXPRESSION_BINARY_RIGHT_SHIFT;
			else Unreachable();

			binary->span.begin = left->span.begin;
			binary->token = token++;
			binary->left  = left;
			binary->right = ParseExpression(token, indent, info, assignment_break, GetBinaryPrecedence(binary->token->kind));
			binary->span.end = token;
			left = binary;
		}
	}

	return left;
}

static Ast_Type ParseType(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Type type;
	type.specifiers = null;

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
			token++;

			if (token != closure)
			{
				specifier.size_expression = ParseExpression(token, indent+1, info);
			}

			token = closure + 1;
		}
		else if (token->kind == TOKEN_ASTERISK)
		{
			specifier.kind = AST_SPECIFIER_POINTER;
			token++;
		}
		else if (token->kind == TOKEN_QUESTION_MARK)
		{
			specifier.kind = AST_SPECIFIER_OPTIONAL;
			token++;
		}

		type.specifiers.Add(specifier);
	}

	type.basetype.token = token;

	if (IsPrimitive(token->kind))
	{
		CheckScope(token, indent, info);
		type.basetype.kind = AST_BASETYPE_PRIMITIVE;
		token++;
	}
	else if (token->kind == TOKEN_IDENTIFIER)
	{
		CheckScope(token, indent, info);
		type.basetype.kind = AST_BASETYPE_USERTYPE;
		token++;
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
		token++;

		List<Ast_Type> params = null;

		while (token != closure)
		{
			Ast_Type param = ParseType(token, indent+1, info);
			params.Add(param);

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, info);
				token++;

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
		token++;

		List<Ast_Type> members = null;

		while (token != closure)
		{
			members.Add(ParseType(token, indent+1, info));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, info);
				token++;

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
	function->parameters = null;

	CheckScope(open_paren, indent, info);
	CheckScope(closure, indent, info);

	while (token < closure)
	{
		Ast_VariableDeclaration param;
		param.type = null;
		param.explicit_type = null;
		param.is_parameter = true;

		if (token->kind == TOKEN_COMMA)
		{
			Error(info, token->location, "Empty parameters not allowed. (Remove redundant ',')\n");
		}

		if (token->kind != TOKEN_IDENTIFIER)
		{
			Error(info, token->location, "Parameter name missing, unexpected: %\n", token);
		}

		CheckScope(token, indent+1, info);
		param.name = token++;

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Parameter type missing.\n");
		}

		CheckScope(token, indent+1, info);
		token++;

		param.explicit_type = info->stack.Allocate<Ast_Type>();
		*param.explicit_type = ParseType(token, indent+2, info);

		function->parameters.Add(param);

		if (token->kind != TOKEN_COMMA && token != closure)
		{
			Error(info, token->location, "Expected ',' or ')', not: %\n", token);
		}

		if (token->kind == TOKEN_COMMA)
		{
			CheckScope(token, indent, info);
			token++;
		}
	}
}

static Ast_BranchBlock ParseBranchBlock(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_BranchBlock branch_block;
	branch_block.branches = null;

	Ast_Branch branch;
	ZeroMemory(&branch);

	branch.token = token;
	token++;
	branch.condition = ParseExpression(token, indent+1, info, false);

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected ':' after branch condition, not: %\n", token);
	}

	CheckScope(token, indent, info);

	token++;
	branch.code = ParseCode(token, indent+1, info);
	branch_block.branches.Add(branch);

	while ((token->kind == TOKEN_ELSE || token->kind == TOKEN_THEN) && IsOnCorrectScope(token, indent))
	{
		Token* continuation_token = token;

		if (token->kind == TOKEN_ELSE)
		{
			branch.kind = AST_BRANCH_ELSE;
		}
		else if (token->kind == TOKEN_THEN)
		{
			branch.kind = AST_BRANCH_THEN;
		}

		token++;

		if (token->kind != TOKEN_COLON &&
 			token->kind != TOKEN_WHILE &&
			token->kind != TOKEN_FOR   &&
			token->kind != TOKEN_IF)
		{
			Error(info, token->location, "Expected 'if', 'while', 'for' or ':' after %, not: %\n", continuation_token, token);
		}

		CheckScope(token, indent, info);
		branch.token = token;

		if (token->kind == TOKEN_COLON)
		{
			CheckScope(token, indent, info);
			token++;
			branch.code = ParseCode(token, indent+1, info);
		}
		else
		{
			branch.condition = ParseExpression(token, indent+1, info, false);

			if (token->kind != TOKEN_COLON)
			{
				Error(info, token->location, "Expected ':' after branch condition, not: %\n", token);
			}

			branch.code = ParseCode(token, indent+1, info);
		}

		branch_block.branches.Add(branch);
	}

	return branch_block;
}

static Ast_Statement ParseStatement(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Statement statement;

	if (token[0].kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_COLON)
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
			token++;
			statement.variable_declaration.assignment = ParseExpression(token, indent+1, info);
		}

		SkipSemiColon(token, indent, info);
		return statement;
	}
	else if (IsExpressionStarter(token->kind))
	{
		Ast_Expression* expression = ParseExpression(token, indent, info, true);

		if (IsAssignment(token->kind))
		{
			if      (token->kind == TOKEN_EQUAL)             statement.kind = AST_STATEMENT_ASSIGNMENT;
			else if (token->kind == TOKEN_PLUS_EQUAL)        statement.kind = AST_STATEMENT_ASSIGNMENT_ADD;
			else if (token->kind == TOKEN_MINUS_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_SUBTRACT;
			else if (token->kind == TOKEN_TIMES_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_MULTIPLY;
			else if (token->kind == TOKEN_DIVIDE_EQUAL)      statement.kind = AST_STATEMENT_ASSIGNMENT_DIVIDE;
			else if (token->kind == TOKEN_EXPONENTIAL_EQUAL) statement.kind = AST_STATEMENT_ASSIGNMENT_POWER;

			CheckScope(token, indent+1, info);
			token++;

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
	else if (token->kind == TOKEN_DEFER)
	{
		statement.kind = AST_STATEMENT_DEFER;
		statement.defer.token = token++;

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Invalid 'defer' statement, Expected ':', not: %\n", token);
		}

		CheckScope(token, indent, info);
		token++;

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
		statement.claim.expression = null;

		if (IsOnCorrectScope(token, indent+1))
		{
			statement.claim.expression = ParseExpression(token, indent+1, info, false);
		}

		return statement;
	}
	else if (token->kind == TOKEN_ALIAS)
	{
		// @Todo
		statement.kind = AST_STATEMENT_ALIAS;
		statement.alias.token = token++;
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
			code.scope.structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, indent, info);
			enumeration.attribute = attribute;
			code.scope.enums.Add(enumeration);
		}
		else if (token->kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_OPEN_PAREN
			&&  (token[1].GetClosure()[1].kind == TOKEN_COLON || token[1].GetClosure()[1].kind == TOKEN_ARROW))
		{
			Ast_Function function = ParseFunction(token, indent, info);
			function.attribute = attribute;
			function.is_global = false;
			code.scope.functions.Add(function);
		}
		else
		{
			Ast_Statement statement = ParseStatement(token, indent, info);
			code.statements.Add(statement);

			if (token->kind != TOKEN_SEMICOLON && !token->newline)
			{
				Error(info, token->location, "Expected ';' at the end of statement. not: %\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				if (IsOnCorrectScope(token, indent))
				{
					token++;
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

	return code;
}

static Ast_Function ParseFunction(Token*& token, u32 indent, Parse_Info* info)
{
	Ast_Function function;
	ZeroMemory(&function);
	function.name = token++;
	ParseParameters(&function, token, indent, info);
	token = token->GetClosure()+1;

	if (token->kind != TOKEN_ARROW && token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected '->' or ':', not %\n", token);
	}

	if (token->kind == TOKEN_ARROW)
	{
		CheckScope(token, indent+1, info);
		token++;
		function.ast_return_type = info->stack.Allocate<Ast_Type>();
		*function.ast_return_type = ParseType(token, indent, info);
	}

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected ':', not %\n", token);
	}

	CheckScope(token, indent, info);

	token++;
	function.code = ParseCode(token, indent+1, info);

	return function;
}

static void ParseGlobalScope(Ast_Root* root, Token* token, Parse_Info* info)
{
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
			Ast_Import import;
			import.token = token;
			token++;

			if (token->kind != TOKEN_IDENTIFIER)
			{
				Error(info, token->location, "Expected identifier after import token, instead got: %\n", token);
			}

			CheckScope(token, 1, info);
			import.module = token;
			token++;
			root->imports.Add(import);

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, 1, info);
				token++;
			}
			else if (IsOnCorrectScope(token, 1))
			{
				// @Bug: IsOnCorrectScope(token, 1) allows indent > 1
				Error(info, token->location, "Unexpected token after import statement: %\n", token);
			}

			CheckScope(token, 0, info);
		}
		else if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStruct(token, 0, info);
			structure.attribute = attribute;
			root->scope.structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, 0, info);
			enumeration.attribute = attribute;
			root->scope.enums.Add(enumeration);
		}
		else if (token[0].kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_OPEN_PAREN)
		{
			Ast_Function function = ParseFunction(token, 0, info);
			function.attribute = attribute;
			function.is_global = true;
			root->scope.functions.Add(function);
		}
		else
		{
			Error(info, token->location, "Unexpected token in global scope: %\n", token);
		}
	}
}

void ParseFile(String file_path)
{
	Parse_Info info = LexicalParse(file_path);
	info.stack.Init();
	info.ast_root = info.stack.Allocate<Ast_Root>();
	ZeroMemory(info.ast_root);
	ParseGlobalScope(info.ast_root, info.tokens, &info);
	SemanticParse(&info);
}


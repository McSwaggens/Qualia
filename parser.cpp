#include "parser.h"
#include "token.h"
#include "print.h"
#include "memory.h"
#include "assert.h"

static void Write(OutputBuffer* buffer, Ast_Expression* expression);
static void Write(OutputBuffer* buffer, Ast_Type* type);
static void Write(OutputBuffer* buffer, Ast_Type type);

static void Write(OutputBuffer* buffer, Ast_Type* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	Write(buffer, *type);
}

static void Write(OutputBuffer* buffer, Ast_Type type)
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
		Write(buffer, type.basetype.primitive.token);
	}
	else if (type.basetype.kind == AST_BASETYPE_USERTYPE)
	{
		Write(buffer, type.basetype.usertype.token);
	}
	else if (type.basetype.kind == AST_BASETYPE_TUPLE)
	{
		Write(buffer, "(");

		for (Ast_Type* t = type.basetype.tuple.types; t < type.basetype.tuple.types; t++)
		{
			if (t != type.basetype.tuple.types) Write(buffer, ", ");
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

static void Write(OutputBuffer* buffer, Ast_Expression* expression)
{
	if (!expression)
	{
		Write(buffer, "null");
		return;
	}

	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL:
			Write(buffer, expression->token);
			break;

		case AST_EXPRESSION_BINARY:
			Write(buffer, "(");
			Write(buffer, expression->left);
			Write(buffer, " ");
			Write(buffer, expression->token);
			Write(buffer, " ");
			Write(buffer, expression->right);
			Write(buffer, ")");
			break;

		case AST_EXPRESSION_UNARY:
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
		case TOKEN_NOT:
			return 1;

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

static bool IsOnCorrectScope(Token* token, u32 scope)
{
	return !token->newline || token->indent == scope;
}

static void CheckScope(Token* token, u32 scope, Parse_Info* info)
{
	if (!IsOnCorrectScope(token, scope))
	{
		Error(info, token->location, "Invalid indentation.\n");
	}
}

static void SkipSemiColon(Token*& token, u32 scope, Parse_Info* info)
{
	if (token->kind == TOKEN_SEMICOLON && IsOnCorrectScope(token, scope))
	{
		token++;
	}
}

static Ast_Type ParseType(Token*& token, u32 scope, Parse_Info* info);
static Ast_Expression* ParseExpression(Token*& token, u32 scope, Parse_Info* info, bool assignment_break = false, u32 parent_precedence = -1);
static Ast_Function ParseFunction(Token*& token, u32 scope, Parse_Info* info);
static Ast_Code ParseCode(Token*& token, u32 scope, Parse_Info* info);

static Ast_Struct ParseStructure(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Struct structure;
	token++;

	if (token->kind != TOKEN_IDENTIFIER)
	{
		Error(info, token->location, "Struct name missing\n");
	}

	CheckScope(token, scope, info);
	structure.name = token;
	token++;

	Print("Ast_Struct %:\n", structure.name);

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid struct declaration syntax, unexpected token ", token, ", Expected ':'\n");
	}

	CheckScope(token, scope, info);
	token++;

	List<Ast_Struct_Member> members = null;

 	while (IsOnCorrectScope(token, scope+1))
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

			CheckScope(token, scope+1, info);
			token++;

			member.type = ParseType(token, scope+2, info);
			members.Add(member);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Unexpected token: % after struct member.\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, scope+1, info);
				token++;
			}

			Print("\t% : %\n", member.name, member.type);

			if (token->kind == TOKEN_SEMICOLON && IsOnCorrectScope(token, scope+1))
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

static Ast_Enum ParseEnumeration(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Enum enumeration;
	token++;

	if (token->kind != TOKEN_IDENTIFIER)
	{
		Error(info, token->location, "Enum name missing\n");
	}

	CheckScope(token, scope, info);
	enumeration.name = token;
	token++;

	Print("Ast_Enum %:\n", enumeration.name);

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Invalid enum declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, scope, info);
	token++;

	List<Ast_Enum_Member> members = null;

	while (IsOnCorrectScope(token, scope+1))
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

			CheckScope(token, scope+2, info);
			token++;

			if (token->kind == TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Expected expression before ';'\n");
			}

			member.expression = ParseExpression(token, scope+2, info);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
			{
				Error(info, token->location, "Unexpected token: % after enum member.\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, scope+1, info);
				token++;
			}

			members.Add(member);

			Print("\t% = %\n", member.name, member.expression);
		}
		else
		{
			Error(info, token->location, "Unexpected token in enum: %\n", token);
		}
	}

	enumeration.members = members;

	return enumeration;
}

static Ast_Expression* ParseExpression(Token*& token, u32 scope, Parse_Info* info, bool assignment_break, u32 parent_precedence)
{
	Ast_Expression* left;

	if (IsUnaryOperator(token->kind) && IsOnCorrectScope(token, scope))
	{
		left = info->stack.Allocate<Ast_Expression>();
		left->kind  = AST_EXPRESSION_UNARY;
		left->token = token++;
		left->right = ParseExpression(token, scope, info, assignment_break, GetUnaryPrecedence(left->token->kind));
	}
	else if (IsTerm(token->kind) && IsOnCorrectScope(token, scope))
	{
		left = info->stack.Allocate<Ast_Expression>();
		left->kind  = AST_EXPRESSION_TERMINAL;
		left->token = token++;
	}
	else if (token->kind == TOKEN_OPEN_PAREN && IsOnCorrectScope(token, scope))
	{
		Token* open = token++;
		Token* closure = open->GetClosure();
		List<Ast_Expression*> elements = null;

		while (token < closure)
		{
			Ast_Expression* element = ParseExpression(token, scope+1, info);

			if (token->kind == TOKEN_COMMA)
			{
				elements.Add(element);
				CheckScope(token, scope, info);
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

		CheckScope(token, scope, info);
		token = closure+1;
	}
	else
	{
		if (token->indent != scope)
		{
			CheckScope(token, scope, info);
		}
		else
		{
			Error(info, token->location, "Invalid expression, expected term, got: %\n", token);
		}
	}

	while (CanTakeNextOp(token, assignment_break, parent_precedence) && IsOnCorrectScope(token, scope))
	{
		// @Indent does unary operators need to be treated differently? (Error check)
		if (token->kind == TOKEN_IF)
		{
			Ast_Expression* if_else = info->stack.Allocate<Ast_Expression>();
			if_else->kind   = AST_EXPRESSION_IF_ELSE;
			if_else->token  = token++;
			if_else->left   = left;
			if_else->middle = ParseExpression(token, scope, info, false);

			if (token->kind != TOKEN_ELSE)
			{
				Error(info, token->location, "Invalid 'if' expression, missing 'else' clause. Unexpected: %\n", token);
			}

			CheckScope(token, scope, info);

			token++;
			if_else->right = ParseExpression(token, scope, info, assignment_break, GetTernaryPrecedence(TOKEN_IF));
			left = if_else;
		}
		else if (token->kind == TOKEN_OPEN_PAREN)
		{
			Token* open = token++;
			Token* closure = open->GetClosure();
			List<Ast_Expression*> arguments = null;

			while (token != closure)
			{
				arguments.Add(ParseExpression(token, scope+1, info));

				if (token->kind == TOKEN_COMMA)
				{
					CheckScope(token, scope, info);
					token++;

					if (token == closure)
					{
						Error(info, token->location, "Expected expression after ','\n");
					}
				}
				else if (token == closure)
				{
					CheckScope(token, scope, info);
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
			subscript->token = open;
			subscript->left  = left;

			if (token != closure)
			{
				subscript->right = ParseExpression(token, scope+1, info);

				if (token != closure)
				{
					Error(info, token->location, "Expected ']', not: %\n", token);
				}
			}
			else subscript->right = null;

			CheckScope(token, scope, info);
			token = closure + 1;

			left = subscript;
		}
		else
		{
			// @Indent I think the CheckScope needs to be here instead of at the start of ParseExpression (where we consume the term).
			Ast_Expression* binary = info->stack.Allocate<Ast_Expression>();
			binary->kind  = AST_EXPRESSION_BINARY;
			binary->token = token++;
			binary->left  = left;
			binary->right = ParseExpression(token, scope, info, assignment_break, GetBinaryPrecedence(binary->token->kind));
			left = binary;
		}
	}

	return left;
}

static Ast_Type ParseType(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Type type;
	type.specifiers = null;

	while (IsSpecifier(token->kind))
	{
		Ast_Specifier specifier;
		specifier.token = token;
		specifier.size_expression = null;
		CheckScope(token, scope, info);

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			specifier.kind = AST_SPECIFIER_ARRAY;
			Token* closure = token->GetClosure();
			CheckScope(closure, scope, info);

			if (token+1 != closure)
			{
				specifier.size_expression = ParseExpression(token, scope+1, info);
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

	if (IsPrimitive(token->kind))
	{
		CheckScope(token, scope, info);
		type.basetype.kind = AST_BASETYPE_PRIMITIVE;
		type.basetype.primitive.token = token;
		token++;
	}
	else if (token->kind == TOKEN_IDENTIFIER)
	{
		CheckScope(token, scope, info);
		type.basetype.kind = AST_BASETYPE_USERTYPE;
		type.basetype.usertype.token = token;
		token++;
	}
	else if (token->kind == TOKEN_OPEN_PAREN && token->GetClosure()[1].kind == TOKEN_ARROW)
	{
		Token* closure = token->GetClosure();
		CheckScope(token, scope, info);
		CheckScope(closure, scope, info);
		CheckScope(closure+1, scope, info);
		type.basetype.kind = AST_BASETYPE_FUNCTION;
		type.basetype.function.input = null;
		token++;

		while (token != closure)
		{
			type.basetype.function.input.Add(ParseType(token, scope+1, info));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, scope, info);
				token++;

				if (token == closure)
				{
					Error(info, token->location, "Expected type after ','\n");
				}
			}
		}

		token = closure+1;

		type.basetype.function.output = info->stack.Allocate<Ast_Type>();
		*type.basetype.function.output = ParseType(token, scope, info);
	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		Token* closure = token->GetClosure();
		CheckScope(token, scope, info);
		CheckScope(closure, scope, info);
		type.basetype.kind = AST_BASETYPE_TUPLE;
		type.basetype.tuple.types = null;
		token++;

		while (token != closure)
		{
			type.basetype.tuple.types.Add(ParseType(token, scope+1, info));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, scope, info);
				token++;

				if (token == closure)
				{
					Error(info, token->location, "Expected type after ','\n");
				}
			}
		}

		token = closure+1;
	}
	else
	{
		Error(info, token->location, "Expected type, not: %\n", token);
	}

	return type;
}

static void ParseParameters(Ast_Function* function, Token* open_paren, u32 scope, Parse_Info* info)
{
	Token* closure = open_paren->GetClosure();
	Token* token = open_paren+1;
	function->params = null;

	CheckScope(open_paren, scope, info);
	CheckScope(closure, scope, info);

	while (token < closure)
	{
		Ast_Param param;

		if (token->kind == TOKEN_COMMA)
		{
			Error(info, token->location, "Empty parameters not allowed. (Remove redundant ',')\n");
		}

		if (token->kind != TOKEN_IDENTIFIER)
		{
			Error(info, token->location, "Parameter name missing, unexpected: %\n", token);
		}

		CheckScope(token, scope+1, info);
		param.name = token++;

		if (token->kind != TOKEN_COLON)
		{
			Error(info, token->location, "Parameter type missing.\n");
		}

		CheckScope(token, scope+1, info);
		token++;

		param.type = ParseType(token, scope+2, info);

		function->params.Add(param);

		if (token->kind != TOKEN_COMMA && token != closure)
		{
			Error(info, token->location, "Expected ',' or ')', not: %\n", token);
		}

		if (token->kind == TOKEN_COMMA)
		{
			CheckScope(token, scope, info);
			token++;
		}
	}
}

static Ast_BranchBlock ParseBranchBlock(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_BranchBlock branch_block;
	branch_block.branches = null;

	Ast_Branch branch;
	ZeroMemory(&branch);

	branch.token = token;
	token++;
	branch.condition = ParseExpression(token, scope+1, info, false);

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected ':' after branch condition, not: %\n", token);
	}

	CheckScope(token, scope, info);

	token++;
	branch.code = ParseCode(token, scope+1, info);
	branch_block.branches.Add(branch);

	while ((token->kind == TOKEN_ELSE || token->kind == TOKEN_THEN) && IsOnCorrectScope(token, scope))
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

		CheckScope(token, scope, info);
		branch.token = token;

		if (token->kind == TOKEN_COLON)
		{
			CheckScope(token, scope, info);
			token++;
			branch.code = ParseCode(token, scope+1, info);
		}
		else
		{
			branch.condition = ParseExpression(token, scope+1, info, false);

			if (token->kind != TOKEN_COLON)
			{
				Error(info, token->location, "Expected ':' after branch condition, not: %\n", token);
			}

			branch.code = ParseCode(token, scope+1, info);
		}

		branch_block.branches.Add(branch);
	}

	return branch_block;
}

static Ast_Statement ParseStatement(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Statement statement;

	if (token[0].kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_COLON)
	{
		CheckScope(token+1, scope, info);

		statement.kind = AST_STATEMENT_VARIABLE_DECLARATION;
		statement.variable_declaration.name = token;
		statement.variable_declaration.type = null;

		token += 2;

		Print("Declaration: %\n", statement.variable_declaration.name);

		if (token->kind != TOKEN_EQUAL)
		{
			CheckScope(token, scope+1, info);
			statement.variable_declaration.type = info->stack.Allocate<Ast_Type>();
			*statement.variable_declaration.type = ParseType(token, scope+1, info);
		}

		if (token->kind == TOKEN_EQUAL)
		{
			CheckScope(token, scope+1, info);
			token++;
			statement.variable_declaration.assignment = ParseExpression(token, scope+1, info);
			Print("Expression: %\n", statement.variable_declaration.assignment);
		}

		SkipSemiColon(token, scope, info);
		return statement;
	}
	else if (IsExpressionStarter(token->kind))
	{
		Ast_Expression* expression = ParseExpression(token, scope, info, true);
		Print("Expression: %\n", expression);

		if (IsAssignment(token->kind))
		{
			CheckScope(token, scope+1, info);
			token++;

			statement.kind = AST_STATEMENT_ASSIGNMENT;
			statement.assignment.left  = expression;
			statement.assignment.token = token;
			statement.assignment.right = ParseExpression(token, scope+1, info, false);
		}
		else
		{
			statement.kind = AST_STATEMENT_EXPRESSION;
			statement.expression = expression;
		}

		if (token->kind != TOKEN_SEMICOLON && IsOnCorrectScope(token, scope+1))
		{
			Error(info, token->location, "Invalid expression-statement, unexpected token: %, expected assignment operator or ';'.\n", token);
		}

		SkipSemiColon(token, scope, info);
		return statement;
	}
	else if (token->kind == TOKEN_IF || token->kind == TOKEN_FOR || token->kind == TOKEN_WHILE)
	{
		Ast_BranchBlock branch_block = ParseBranchBlock(token, scope, info);
		statement.kind = AST_STATEMENT_BRANCH_BLOCK;
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

		CheckScope(token, scope, info);
		token++;

		Ast_Code code = ParseCode(token, scope+1, info);
		statement.defer.code = code;

		return statement;
	}
	else if (token->kind == TOKEN_BREAK)
	{
		statement.kind = AST_STATEMENT_BREAK;
		statement.token = token++;
		return statement;
	}
	else if (token->kind == TOKEN_RETURN)
	{
		statement.kind = AST_STATEMENT_RETURN;
		statement.ret.token = token++;
		statement.ret.expression = null;

		if (IsOnCorrectScope(token, scope+1))
		{
			statement.ret.expression = ParseExpression(token, scope+1, info, false);
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

static Ast_Attribute ParseAttribute(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Attribute attribute;
	Token* closure = token->GetClosure();
	CheckScope(token, scope, info);
	CheckScope(closure, scope, info);
	attribute.expression = ParseExpression(token, scope+1, info);
	token = closure+1;
	return attribute;
}

static Ast_Code ParseCode(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Code code;
	code.statements = null;
	code.functions  = null;
	code.structs    = null;
	code.enums      = null;

	while (IsOnCorrectScope(token, scope))
	{
		Ast_Attribute attribute;
		bool has_attribute = false;
		ZeroMemory(&attribute);

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			attribute = ParseAttribute(token, scope, info);
			has_attribute = true;
		}

		if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStructure(token, scope, info);
			structure.attribute = attribute;
			code.structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnumeration(token, scope, info);
			enumeration.attribute = attribute;
			code.enums.Add(enumeration);
		}
		else if (token->kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_OPEN_PAREN
			&&  (token[1].GetClosure()[1].kind == TOKEN_COLON || token[1].GetClosure()[1].kind == TOKEN_ARROW))
		{
			Ast_Function function = ParseFunction(token, scope, info);
			function.attribute = attribute;
			code.functions.Add(function);
		}
		else
		{
			Ast_Statement statement = ParseStatement(token, scope, info);
			code.statements.Add(statement);

			if (token->kind != TOKEN_SEMICOLON && !token->newline)
			{
				Error(info, token->location, "Expected ';' at the end of statement. not: %\n", token);
			}

			if (token->kind == TOKEN_SEMICOLON)
			{
				if (IsOnCorrectScope(token, scope))
				{
					token++;
				}
				else if (token->newline && token->indent > scope)
				{
					Error(info, token->location, "';' on wrong indentation.\n");
				}
			}
		}

		if (token->newline && token->indent < scope)
		{
			break;
		}
	}

	return code;
}

static Ast_Function ParseFunction(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Function function;
	function.name = token++;
	ParseParameters(&function, token, scope, info);
	token = token->GetClosure()+1;

	if (token->kind != TOKEN_ARROW && token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected '->' or ':', not %\n", token);
	}

	if (token->kind == TOKEN_ARROW)
	{
		CheckScope(token, scope+1, info);
		token++;
		function.return_type = info->stack.Allocate<Ast_Type>();
		*function.return_type = ParseType(token, scope, info);
	}
	else function.return_type = null;

	if (token->kind != TOKEN_COLON)
	{
		Error(info, token->location, "Expected ':', not %\n", token);
	}

	CheckScope(token, scope, info);

	token++;
	function.code = ParseCode(token, scope+1, info);

	return function;
}

static void ParseGlobalScope(Ast_Root* root, Token* token, Parse_Info* info)
{
	while (token->kind != TOKEN_EOF)
	{
		Ast_Attribute attribute;
		bool has_attribute = false;
		ZeroMemory(&attribute);

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

			Print("Import: %\n", import.module);

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
			Ast_Struct structure = ParseStructure(token, 0, info);
			structure.attribute = attribute;
			root->structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnumeration(token, 0, info);
			enumeration.attribute = attribute;
			root->enums.Add(enumeration);
		}
		else if (token[0].kind == TOKEN_IDENTIFIER && token[1].kind == TOKEN_OPEN_PAREN)
		{
			Ast_Function function = ParseFunction(token, 0, info);
			function.attribute = attribute;
			root->functions.Add(function);
		}
		else
		{
			Error(info, token->location, "Unexpected token in global scope: %\n", token);
		}
	}
}

Parse_Info ParseFile(String file_path)
{
	Parse_Info info = LexicalParse(file_path);
	info.stack.Init();
	info.ast_root = info.stack.Allocate<Ast_Root>();
	ZeroMemory(info.ast_root);

	ParseGlobalScope(info.ast_root, info.tokens, &info);
	Print("Parser completed successfully.\n");

	return info;
}


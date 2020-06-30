#include "parser.h"
#include "token.h"
#include "print.h"
#include "memory.h"
#include "assert.h"


static void Write(OutputBuffer* buffer, Ast_Expression* expression);
static void Write(OutputBuffer* buffer, Ast_TypeExpression* type);


static void Write(OutputBuffer* buffer, Ast_TypeExpression* type)
{
	if (!type)
	{
		Write(buffer, "null");
		return;
	}

	switch (type->kind)
	{
		case AST_TYPE_EXPRESSION_USERTYPE:
		case AST_TYPE_EXPRESSION_PRIMITIVE:
			Write(buffer, type->token);
			break;

		case AST_TYPE_EXPRESSION_POINTER:
			buffer->Write('*');
			Write(buffer, type->subtype);
			break;

		case AST_TYPE_EXPRESSION_OPTIONAL:
			buffer->Write('?');
			Write(buffer, type->subtype);
			break;

		case AST_TYPE_EXPRESSION_FUNCTION:
			buffer->Write('(');

			for (u32 i = 0; i < type->input_params.count; i++)
			{
				if (i) Write(buffer, ", ");
				Write(buffer, type->input_params[i]);
			}

			Write(buffer, ") -> ");
			Write(buffer, type->output_type);
			break;

		case AST_TYPE_EXPRESSION_ARRAY:
			buffer->Write('[');
			Write(buffer, type->size_expression);
			buffer->Write(']');
			Write(buffer, type->subtype);
			break;

		case AST_TYPE_EXPRESSION_TUPLE:
			Write(buffer, "(");

			for (u32 i = 0; i < type->tuple.count; i++)
			{
				if (i) Write(buffer, ", ");
				Write(buffer, type->tuple[i]);
			}

			Write(buffer, ")");
			break;
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


static bool IsTernaryOperator(Token_Kind kind)
{
	return kind == Token_If;
}


static bool IsBinaryOperator(Token_Kind kind)
{
	return kind == Token_Dot
		|| kind == Token_Exponent
		|| kind == Token_Asterisk
		|| kind == Token_Divide
		|| kind == Token_Plus
		|| kind == Token_Minus
		|| kind == Token_And
		|| kind == Token_Or
		|| kind == Token_AND
		|| kind == Token_OR
		|| kind == Token_XOR
		|| kind == Token_WeakAnd
		|| kind == Token_WeakOr
		|| kind == Token_StrongAnd
		|| kind == Token_StrongOr
		|| kind == Token_LeftShift
		|| kind == Token_RightShift
		|| kind == Token_Equal
		|| kind == Token_NotEqual
		|| kind == Token_Less
		|| kind == Token_LessOrEqual
		|| kind == Token_Greater
		|| kind == Token_GreaterOrEqual;
}


static bool IsUnaryOperator(Token_Kind kind)
{
	return kind == Token_Asterisk
		|| kind == Token_Ampersand
		|| kind == Token_Plus
		|| kind == Token_Minus
		|| kind == Token_NOT
		|| kind == Token_ExclamationMark;
}


static bool IsAssignment(Token_Kind kind)
{
	return kind == Token_Equal
		|| kind == Token_PlusEqual
		|| kind == Token_MinusEqual
		|| kind == Token_TimesEqual
		|| kind == Token_DivideEqual
		|| kind == Token_ExponentialEqual;
}


static bool IsPrimitive(Token_Kind kind)
{
	return kind == Token_Bool
		|| kind == Token_Int
		|| kind == Token_Int8
		|| kind == Token_Int16
		|| kind == Token_Int32
		|| kind == Token_Int64
		|| kind == Token_Uint
		|| kind == Token_Uint8
		|| kind == Token_Uint16
		|| kind == Token_Uint32
		|| kind == Token_Uint64
		|| kind == Token_Float16
		|| kind == Token_Float32
		|| kind == Token_Float64;
}


static bool IsTerm(Token_Kind kind)
{
	return kind == Token_Identifier
		|| kind == Token_IntegerLiteral
		|| kind == Token_FloatLiteral
		|| kind == Token_StringLiteral
		|| kind == Token_True
		|| kind == Token_False
		|| kind == Token_Null;
}


static bool IsExpressionStarter(Token_Kind kind)
{
	return IsUnaryOperator(kind)
		|| IsTerm(kind)
		|| kind == Token_OpenParen;
}


static u32 GetTernaryPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case Token_If:
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
		case Token_Dot:
			return 0;

		case Token_Exponent:
			return 1;

		case Token_Asterisk:
		case Token_Divide:
			return 3;

		case Token_Plus:
		case Token_Minus:
			return 4;

		case Token_AND:
		case Token_OR:
		case Token_XOR:
		case Token_LeftShift:
		case Token_RightShift:
			return 5;

		case Token_Equal:
		case Token_NotEqual:
		case Token_Less:
		case Token_LessOrEqual:
		case Token_Greater:
		case Token_GreaterOrEqual:
			return 7;

		case Token_And:
		case Token_WeakAnd:
		case Token_StrongAnd:
			return 8;

		case Token_Or:
		case Token_WeakOr:
		case Token_StrongOr:
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
		case Token_Asterisk:
		case Token_Ampersand:
			return 1;

		case Token_Plus:
		case Token_Minus:
			return 2;

		default:
			Assert("Invalid unary operator.");
			Unreachable();
	}
}


static u32 GetPostfixPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case Token_OpenParen:
		case Token_OpenBracket:
			return 0;

		default:
			Assert("Invalid postfix operator.");
			Unreachable();
	}
}


static bool IsOperatorRightToLeft(Token_Kind kind)
{
	return kind == Token_Exponent;
}


static bool IsPostfixOperator(Token_Kind kind)
{
	return kind == Token_OpenParen
		|| kind == Token_OpenBracket;
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
	return !(!IsOperator(token->kind) || (token->kind == Token_Equal && assignment_break)) && GetOperatorPrecedence(token->kind) < parent_precedence + IsOperatorRightToLeft(token->kind);
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
	if (token->kind == Token_SemiColon && IsOnCorrectScope(token, scope))
	{
		token++;
	}
}


static Ast_TypeExpression* ParseTypeExpression(Token*& token, u32 scope, Parse_Info* info);
static Ast_Expression* ParseExpression(Token*& token, u32 scope, Parse_Info* info, bool assignment_break = false, u32 parent_precedence = -1);
static Ast_Function ParseFunction(Token*& token, u32 scope, Parse_Info* info);
static Ast_Code ParseCode(Token*& token, u32 scope, Parse_Info* info);


static Ast_Struct ParseStructure(Token*& token, u32 scope, Parse_Info* info)
{
	Ast_Struct structure;
	token++;

	if (token->kind != Token_Identifier)
	{
		Error(info, token->location, "Struct name missing\n");
	}

	CheckScope(token, scope, info);
	structure.name = token;
	token++;

	Print("Ast_Struct:\n\tname: %\n", structure.name);

	if (token->kind != Token_Colon)
	{
		Error(info, token->location, "Invalid struct declaration syntax, unexpected token ", token, ", Expected ':'\n");
	}

	CheckScope(token, scope, info);
	token++;

	List<Ast_Struct_Member> members = null;

	while (IsOnCorrectScope(token, scope+1))
	{
		if (token->kind == Token_Identifier)
		{
			Ast_Struct_Member member;
			member.name = token;
			token++;

			if (token->kind != Token_Colon)
			{
				Error(info, token->location, "Expected ':', not: %\n", token);
			}

			CheckScope(token, scope+1, info);
			token++;

			member.type = ParseTypeExpression(token, scope+2, info);
			members.Add(member);

			if (!token->newline && token->kind != Token_SemiColon)
			{
				Error(info, token->location, "Unexpected token: % after struct member.\n", token);
			}

			if (token->kind == Token_SemiColon)
			{
				CheckScope(token, scope+1, info);
				token++;
			}

			Print("\tAst_Struct_Member:\n");
			Print("\t\tname: %\n", member.name);
			Print("\t\ttype: %\n", member.type);

			if (token->kind == Token_SemiColon && IsOnCorrectScope(token, scope+1))
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

	if (token->kind != Token_Identifier)
	{
		Error(info, token->location, "Enum name missing\n");
	}

	CheckScope(token, scope, info);
	enumeration.name = token;
	token++;

	Print("Ast_Enum:\n\tname: %\n", enumeration.name);

	if (token->kind != Token_Colon)
	{
		Error(info, token->location, "Invalid enum declaration syntax, unexpected token %, Expected ':'\n", token);
	}

	CheckScope(token, scope, info);
	token++;

	List<Ast_Enum_Member> members = null;

	while (IsOnCorrectScope(token, scope+1))
	{
		if (token->kind == Token_Identifier)
		{
			Ast_Enum_Member member;
			member.name = token;
			token++;

			if (token->kind != Token_Equal)
			{
				Error(info, token->location, "Expected '=', not: %\n", token);
			}

			CheckScope(token, scope+2, info);
			token++;

			if (token->kind == Token_SemiColon)
			{
				Error(info, token->location, "Expected expression before ';'\n");
			}

			member.expression = ParseExpression(token, scope+2, info);

			if (!token->newline && token->kind != Token_SemiColon)
			{
				Error(info, token->location, "Unexpected token: % after enum member.\n", token);
			}

			if (token->kind == Token_SemiColon)
			{
				CheckScope(token, scope+1, info);
				token++;
			}

			members.Add(member);

			Print("\tAst_Enum_Member:\n");
			Print("\t\tname: %\n", member.name);
			Print("\t\tvalue: %\n", member.expression);
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
	else if (token->kind == Token_OpenParen && IsOnCorrectScope(token, scope))
	{
		Token* open = token++;
		Token* closure = open->GetClosure();
		List<Ast_Expression*> elements = null;

		while (token < closure)
		{
			Ast_Expression* element = ParseExpression(token, scope+1, info);

			if (token->kind == Token_Comma)
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
		if (token->kind == Token_If)
		{
			Ast_Expression* if_else = info->stack.Allocate<Ast_Expression>();
			if_else->kind   = AST_EXPRESSION_IF_ELSE;
			if_else->token  = token++;
			if_else->left   = left;
			if_else->middle = ParseExpression(token, scope, info, false);

			if (token->kind != Token_Else)
			{
				Error(info, token->location, "Invalid 'if' expression, missing 'else' clause. Unexpected: %\n", token);
			}

			CheckScope(token, scope, info);

			token++;
			if_else->right = ParseExpression(token, scope, info, assignment_break, GetTernaryPrecedence(Token_If));
			left = if_else;
		}
		else if (token->kind == Token_OpenParen)
		{
			Token* open = token++;
			Token* closure = open->GetClosure();
			List<Ast_Expression*> arguments = null;

			while (token != closure)
			{
				arguments.Add(ParseExpression(token, scope+1, info));

				if (token->kind == Token_Comma)
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
		else if (token->kind == Token_OpenBracket)
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


static Ast_TypeExpression* ParseTypeExpression(Token*& token, u32 scope, Parse_Info* info)
{
	if (token->kind == Token_OpenParen)
	{
		Token* open = token++;
		Token* closure = open->GetClosure();
		List<Ast_TypeExpression*> types = null;
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();

		bool is_function = closure[1].kind == Token_Arrow;

		CheckScope(open, scope, info);
		CheckScope(closure, scope, info);

		while (token < closure)
		{
			types.Add(ParseTypeExpression(token, scope+1, info));

			if (token->kind == Token_Comma)
			{
				CheckScope(token, scope, info);
				token++;
			}
			else if (token < closure)
			{
				Error(info, token->location, "Invalid type, unexpected token: %\n", token);
			}
		}

		if (is_function)
		{
			CheckScope(closure + 1, scope, info);
			type->kind = AST_TYPE_EXPRESSION_FUNCTION;
			type->token = open;
			type->input_params = types;
			token = closure + 2;
			type->output_type = ParseTypeExpression(token, scope, info);
		}
		else
		{
			token = closure + 1;
			type->kind = AST_TYPE_EXPRESSION_TUPLE;
			type->token = open;
			type->tuple = types;
		}

		return type;
	}
	else if (token->kind == Token_OpenBracket)
	{
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();
		type->kind = AST_TYPE_EXPRESSION_ARRAY;
		Token* open = token++;
		Token* closure = open->GetClosure();
		type->token = open;

		CheckScope(open, scope, info);
		CheckScope(closure, scope, info);

		if (token != closure)
		{
			type->size_expression = ParseExpression(token, scope+1, info);
			if (token != closure)
			{
				Error(info, token->location, "Invalid array type specifier, unexpected token: %, expected: ']'\n", token);
			}
		}

		token = closure+1;
		type->subtype = ParseTypeExpression(token, scope, info);
		return type;
	}
	else if (token->kind == Token_Asterisk)
	{
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();
		type->kind = AST_TYPE_EXPRESSION_POINTER;
		type->token = token;
		CheckScope(token, scope, info);
		token++;
		type->subtype = ParseTypeExpression(token, scope, info);
		return type;
	}
	else if (token->kind == Token_QuestionMark)
	{
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();
		type->kind = AST_TYPE_EXPRESSION_OPTIONAL;
		type->token = token;
		CheckScope(token, scope, info);
		token++;
		type->subtype = ParseTypeExpression(token, scope, info);
		return type;
	}
	else if (IsPrimitive(token->kind))
	{
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();
		type->kind = AST_TYPE_EXPRESSION_PRIMITIVE;
		type->token = token;
		CheckScope(token, scope, info);
		token++;
		return type;
	}
	else if (token->kind == Token_Identifier)
	{
		Ast_TypeExpression* type = info->stack.Allocate<Ast_TypeExpression>();
		type->kind = AST_TYPE_EXPRESSION_USERTYPE;
		type->token = token;
		CheckScope(token, scope, info);
		token++;
		return type;
	}
	else
	{
		Error(info, token->location, "Invalid TypeExpression: %\n", token);
	}
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

		if (token->kind == Token_Comma)
		{
			Error(info, token->location, "Empty parameters not allowed. (Remove redundant ',')\n");
		}

		if (token->kind != Token_Identifier)
		{
			Error(info, token->location, "Parameter name missing, unexpected: %\n", token);
		}

		CheckScope(token, scope+1, info);
		param.name = token++;

		if (token->kind != Token_Colon)
		{
			Error(info, token->location, "Parameter type missing.\n");
		}

		CheckScope(token, scope+1, info);
		token++;

		param.type = ParseTypeExpression(token, scope+2, info);

		function->params.Add(param);

		if (token->kind != Token_Comma && token != closure)
		{
			Error(info, token->location, "Expected ',' or ')', not: %\n", token);
		}

		if (token->kind == Token_Comma)
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

	if (token->kind != Token_Colon)
	{
		Error(info, token->location, "Expected ':' after branch condition, not: %\n", token);
	}

	CheckScope(token, scope, info);

	token++;
	branch.code = ParseCode(token, scope+1, info);
	branch_block.branches.Add(branch);

	while ((token->kind == Token_Else || token->kind == Token_Then) && IsOnCorrectScope(token, scope))
	{
		Token* continuation_token = token;

		if (token->kind == Token_Else)
		{
			branch.kind = AST_BRANCH_ELSE;
		}
		else if (token->kind == Token_Then)
		{
			branch.kind = AST_BRANCH_THEN;
		}

		token++;

		if (token->kind != Token_Colon &&
 			token->kind != Token_While &&
			token->kind != Token_For   &&
			token->kind != Token_If)
		{
			Error(info, token->location, "Expected 'if', 'while', 'for' or ':' after %, not: %\n", continuation_token, token);
		}

		CheckScope(token, scope, info);
		branch.token = token;

		if (token->kind == Token_Colon)
		{
			CheckScope(token, scope, info);
			token++;
			branch.code = ParseCode(token, scope+1, info);
		}
		else
		{
			branch.condition = ParseExpression(token, scope+1, info, false);

			if (token->kind != Token_Colon)
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

	if (token[0].kind == Token_Identifier && token[1].kind == Token_Colon)
	{
		CheckScope(token+1, scope, info);

		statement.kind = AST_STATEMENT_VARIABLE_DECLARATION;
		statement.variable_declaration.name = token;
		statement.variable_declaration.type = null;

		token += 2;

		Print("Declaration: %\n", statement.variable_declaration.name);

		if (token->kind != Token_Equal)
		{
			CheckScope(token, scope+1, info);
			statement.variable_declaration.type = ParseTypeExpression(token, scope+1, info);
		}

		if (token->kind == Token_Equal)
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

		if (token->kind != Token_SemiColon && IsOnCorrectScope(token, scope+1))
		{
			Error(info, token->location, "Invalid expression-statement, unexpected token: %, expected assignment operator or ';'.\n", token);
		}

		SkipSemiColon(token, scope, info);
		return statement;
	}
	else if (token->kind == Token_If || token->kind == Token_For || token->kind == Token_While)
	{
		Ast_BranchBlock branch_block = ParseBranchBlock(token, scope, info);
		statement.kind = AST_STATEMENT_BRANCH_BLOCK;
		return statement;
	}
	else if (token->kind == Token_Defer)
	{
		statement.kind = AST_STATEMENT_DEFER;
		statement.defer.token = token++;

		if (token->kind != Token_Colon)
		{
			Error(info, token->location, "Invalid 'defer' statement, Expected ':', not: %\n", token);
		}

		CheckScope(token, scope, info);
		token++;

		Ast_Code code = ParseCode(token, scope+1, info);
		statement.defer.code = code;

		return statement;
	}
	else if (token->kind == Token_Break)
	{
		statement.kind = AST_STATEMENT_BREAK;
		statement.token = token++;
		return statement;
	}
	else if (token->kind == Token_Return)
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
	else if (token->kind == Token_Alias)
	{
		// @Todo
		statement.kind = AST_STATEMENT_ALIAS;
		statement.alias.token = token++;
		return statement;
	}
	else if (token->kind == Token_SemiColon)
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

		if (token->kind == Token_OpenBracket)
		{
			attribute = ParseAttribute(token, scope, info);
			has_attribute = true;
		}

		if (token->kind == Token_Struct)
		{
			Ast_Struct structure = ParseStructure(token, scope, info);
			structure.attribute = attribute;
			code.structs.Add(structure);
		}
		else if (token->kind == Token_Enum)
		{
			Ast_Enum enumeration = ParseEnumeration(token, scope, info);
			enumeration.attribute = attribute;
			code.enums.Add(enumeration);
		}
		else if (token->kind == Token_Identifier && token[1].kind == Token_OpenParen
			&&  (token[1].GetClosure()[1].kind == Token_Colon || token[1].GetClosure()[1].kind == Token_Arrow))
		{
			Ast_Function function = ParseFunction(token, scope, info);
			function.attribute = attribute;
			code.functions.Add(function);
		}
		else
		{
			Ast_Statement statement = ParseStatement(token, scope, info);
			code.statements.Add(statement);

			if (token->kind != Token_SemiColon && !token->newline)
			{
				Error(info, token->location, "Expected ';' at the end of statement. not: %\n", token);
			}

			if (token->kind == Token_SemiColon)
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

	if (token->kind != Token_Arrow && token->kind != Token_Colon)
	{
		Error(info, token->location, "Expected '->' or ':', not %\n", token);
	}

	if (token->kind == Token_Arrow)
	{
		CheckScope(token, scope+1, info);
		token++;
		function.return_type = ParseTypeExpression(token, scope, info);
	}
	else function.return_type = null;

	if (token->kind != Token_Colon)
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
	while (token->kind != Token_Eof)
	{
		Ast_Attribute attribute;
		bool has_attribute = false;
		ZeroMemory(&attribute);

		if (token->kind == Token_OpenBracket)
		{
			attribute = ParseAttribute(token, 0, info);
			has_attribute = true;
		}

		if (token->kind == Token_Import)
		{
			Ast_Import import;
			import.token = token;
			token++;

			if (token->kind != Token_Identifier)
			{
				Error(info, token->location, "Expected identifier after import token, instead got: %\n", token);
			}

			CheckScope(token, 1, info);
			import.module = token;
			token++;
			root->imports.Add(import);

			Print("Import: %\n", import.module);

			if (token->kind == Token_SemiColon)
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
		else if (token->kind == Token_Struct)
		{
			Ast_Struct structure = ParseStructure(token, 0, info);
			structure.attribute = attribute;
			root->structs.Add(structure);
		}
		else if (token->kind == Token_Enum)
		{
			Ast_Enum enumeration = ParseEnumeration(token, 0, info);
			enumeration.attribute = attribute;
			root->enums.Add(enumeration);
		}
		else if (token[0].kind == Token_Identifier && token[1].kind == Token_OpenParen)
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


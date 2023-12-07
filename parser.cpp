#include "parser.h"
#include "error.h"
#include "token.h"
#include "print.h"
#include "memory.h"
#include "assert.h"
#include "array_buffer.h"

static bool IsSpecifier(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_ASTERISK:
		case TOKEN_QUESTION_MARK:
		case TOKEN_OPEN_BRACKET:
			return true;

		default:
			return false;
	}
}

static bool IsTernaryOperator(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_IF:
			return true;

		default:
			return false;
	}
}

static bool IsIdentifier(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL:
			return true;

		default:
			return false;
	}
}

static bool IsBinaryOperator(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_DOT:
		case TOKEN_EXPONENT:
		case TOKEN_ASTERISK:
		case TOKEN_DIVIDE:
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_AND:
		case TOKEN_OR:
		case TOKEN_MOD:
		case TOKEN_BITWISE_AND:
		case TOKEN_BITWISE_OR:
		case TOKEN_BITWISE_XOR:
		case TOKEN_LEFT_SHIFT:
		case TOKEN_RIGHT_SHIFT:
		case TOKEN_AS:
		case TOKEN_EQUAL:
		case TOKEN_NOT_EQUAL:
		case TOKEN_LESS:
		case TOKEN_LESS_OR_EQUAL:
		case TOKEN_GREATER:
		case TOKEN_GREATER_OR_EQUAL:
			return true;

		default:
			return false;
	}
}

static bool IsUnaryOperator(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_ASTERISK:
		case TOKEN_AMPERSAND:
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_BITWISE_NOT:
		case TOKEN_NOT:
		case TOKEN_EXCLAMATION_MARK:
			return true;

		default:
			return false;
	}
}

static bool IsAssignment(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_EQUAL:
		case TOKEN_PLUS_EQUAL:
		case TOKEN_MINUS_EQUAL:
		case TOKEN_TIMES_EQUAL:
		case TOKEN_DIVIDE_EQUAL:
		case TOKEN_EXPONENTIAL_EQUAL:
			return true;

		default:
			return false;
	}
}

static bool IsPrimitive(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_INT:
		case TOKEN_INT8:
		case TOKEN_INT16:
		case TOKEN_INT32:
		case TOKEN_INT64:
		case TOKEN_UINT:
		case TOKEN_UINT8:
		case TOKEN_UINT16:
		case TOKEN_UINT32:
		case TOKEN_UINT64:
		case TOKEN_FLOAT16:
		case TOKEN_FLOAT32:
		case TOKEN_FLOAT64:
			return true;

		default:
			return false;
	}
}

static bool IsTerm(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL:
		case TOKEN_LITERAL_INT:
		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT:
		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT16:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		case TOKEN_LITERAL_STRING:
		case TOKEN_TRUE:
		case TOKEN_FALSE:
		case TOKEN_NULL:
			return true;

		default:
			return false;
	}
}

static bool IsLiteral(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_LITERAL_INT:
		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT:
		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT16:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		case TOKEN_LITERAL_STRING:
		case TOKEN_TRUE:
		case TOKEN_FALSE:
		case TOKEN_NULL:
			return true;

		default:
			return false;
	}
}

static bool IsExpressionStarter(Token_Kind kind)
{
	return IsUnaryOperator(kind)
		|| IsTerm(kind)
		|| kind == TOKEN_OPEN_PAREN
		|| kind == TOKEN_OPEN_BRACKET
		|| kind == TOKEN_OPEN_BRACE;
}

static uint32 GetTernaryPrecedence(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_IF:
			return 7;

		default:
			Assert("Invalid ternary operator.");
			Unreachable();
	}
}

static uint32 GetBinaryPrecedence(Token_Kind kind)
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

		// case TOKEN_DOT_DOT:
		// 	return 8;

		case TOKEN_EQUAL:
		case TOKEN_NOT_EQUAL:
		case TOKEN_LESS:
		case TOKEN_LESS_OR_EQUAL:
		case TOKEN_GREATER:
		case TOKEN_GREATER_OR_EQUAL:
			return 9;

		case TOKEN_AND:
			return 10;

		case TOKEN_OR:
			return 11;

		default:
			Assert("Invalid binary operator.");
			// Unreachable();
			return -1;
	}
}

static uint32 GetUnaryPrecedence(Token_Kind kind)
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
			return 10;

		default:
			Assert("Invalid unary operator.");
			// Unreachable();
			return -1;
	}
}

static uint32 GetPostfixPrecedence(Token_Kind kind)
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

static uint32 GetOperatorPrecedence(Token_Kind kind)
{
	if (IsBinaryOperator(kind))  return GetBinaryPrecedence(kind);
	if (IsTernaryOperator(kind)) return GetTernaryPrecedence(kind);
	if (IsPostfixOperator(kind)) return GetPostfixPrecedence(kind);
	AssertUnreachable();
}

static bool IsCorrectScope(Token* token, uint32 indent)
{
	return !token->newline || token->indent == indent;
}

static void CheckScope(Token* token, uint32 indent, Ast_Module* module)
{
	if (!IsCorrectScope(token, indent))
		Error(module, token->location, "Invalid indentation.\n");
}

static Ast_Type ParseType(Token*& token, uint32 indent, Ast_Module* module);
static Ast_Expression* ParseExpression(Token*& token, uint32 indent, Ast_Module* module, bool assignment_break = false, uint32 parent_precedence = -2);
static Ast_Function ParseFunction(Token*& token, uint32 indent, Ast_Module* module);
static Ast_Code ParseCode(Token*& token, uint32 indent, Ast_Module* module);

static Ast_Struct ParseStruct(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Struct structure;
	ZeroMemory(&structure);
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
	{
		if (IsIdentifier(token->kind))
			Error(module, token->location, "Struct name must start with an uppercase letter.\n");
		else
			Error(module, token->location, "Struct name missing\n");
	}

	CheckScope(token, indent+1, module);
	structure.name = token->identifier_string;
	structure.name_token = token;
	token += 1;

	if (token->kind != TOKEN_COLON)
		Error(module, token->location, "Invalid struct declaration syntax, unexpected token %, Expected ':'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	Array_Buffer<Ast_Struct_Member> members = CreateArrayBuffer<Ast_Struct_Member>();

 	while (IsCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER_CASUAL)
		{
			Ast_Struct_Member member;
			member.name = token->identifier_string;
			member.name_token = token;
			member.index = members.count;
			token += 1;

			if (token->kind != TOKEN_COLON)
				Error(module, token->location, "Expected ':', not: '%'\n", token);

			CheckScope(token, indent+1, module);
			token += 1;

			member.ast_type = ParseType(token, indent+2, module);
			members.Add(member);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
				Error(module, token->location, "Unexpected token: % after struct member.\n", token);

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, indent+1, module);
				token += 1;
			}

			if (token->kind == TOKEN_SEMICOLON && IsCorrectScope(token, indent+1))
			{
				token += 1;
			}
		}
		else if (token->kind == TOKEN_IDENTIFIER_FORMAL)
			Error(module, token->location, "Struct member names must start with a lowercase letter.\n");
		else
			Error(module, token->location, "Unexpected token in struct: '%'\n", token);
	}

	structure.members = members.Lock();

	return structure;
}

static Ast_Enum ParseEnum(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Enum enumeration;
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
	{
		if (IsIdentifier(token->kind))
			Error(module, token->location, "Enum names must start with an uppercase letter.\n");
		else
			Error(module, token->location, "Enum name missing.\n");
	}

	CheckScope(token, indent+1, module);
	enumeration.name = token->identifier_string;
	enumeration.name_token = token;
	token += 1;

	if (token->kind != TOKEN_COLON)
		Error(module, token->location, "Invalid enum declaration syntax, unexpected token '%', Expected ':'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	Array_Buffer<Ast_Enum_Member> members = CreateArrayBuffer<Ast_Enum_Member>();

	while (IsCorrectScope(token, indent+1))
	{
		if (token->kind == TOKEN_IDENTIFIER_FORMAL)
		{
			Ast_Enum_Member member;
			member.name_token = token;
			member.name = token->identifier_string;
			member.index = members.count;
			token += 1;

			if (token->kind != TOKEN_EQUAL)
				Error(module, token->location, "Expected '=', not: '%'\n", token);

			CheckScope(token, indent+2, module);
			token += 1;

			if (token->kind == TOKEN_SEMICOLON)
				Error(module, token->location, "Expected expression before ';'\n");

			CheckScope(token, indent+2, module);
			member.expression = ParseExpression(token, indent+2, module);

			if (!token->newline && token->kind != TOKEN_SEMICOLON)
				Error(module, token->location, "Unexpected token: '%' after enum member.\n", token);

			if (token->kind == TOKEN_SEMICOLON)
			{
				CheckScope(token, indent+1, module);
				token += 1;
			}

			members.Add(member);
		}
		else if (token->kind == TOKEN_IDENTIFIER_CASUAL)
			Error(module, token->location, "Enum member names must start with a uppercase letter.\n");
		else
			Error(module, token->location, "Unexpected token in enum: '%'\n", token);
	}

	enumeration.members = members.Lock();

	return enumeration;
}

static bool CanTakeNextOp(Token* token, bool assignment_break, uint32 parent_precedence)
{
	return !(!IsOperator(token->kind) || (token->kind == TOKEN_EQUAL && assignment_break)) && GetOperatorPrecedence(token->kind) < parent_precedence + IsOperatorRightToLeft(token->kind);
}

static Ast_Expression* ParseExpression(Token*& token, uint32 indent, Ast_Module* module, bool assignment_break, uint32 parent_precedence)
{
	Ast_Expression* left;

	Token* begin = token;

	if (IsUnaryOperator(token->kind))
	{
		Ast_Expression_Unary* unary = StackAllocate<Ast_Expression_Unary>(&module->stack);
		ZeroMemory(unary);

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

		unary->op = token;
		token += 1;
		CheckScope(token, indent, module);
		unary->subexpression = ParseExpression(token, indent, module, assignment_break, GetUnaryPrecedence(unary->op->kind));
		left = unary;
	}
	else if (IsLiteral(token->kind))
	{
		Ast_Expression_Literal* literal = StackAllocate<Ast_Expression_Literal>(&module->stack);
		ZeroMemory(literal);
		literal->kind  = AST_EXPRESSION_TERMINAL_LITERAL;
		literal->flags |= AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE | AST_EXPRESSION_FLAG_PURE;
		literal->token = token;
		token += 1;
		left = literal;
	}
	else if (IsIdentifier(token->kind))
	{
		Ast_Expression_Terminal* term = StackAllocate<Ast_Expression_Terminal>(&module->stack);
		ZeroMemory(term);
		term->kind = AST_EXPRESSION_TERMINAL_NAME;
		term->token = token;
		token += 1;
		left = term;
	}
	else if (token->kind == TOKEN_OPEN_BRACKET)
	{
		Token* closure = token->closure;

		Ast_Expression_Array* array = StackAllocate<Ast_Expression_Array>(&module->stack);
		ZeroMemory(array);

		array->kind = AST_EXPRESSION_ARRAY;
		token += 1;

		if (!IsExpressionStarter(token->kind))
			Error(module, token->location, "Expected expression after '['\n");

		CheckScope(token, indent, module);
		array->left = ParseExpression(token, indent+1, module);

		if (token->kind != TOKEN_DOT_DOT)
			Error(module, token->location, "Expected '..' operator, not: \n", token);

		CheckScope(token, indent, module);
		token += 1;

		if (!IsExpressionStarter(token->kind))
			Error(module, token->location, "Array extent not specified\n");

		CheckScope(token, indent+1, module);
		array->right = ParseExpression(token, indent+1, module);

		if (token->kind != TOKEN_CLOSE_BRACKET)
			Error(module, token->location, "End of array expression missing, expected ']', not: '%'\n", token);

		CheckScope(token, indent, module);
		token += 1;

		left = array;
	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		Ast_Expression_Tuple* tuple = StackAllocate<Ast_Expression_Tuple>(&module->stack);
		ZeroMemory(tuple);

		Token* closure = token->closure;

		tuple->kind  = AST_EXPRESSION_TUPLE;

		token += 1;

		Array_Buffer<Ast_Expression*> elements = CreateArrayBuffer<Ast_Expression*>();

		if (closure[-1].kind == TOKEN_COMMA)
			Error(module, token->location, "Expected expression after ','\n");

		while (token < closure)
		{
			CheckScope(token, indent+1, module);
			elements.Add(ParseExpression(token, indent+1, module));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, module);
				token += 1;
			}
			else if (token < closure)
				Error(module, token->location, "Invalid expression, unexpected token: '%'\n", token);
		}

		tuple->elements = elements.Lock();

		CheckScope(token, indent, module);
		token = closure+1;
		left = tuple;
	}
	else if (token->kind == TOKEN_OPEN_BRACE)
	{
		Ast_Expression_Fixed_Array* fixed_array = StackAllocate<Ast_Expression_Fixed_Array>(&module->stack);
		ZeroMemory(fixed_array);

		fixed_array->kind = AST_EXPRESSION_FIXED_ARRAY;

		Token* closure = token->closure;
		Array_Buffer<Ast_Expression*> elements = CreateArrayBuffer<Ast_Expression*>();

		CheckScope(closure, indent, module);

		if (token+1 == closure)
			Error(module, token->location, "Empty arrays literals aren't allowed.\n");

		token += 1;

		while (token < closure)
		{
			CheckScope(token, indent+1, module);
			Ast_Expression* expression = ParseExpression(token, indent+1, module, false);
			elements.Add(expression);
			CheckScope(token, indent, module);

			if (token->kind == TOKEN_COMMA)
			{
				token += 1;

				if (token == closure)
					Error(module, token->location, "Expected expression after ',', not: '%'\n", token);
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
			CheckScope(token, indent, module);
		}

		Error(module, token->location, "Invalid expression, expected term, got: '%'\n", token);
	}

	while (CanTakeNextOp(token, assignment_break, parent_precedence) && IsCorrectScope(token, indent))
	{
		// @Indent does unary operators need to be treated differently? (Error check)
		if (token->kind == TOKEN_IF)
		{
			Ast_Expression_Ternary* if_else = StackAllocate<Ast_Expression_Ternary>(&module->stack);
			ZeroMemory(if_else);

			if_else->kind = AST_EXPRESSION_IF_ELSE;
			if_else->ops[0] = token;
			token += 1;
			if_else->left = left;
			CheckScope(token, indent, module);
			if_else->middle = ParseExpression(token, indent, module, false);

			if (token->kind != TOKEN_ELSE)
				Error(module, token->location, "Invalid 'if' expression, missing 'else' clause. Unexpected: '%'\n", token);

			if_else->ops[1] = token;
			CheckScope(token, indent, module);
			token += 1;

			CheckScope(token, indent, module);
			if_else->right = ParseExpression(token, indent, module, assignment_break, GetTernaryPrecedence(TOKEN_IF));
			left = if_else;
		}
		else if (token->kind == TOKEN_AS)
		{
			Ast_Expression_As* as = StackAllocate<Ast_Expression_As>(&module->stack);
			ZeroMemory(as);

			CheckScope(token, indent, module);
			as->op = token++;

			as->kind = AST_EXPRESSION_AS;
			as->ast_type = ParseType(token, indent, module);
			as->expression = left;

			left = as;
		}
		else if (token->kind == TOKEN_OPEN_PAREN)
		{
			Ast_Expression_Call* call = StackAllocate<Ast_Expression_Call>(&module->stack);
			ZeroMemory(call);

			Token* open = token;
			Token* closure = open->closure;
			List<Ast_Expression*> arguments = null;

			CheckScope(token, indent, module);
			call->parameters = (Ast_Expression_Tuple*)ParseExpression(token, indent, module, false, GetOperatorPrecedence(token->kind));

			token = closure + 1;

			call->kind  = AST_EXPRESSION_CALL;
			call->function = left;
			left = call;
		}
		else if (token->kind == TOKEN_OPEN_BRACKET)
		{
			Token* open = token++;
			Token* closure = open->closure;

			Ast_Expression_Subscript* subscript = StackAllocate<Ast_Expression_Subscript>(&module->stack);
			ZeroMemory(subscript);

			subscript->kind  = AST_EXPRESSION_SUBSCRIPT;
			subscript->array = left;

			if (token != closure)
			{
				CheckScope(token, indent+1, module);
				subscript->index = ParseExpression(token, indent+1, module);

				if (token != closure)
					Error(module, token->location, "Expected ']', not: '%'\n", token);
			}
			else subscript->index = null;

			CheckScope(token, indent, module);
			token = closure + 1;

			left = subscript;
		}
		else
		{
			// @Indent I think the CheckScope needs to be here instead of at the start of ParseExpression (where we consume the term).
			Ast_Expression_Binary* binary = StackAllocate<Ast_Expression_Binary>(&module->stack);
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
				// case TOKEN_DOT_DOT:          binary->kind = AST_EXPRESSION_BINARY_RANGE;                    break;
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
				default: Assert(); Unreachable();
			}

			binary->op = token++;
			binary->left  = left;
			CheckScope(token, indent, module);
			binary->right = ParseExpression(token, indent, module, assignment_break, GetBinaryPrecedence(binary->op->kind));
			left = binary;
		}

		left->begin = begin;
		left->end = token;
	}

	return left;
}

static Token* GetEndOfTypeIfValid(Token* token)
{
	while (IsSpecifier(token->kind))
	{
		if (token->kind == TOKEN_OPEN_BRACKET) token = token->closure;
		token += 1;
	}

	if (IsPrimitive(token->kind) || token->kind == TOKEN_IDENTIFIER_FORMAL)
	{
		return token + 1;
	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		token = token->closure + 1;

		if (token->kind == TOKEN_ARROW)
		{
			token = GetEndOfTypeIfValid(token + 1);
		}

		return token;
	}

	return null;
}

static Ast_Type ParseType(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Type type;
	ZeroMemory(&type);

	Array_Buffer<Ast_Specifier> specifiers = CreateArrayBuffer<Ast_Specifier>();

	while (IsSpecifier(token->kind))
	{
		Ast_Specifier specifier;
		specifier.token = token;
		specifier.size_expression = null;
		CheckScope(token, indent, module);

		if (token->kind == TOKEN_OPEN_BRACKET)
		{
			specifier.kind = AST_SPECIFIER_ARRAY;

			Token* closure = token->closure;
			CheckScope(closure, indent, module);
			token += 1;

			if (token != closure)
			{
				specifier.kind = AST_SPECIFIER_FIXED_ARRAY;
				specifier.size_expression = ParseExpression(token, indent+1, module);
			}

			if (token != closure)
				Error(module, token->location, "Expected ']', not: %\n", token);

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
		CheckScope(token, indent, module);
		type.basetype.kind = AST_BASETYPE_PRIMITIVE;
		token += 1;
	}
	else if (token->kind == TOKEN_IDENTIFIER_FORMAL)
	{
		CheckScope(token, indent, module);
		type.basetype.kind = AST_BASETYPE_USERTYPE;
		token += 1;
	}
	else if (token->kind == TOKEN_OPEN_PAREN && token->closure[1].kind == TOKEN_ARROW)
	{
		Token* closure = token->closure;
		CheckScope(token, indent, module);
		CheckScope(closure, indent, module);
		CheckScope(closure+1, indent, module);
		type.basetype.kind = AST_BASETYPE_FUNCTION;
		type.basetype.function.input = null;
		type.basetype.function.output = null;
		token += 1;

		List<Ast_Type> params = null;

		while (token != closure)
		{
			Ast_Type param = ParseType(token, indent+1, module);
			params.Add(param);

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, module);
				token += 1;

				if (token == closure)
					Error(module, token->location, "Expected type after ','\n");
			}
		}

		token = closure+2;

		if (params.count == 0)
		{
			type.basetype.function.input = null;
		}
		else
		{
			type.basetype.function.input = StackAllocate<Ast_Type>(&module->stack);
			ZeroMemory(type.basetype.function.input);
			type.basetype.function.input->basetype.kind = AST_BASETYPE_TUPLE;
			type.basetype.function.input->basetype.tuple = params.ToArray();
		}

		// () -> () -> XXX
		if (token->kind == TOKEN_OPEN_PAREN && token[1].kind == TOKEN_CLOSE_PAREN && token->closure[1].kind != TOKEN_ARROW)
		{
			type.basetype.function.output = null;
			token = token->closure+1;
		}
		else
		{
			type.basetype.function.output = StackAllocate<Ast_Type>(&module->stack);
			*type.basetype.function.output = ParseType(token, indent, module);
		}

	}
	else if (token->kind == TOKEN_OPEN_PAREN)
	{
		Token* closure = token->closure;
		CheckScope(token, indent, module);
		CheckScope(closure, indent, module);
		type.basetype.kind = AST_BASETYPE_TUPLE;
		type.basetype.tuple = null;
		token += 1;

		List<Ast_Type> members = null;

		while (token != closure)
		{
			members.Add(ParseType(token, indent+1, module));

			if (token->kind == TOKEN_COMMA)
			{
				CheckScope(token, indent, module);
				token += 1;

				if (token == closure)
					Error(module, token->location, "Expected type after ','\n");
			}
		}

		type.basetype.tuple = members.ToArray();

		token = closure+1;
	}
	else
		Error(module, token->location, "Expected type, not: '%'\n", token);

	return type;
}

static void ParseParameters(Ast_Function* function, Token* open_paren, uint32 indent, Ast_Module* module)
{
	Token* closure = open_paren->closure;
	Token* token = open_paren+1;
	Array_Buffer<Ast_Variable> params = CreateArrayBuffer<Ast_Variable>(); // @Todo: Get lexer to count commas in parens?

	CheckScope(open_paren, indent, module);
	CheckScope(closure, indent, module);

	while (token < closure)
	{
		Ast_Variable param;
		ZeroMemory(&param);
		param.type = TYPE_NULL;
		param.ast_type = null;
		param.flags |= AST_VARIABLE_FLAG_PARAMETER;

		if (token->kind == TOKEN_COMMA)
			Error(module, token->location, "Empty parameters not allowed. (Remove redundant ',')\n");

		if (token->kind != TOKEN_IDENTIFIER_CASUAL)
		{
			if (token->kind == TOKEN_IDENTIFIER_FORMAL)
				Error(module, token->location, "Parameter names must start with a lowercase letter, not: '%'\n", token);
			else
				Error(module, token->location, "Parameter name missing, unexpected: '%'\n", token);
		}

		CheckScope(token, indent+1, module);
		param.name_token = token;
		param.name = token->identifier_string;
		token += 1;

		if (token->kind != TOKEN_COLON)
			Error(module, token->location, "Parameter type missing.\n");

		CheckScope(token, indent+1, module);
		token += 1;

		param.ast_type = StackAllocate<Ast_Type>(&module->stack);
		*param.ast_type = ParseType(token, indent+2, module);

		params.Add(param);

		if (token->kind != TOKEN_COMMA && token != closure)
			Error(module, token->location, "Expected ',' or ')', not: '%'\n", token);

		if (token->kind == TOKEN_COMMA)
		{
			CheckScope(token, indent, module);
			token += 1;
		}
	}

	function->parameters = params.Lock();
}

static Ast_BranchBlock ParseBranchBlock(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_BranchBlock branch_block;
	ZeroMemory(&branch_block);
	Array_Buffer<Ast_Branch> branches = CreateArrayBuffer<Ast_Branch>();

	Token* branch_block_begin_token = token;

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
				CheckScope(token, indent, module);
				token += 1;
				branch.if_condition = ParseExpression(token, indent+1, module, false);
			} break;

			case TOKEN_WHILE:
			{
				branch.kind = AST_BRANCH_WHILE;
				CheckScope(token, indent, module);
				token += 1;
				branch.while_condition = ParseExpression(token, indent+1, module, false);
			} break;

			case TOKEN_FOR:
			{
				CheckScope(token, indent, module);
				token += 1;

				if (token[0].kind == TOKEN_IDENTIFIER_FORMAL && token[1].kind == TOKEN_COLON)
					Error(module, token->location, "Variable names must start with a lowercase letter.\n");

				if (token[0].kind == TOKEN_IDENTIFIER_FORMAL && token[1].kind == TOKEN_IN)
					Error(module, token->location, "Iterator names must start with a lowercase letter.\n");


				if (token[0].kind == TOKEN_IDENTIFIER_CASUAL && token[1].kind == TOKEN_IN)
				{
					branch.kind = AST_BRANCH_FOR_RANGE;

					Ast_Variable* iterator = StackAllocate<Ast_Variable>(&module->stack);
					ZeroMemory(iterator);
					iterator->name_token = token;
					iterator->name = token->identifier_string;
					iterator->flags |= AST_VARIABLE_FLAG_ITERATOR;
					branch.for_range.iterator = iterator;

					CheckScope(token, indent+1, module);
					token += 1;

					CheckScope(token, indent, module);
					token += 1;

					CheckScope(token, indent+1, module);
					branch.for_range.range = ParseExpression(token, indent+1, module, false);

					if (token->kind == TOKEN_COMMA)
					{
						CheckScope(token, indent, module);
						token += 1;

						CheckScope(token, indent+1, module);
						branch.for_range.stride = ParseExpression(token, indent+1, module, false);
					}

					if (token->kind == TOKEN_WHERE)
					{
						CheckScope(token, indent, module);
						token += 1;

						CheckScope(token, indent, module);
						branch.for_range.filter = ParseExpression(token, indent+1, module, false);
					}
				}
				else if (token[0].kind == TOKEN_IDENTIFIER_CASUAL && token[1].kind == TOKEN_COLON)
				{
					branch.kind = AST_BRANCH_FOR_VERBOSE;

					Ast_Variable* variable = StackAllocate<Ast_Variable>(&module->stack);
					ZeroMemory(variable);

					variable->name_token = token;
					variable->name = token->identifier_string;

					CheckScope(token, indent+1, module);
					token += 1;

					CheckScope(token, indent, module);
					token += 1;

					if (token->kind != TOKEN_EQUAL)
					{
						CheckScope(token, indent+1, module);
						variable->ast_type = StackAllocate<Ast_Type>(&module->stack);
						*variable->ast_type = ParseType(token, indent, module);
					}

					branch.for_verbose.variable = variable;

					if (token->kind == TOKEN_EQUAL)
					{
						CheckScope(token, indent, module);
						token += 1;

						CheckScope(token, indent+1, module);
						variable->assignment = ParseExpression(token, indent+1, module, false);
					}

					if (token->kind != TOKEN_COMMA)
						Error(module, token->location, "Expected ',' and loop condition, not: '%'\n", token);

					CheckScope(token, indent, module);
					token += 1;

					if (token->kind == TOKEN_COLON)
						Error(module, token->location, "For loop missing condition expression.\n");

					CheckScope(token, indent+1, module);
					branch.for_verbose.condition = ParseExpression(token, indent+1, module, false);

					if (token->kind == TOKEN_COMMA)
					{
						CheckScope(token, indent, module);
						token += 1;

						if (token->kind == TOKEN_COLON)
							Error(module, token->location, "For loop stride missing\n");

						CheckScope(token, indent+1, module);
						branch.for_verbose.stride = ParseExpression(token, indent+1, module, false);
					}
				}
			} break;

			case TOKEN_COLON: branch.kind = AST_BRANCH_NAKED; break;

			default: Error(module, token->location, "Expected 'if', 'while', 'for' or ':' after '%' clause, not: '%'\n", clause_token, token);
		}

		if (token->kind != TOKEN_COLON)
			Error(module, token->location, "Expected ':' after branch, not: '%'\n", token);

		CheckScope(token, indent, module);
		token += 1;

		branch.code = ParseCode(token, indent+1, module);

		branches.Add(branch);
	} while ((token->kind == TOKEN_ELSE || token->kind == TOKEN_THEN) && IsCorrectScope(token, indent));

	branch_block.branches = branches.Lock();

	Ast_Branch* else_branch = null;
	Ast_Branch* then_branch = null;

	for (Ast_Branch* branch = branch_block.branches.End()-1; branch >= branch_block.branches.Begin(); branch--)
	{
		branch->else_branch = else_branch;
		branch->then_branch = then_branch;

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

static Ast_Statement ParseStatement(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Statement statement = Ast_Statement();
	ZeroMemory(&statement);

	if (token[0].kind == TOKEN_IDENTIFIER_FORMAL && token[1].kind == TOKEN_COLON)
		Error(module, token->location, "Variable names must start with a lowercase letter.\n");

	if (token[0].kind == TOKEN_IDENTIFIER_CASUAL && token[1].kind == TOKEN_COLON)
	{
		CheckScope(token+1, indent, module);

		statement.kind = AST_STATEMENT_VARIABLE_DECLARATION;
		statement.variable_declaration.name_token = token;
		statement.variable_declaration.name = token->identifier_string;

		token += 2;

		if (token->kind != TOKEN_EQUAL)
		{
			CheckScope(token, indent+1, module);
			statement.variable_declaration.ast_type = StackAllocate<Ast_Type>(&module->stack);
			*statement.variable_declaration.ast_type = ParseType(token, indent+1, module);
		}

		if (token->kind == TOKEN_EQUAL)
		{
			CheckScope(token, indent+1, module);
			token += 1;
			CheckScope(token, indent+1, module);
			statement.variable_declaration.assignment = ParseExpression(token, indent+1, module);
		}

		return statement;
	}
	else if (IsExpressionStarter(token->kind))
	{
		Ast_Expression* expression = ParseExpression(token, indent+1, module, true);

		if (IsAssignment(token->kind))
		{
			if      (token->kind == TOKEN_EQUAL)             statement.kind = AST_STATEMENT_ASSIGNMENT;
			else if (token->kind == TOKEN_PLUS_EQUAL)        statement.kind = AST_STATEMENT_ASSIGNMENT_ADD;
			else if (token->kind == TOKEN_MINUS_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_SUBTRACT;
			else if (token->kind == TOKEN_TIMES_EQUAL)       statement.kind = AST_STATEMENT_ASSIGNMENT_MULTIPLY;
			else if (token->kind == TOKEN_DIVIDE_EQUAL)      statement.kind = AST_STATEMENT_ASSIGNMENT_DIVIDE;
			else if (token->kind == TOKEN_EXPONENTIAL_EQUAL) statement.kind = AST_STATEMENT_ASSIGNMENT_EXPONENTIAL;

			CheckScope(token, indent+1, module);
			token += 1;

			statement.assignment.left  = expression;
			statement.assignment.token = token;

			CheckScope(token, indent+1, module);
			statement.assignment.right = ParseExpression(token, indent+1, module, false);
		}
		else
		{
			statement.kind = AST_STATEMENT_EXPRESSION;
			statement.expression = expression;
		}

		return statement;
	}
	else if (token->kind == TOKEN_IF || token->kind == TOKEN_FOR || token->kind == TOKEN_WHILE)
	{
		Ast_BranchBlock branch_block = ParseBranchBlock(token, indent, module);
		statement.kind = AST_STATEMENT_BRANCH_BLOCK;
		statement.branch_block = branch_block;

		return statement;
	}
	else if (token->kind == TOKEN_INC || token->kind == TOKEN_DEC)
	{
		statement.kind = token->kind == TOKEN_INC ? AST_STATEMENT_INCREMENT : AST_STATEMENT_DECREMENT;
		statement.increment.token = token;
		token += 1;

		if (!IsCorrectScope(token, indent+1) || token->kind == TOKEN_SEMICOLON)
			Error(module, statement.increment.token->location, "Expected expression after '%' keyword\n", statement.increment.token);

		statement.increment.expression = ParseExpression(token, indent+1, module);

		return statement;
	}
	else if (token->kind == TOKEN_DEFER)
	{
		statement.kind = AST_STATEMENT_DEFER;
		statement.defer.token = token;
		token += 1;

		if (token->kind != TOKEN_COLON)
			Error(module, token->location, "Invalid 'defer' statement, Expected ':', not: '%'\n", token);

		CheckScope(token, indent, module);
		token += 1;

		Ast_Code code = ParseCode(token, indent+1, module);
		statement.defer.code = code;

		return statement;
	}
	else if (token->kind == TOKEN_BREAK)
	{
		statement.kind = AST_STATEMENT_BREAK;
		statement.brk.token = token;
		token += 1;

		return statement;
	}
	else if (token->kind == TOKEN_RETURN)
	{
		statement.kind = AST_STATEMENT_RETURN;
		statement.ret.token = token;
		statement.ret.expression = null;
		token += 1;

		if (IsCorrectScope(token, indent+1))
		{
			statement.ret.expression = ParseExpression(token, indent+1, module, false);
		}

		return statement;
	}
	else if (token->kind == TOKEN_CLAIM)
	{
		statement.kind = AST_STATEMENT_CLAIM;
		statement.claim.token = token;
		token += 1;

		if (!IsCorrectScope(token, indent+1) || token->kind == TOKEN_SEMICOLON)
			Error(module, statement.increment.token->location, "Expected expression after '%' keyword\n", statement.increment.token);

		statement.claim.expression = ParseExpression(token, indent+1, module, false);

		return statement;
	}
	else
		Error(module, token->location, "Invalid statement starting with '%'\n", token);
}

static bool IsScopeTerminator(Token_Kind kind)
{
	return kind == TOKEN_SEMICOLON || kind == TOKEN_ELSE || kind == TOKEN_THEN;
}

static Ast_Code ParseCode(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Code code;
	ZeroMemory(&code);

	Array_Buffer<Ast_Statement> statements = CreateArrayBuffer<Ast_Statement>();
	Array_Buffer<Ast_Struct>    structs    = CreateArrayBuffer<Ast_Struct>();
	Array_Buffer<Ast_Enum>      enums      = CreateArrayBuffer<Ast_Enum>();
	Array_Buffer<Ast_Function>  functions  = CreateArrayBuffer<Ast_Function>();

	if (token->newline && token->indent > indent)
	{
	}

	while (IsCorrectScope(token, indent) && !IsScopeTerminator(token->kind))
	{
		if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStruct(token, indent, module);
			structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, indent, module);
			enums.Add(enumeration);
		}
		else if (IsIdentifier(token->kind) && token[1].kind == TOKEN_OPEN_PAREN
			&&  (token[1].closure[1].kind == TOKEN_COLON || token[1].closure[1].kind == TOKEN_ARROW))
		{
			if (token->kind != TOKEN_IDENTIFIER_FORMAL)
				Error(module, token->location, "Function names must start with an uppercase letter.\n", token);

			Ast_Function function = ParseFunction(token, indent, module);
			function.is_global = false;
			functions.Add(function);
		}
		else
		{
			Ast_Statement statement = ParseStatement(token, indent, module);
			statements.Add(statement);

			if (!token->newline && !IsScopeTerminator(token->kind))
				Error(module, token->location, "Expected ';' before end of statement, not: '%'.\n", token);

			if (token->kind == TOKEN_SEMICOLON && IsCorrectScope(token, indent))
			{
				token += 1;
			}
		}
	}

	if (token->indent > indent)
		Error(module, token->location, "Token '%' is on the wrong indentation.\n", token);

	code.statements = statements.Lock();
	code.scope.enums = enums.Lock();
	code.scope.structs = structs.Lock();
	code.scope.functions = functions.Lock();

	return code;
}

static Ast_Function ParseFunction(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Function function;
	ZeroMemory(&function);
	function.ir = null;
	function.name = token->identifier_string;
	function.name_token = token;
	token += 1;
	ParseParameters(&function, token, indent, module);
	token = token->closure+1;

	if (token->kind != TOKEN_ARROW && token->kind != TOKEN_COLON)
		Error(module, token->location, "Expected '->' or ':', not '%'\n", token);

	if (token->kind == TOKEN_ARROW)
	{
		CheckScope(token, indent+1, module);
		token += 1;
		function.ast_return_type = StackAllocate<Ast_Type>(&module->stack);
		*function.ast_return_type = ParseType(token, indent, module);
	}

	if (token->kind != TOKEN_COLON)
		Error(module, token->location, "Expected ':', not '%'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	function.code = ParseCode(token, indent+1, module);

	return function;
}

static Ast_Import ParseImport(Token*& token, uint32 indent, Ast_Module* module)
{
	Ast_Import import;
	import.token = token;
	token += 1;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
		Error(module, token->location, "Expected identifier after import token, instead got: '%'\n", token);

	CheckScope(token, 1, module);
	import.module = token;
	token += 1;

	if (token->kind == TOKEN_SEMICOLON)
	{
		CheckScope(token, 1, module);
		token += 1;
	}
	else if (IsCorrectScope(token, 1))
		Error(module, token->location, "Unexpected token after import statement: '%'\n", token);

	CheckScope(token, 0, module);

	return import;
}

static void ParseGlobalScope(Ast_Module* module)
{
	Token* token = &module->tokens[0];

	Array_Buffer<Ast_Import>   imports    = CreateArrayBuffer<Ast_Import>();
	Array_Buffer<Ast_Struct>   structs    = CreateArrayBuffer<Ast_Struct>();
	Array_Buffer<Ast_Enum>     enums      = CreateArrayBuffer<Ast_Enum>();
	Array_Buffer<Ast_Function> functions  = CreateArrayBuffer<Ast_Function>();

	while (token->kind != TOKEN_EOF)
	{
		if (token->kind == TOKEN_IMPORT)
		{
			Ast_Import import = ParseImport(token, 0, module);
			imports.Add(import);
		}
		else if (token->kind == TOKEN_STRUCT)
		{
			Ast_Struct structure = ParseStruct(token, 0, module);
			structs.Add(structure);
		}
		else if (token->kind == TOKEN_ENUM)
		{
			Ast_Enum enumeration = ParseEnum(token, 0, module);
			enums.Add(enumeration);
		}
		else if (IsIdentifier(token->kind) && token[1].kind == TOKEN_OPEN_PAREN)
		{
			if (token->kind != TOKEN_IDENTIFIER_FORMAL)
				Error(module, token->location, "Function names must start with an uppercase letter.\n", token);

			Ast_Function function = ParseFunction(token, 0, module);
			function.is_global = true;
			functions.Add(function);
		}
		else Error(module, token->location, "Unexpected token in global scope: '%'\n", token);
	}

	module->imports = imports.Lock();
	module->scope.functions = functions.Lock();
	module->scope.structs = structs.Lock();
	module->scope.enums = enums.Lock();
}


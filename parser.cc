#include "parser.h"
#include "error.h"
#include "general.h"
#include "ir.h"
#include "token.h"
#include "print.h"
#include "alloc.h"
#include "assert.h"
#include "array_buffer.h"

template<typename... Args>
[[noreturn]] void Parser::Error(String format, Args&&... args) {
	::Error(module, token->location, format, args...);
}

static bool IsSpecifier(TokenKind kind) {
	switch (kind) {
		case TOKEN_ASTERISK:
		case TOKEN_QUESTION_MARK:
		case TOKEN_OPEN_BRACKET:
			return true;

		default:
			return false;
	}
}

static bool IsTernaryOperator(TokenKind kind) {
	switch (kind) {
		case TOKEN_IF:
			return true;

		default:
			return false;
	}
}

static bool IsIdentifier(TokenKind kind) {
	switch (kind) {
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL:
			return true;

		default:
			return false;
	}
}

static bool IsBinaryOperator(TokenKind kind) {
	switch (kind) {
		case TOKEN_DOT:
		case TOKEN_ASTERISK:
		case TOKEN_DIVIDE:
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_AND:
		case TOKEN_OR:
		case TOKEN_CARET:
		case TOKEN_MOD:
		case TOKEN_AMPERSAND:
		case TOKEN_BAR:
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

static bool IsUnaryOperator(TokenKind kind) {
	switch (kind) {
		case TOKEN_ASTERISK:
		case TOKEN_AMPERSAND:
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_TILDE:
		case TOKEN_NOT:
		case TOKEN_EXCLAMATION_MARK:
			return true;

		default:
			return false;
	}
}

static bool IsAssignment(TokenKind kind) {
	switch (kind) {
		case TOKEN_EQUAL:
		case TOKEN_PLUS_EQUAL:
		case TOKEN_MINUS_EQUAL:
		case TOKEN_TIMES_EQUAL:
		case TOKEN_DIVIDE_EQUAL:
		case TOKEN_CARET_EQUAL:
			return true;

		default:
			return false;
	}
}

static bool IsPrimitive(TokenKind kind) {
	switch (kind) {
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
		case TOKEN_FLOAT32:
		case TOKEN_FLOAT64:
			return true;

		default:
			return false;
	}
}

static bool IsTerm(TokenKind kind) {
	switch (kind) {
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

static bool IsLiteral(TokenKind kind) {
	switch (kind) {
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

static bool IsExpressionStarter(TokenKind kind) {
	return IsUnaryOperator(kind)
		|| IsTerm(kind)
		|| kind == TOKEN_OPEN_PAREN
		|| kind == TOKEN_OPEN_BRACKET
		|| kind == TOKEN_OPEN_BRACE;
}

static s32 GetTernaryPrecedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_IF:
			return 8;

		default: {
			Assert("Invalid ternary operator.");
			Unreachable();
		}
	}
}

static s32 GetBinaryPrecedence(TokenKind kind, bool is_boosted) {
	switch (kind) {
		case TOKEN_DOT:
			return 15 << (is_boosted ? 16 : 0);

		case TOKEN_ASTERISK:
		case TOKEN_DIVIDE:
		case TOKEN_MOD:
			return 12 << (is_boosted ? 16 : 0);

		case TOKEN_PLUS:
		case TOKEN_MINUS:
			return 11 << (is_boosted ? 16 : 0);

		case TOKEN_CARET:
		case TOKEN_AMPERSAND:
		case TOKEN_BAR:
		case TOKEN_LEFT_SHIFT:
		case TOKEN_RIGHT_SHIFT:
			return 10 << (is_boosted ? 16 : 0);

		case TOKEN_AS:
			return 9 << (is_boosted ? 16 : 0);

		// case TOKEN_DOT_DOT:
		// 	return 7 << (is_boosted ? 16 : 0);

		case TOKEN_EQUAL:
		case TOKEN_NOT_EQUAL:
		case TOKEN_LESS:
		case TOKEN_LESS_OR_EQUAL:
		case TOKEN_GREATER:
		case TOKEN_GREATER_OR_EQUAL:
			return 6 << (is_boosted ? 16 : 0);

		case TOKEN_AND:
			return 5 << (is_boosted ? 16 : 0);

		case TOKEN_OR:
			return 4 << (is_boosted ? 16 : 0);

		default: {
			Assert("Invalid binary operator.");
			Unreachable();
		}
	}
}

static s32 GetUnaryPrecedence(TokenKind kind, bool is_boosted) {
	switch (kind) {
		case TOKEN_ASTERISK:
		case TOKEN_AMPERSAND:
		case TOKEN_EXCLAMATION_MARK:
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_TILDE:
			return 14 << (is_boosted ? 16 : 0);

		case TOKEN_NOT:
			return 5 << (is_boosted ? 16 : 0);

		default: {
			Assert("Invalid unary operator.");
			Unreachable();
		}
	}
}

static s32 GetPostfixPrecedence(TokenKind kind, bool is_boosted) {
	switch (kind) {
		case TOKEN_OPEN_PAREN:
		case TOKEN_OPEN_BRACKET:
			return 15 << (is_boosted ? 16 : 0);

		default: {
			Assert("Invalid postfix operator.");
			Unreachable();
		}
	}
}

static bool IsOperatorRightToLeft(TokenKind kind) {
	return false;
}

static bool IsPostfixOperator(TokenKind kind) {
	return kind == TOKEN_OPEN_PAREN
		|| kind == TOKEN_OPEN_BRACKET;
}

static bool IsOperator(TokenKind kind) {
	return IsBinaryOperator(kind)
		|| IsPostfixOperator(kind)
		|| IsTernaryOperator(kind);
}

static bool IsCorrectScope(Token* token, u32 indent) {
	return !token->IsNewLine() || token->indent == indent;
}

static void CheckScope(Token* token, u32 indent, Ast::Module* module) {
	if (!IsCorrectScope(token, indent))
		Error(module, token->location, "Invalid indentation.\n");
}

Ast::Struct Parser::ParseStruct(u32 indent) {
	token += 1;

	if (*token != TOKEN_IDENTIFIER_FORMAL) {
		if (IsIdentifier(token->kind))
			Error("Struct name must start with an uppercase letter.\n");
		else
			Error("Struct name missing\n");
	}

	CheckScope(token, indent+1, module);
	Ast::Struct structure = {
		.name = token->identifier_string,
		.name_token = token,
	};
	token += 1;

	if (*token != TOKEN_COLON)
		Error("Invalid struct declaration syntax, unexpected token %, Expected ':'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	ArrayBuffer<Ast::Struct_Member> members = CreateArrayBuffer<Ast::Struct_Member>();

 	while (IsCorrectScope(token, indent+1)) {
		if (*token == TOKEN_IDENTIFIER_CASUAL) {
			Ast::Struct_Member member;
			member.name = token->identifier_string;
			member.name_token = token;
			member.index = members.count;
			token += 1;

			if (*token != TOKEN_COLON)
				Error("Expected ':', not: '%'\n", token);

			CheckScope(token, indent+1, module);
			token += 1;

			member.ast_type = ParseType(indent+2);
			members.Add(member);

			if (!token->IsNewLine() && *token != TOKEN_SEMICOLON)
				Error("Unexpected token: % after struct member.\n", token);

			if (*token == TOKEN_SEMICOLON) {
				CheckScope(token, indent+1, module);
				token += 1;
			}

			if (*token == TOKEN_SEMICOLON && IsCorrectScope(token, indent+1)) {
				token += 1;
			}
		}
		else if (*token == TOKEN_IDENTIFIER_FORMAL)
			Error("Struct member names must start with a lowercase letter.\n");
		else
			Error("Unexpected token in struct: '%'\n", token);
	}

	structure.members = members.Lock();

	return structure;
}

Ast::Enum Parser::ParseEnum(u32 indent) {
	Ast::Enum enumeration;
	token += 1;

	if (*token != TOKEN_IDENTIFIER_FORMAL) {
		if (IsIdentifier(token->kind))
			Error("Enum names must start with an uppercase letter.\n");
		else
			Error("Enum name missing.\n");
	}

	CheckScope(token, indent+1, module);
	enumeration.name = token->identifier_string;
	enumeration.name_token = token;
	token += 1;

	if (*token != TOKEN_COLON)
		Error("Invalid enum declaration syntax, unexpected token '%', Expected ':'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	ArrayBuffer<Ast::Enum_Member> members = CreateArrayBuffer<Ast::Enum_Member>();

	while (IsCorrectScope(token, indent+1)) {
		if (*token == TOKEN_IDENTIFIER_FORMAL) {
			Ast::Enum_Member member;
			member.name_token = token;
			member.name = token->identifier_string;
			member.index = members.count;
			token += 1;

			if (*token != TOKEN_EQUAL)
				Error("Expected '=', not: '%'\n", token);

			CheckScope(token, indent+2, module);
			token += 1;

			if (*token == TOKEN_SEMICOLON)
				Error("Expected expression before ';'\n");

			CheckScope(token, indent+2, module);
			member.expression = ParseExpression(indent+2);

			if (!token->IsNewLine() && *token != TOKEN_SEMICOLON)
				Error("Unexpected token: '%' after enum member.\n", token);

			if (*token == TOKEN_SEMICOLON) {
				CheckScope(token, indent+1, module);
				token += 1;
			}

			members.Add(member);
			continue;
		}

		if (*token == TOKEN_IDENTIFIER_CASUAL)
			Error("Enum member names must start with a uppercase letter.\n");
		else
			Error("Unexpected token in enum: '%'\n", token);
	}

	enumeration.members = members.Lock();

	return enumeration;
}

static bool CanTakeNextOp(Token* token, bool assignment_break, s32 parent_precedence) {
	if (!IsOperator(token->kind))
		return false;

	// Don't consume assignment statement or variable declaration's equal token.
	if (*token == TOKEN_EQUAL && assignment_break)
		return false;

	// Adjust for right to left operators.
	parent_precedence -= IsOperatorRightToLeft(token->kind);

	if (IsBinaryOperator(token->kind))  return GetBinaryPrecedence(token->kind, token->IsLeftTight() || token->IsRightTight()) > parent_precedence;
	if (IsTernaryOperator(token->kind)) return GetTernaryPrecedence(token->kind)                                               > parent_precedence;
	if (IsPostfixOperator(token->kind)) return GetPostfixPrecedence(token->kind, token->IsLeftTight())                         > parent_precedence;

	AssertUnreachable();
}


Ast::Expression* Parser::ParseExpression(u32 indent, bool assignment_break, s32 parent_precedence) {
	Ast::Expression* left = null;

	Token* begin = token;

	if (IsUnaryOperator(token->kind)) {
		Ast::Expression::Kind kind;
		switch (token->kind) {
			case TOKEN_ASTERISK:         kind = Ast::Expression::UNARY_REFERENCE_OF; break;
			case TOKEN_AMPERSAND:        kind = Ast::Expression::UNARY_ADDRESS_OF;   break;
			case TOKEN_TILDE:            kind = Ast::Expression::UNARY_BITWISE_NOT;  break;
			case TOKEN_NOT:              kind = Ast::Expression::UNARY_NOT;          break;
			case TOKEN_MINUS:            kind = Ast::Expression::UNARY_MINUS;        break;
			case TOKEN_PLUS:             kind = Ast::Expression::UNARY_PLUS;         break;
			case TOKEN_EXCLAMATION_MARK: kind = Ast::Expression::UNARY_NOT;          break;
			default: Assert();
		}

		Ast::Expression_Unary* unary = stack.New<Ast::Expression_Unary>(kind, token);
		token += 1;
		CheckScope(token, indent, module);
		unary->subexpr = ParseExpression(indent, assignment_break, GetUnaryPrecedence(unary->op->kind, unary->op->IsRightTight()));
		left = unary;
	}
	else if (IsLiteral(token->kind)) {
		Ast::Expression_Literal* literal = stack.New<Ast::Expression_Literal>(token);

		switch (token->kind) {
			case TOKEN_LITERAL_INT:    literal->type = TYPE_INT64;  literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_INT8:   literal->type = TYPE_INT8;   literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_INT16:  literal->type = TYPE_INT16;  literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_INT32:  literal->type = TYPE_INT32;  literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_INT64:  literal->type = TYPE_INT64;  literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_UINT:   literal->type = TYPE_UINT64; literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_UINT8:  literal->type = TYPE_UINT8;  literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_UINT16: literal->type = TYPE_UINT16; literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_UINT32: literal->type = TYPE_UINT32; literal->value = IR::Constant(literal->token->literal_int); break;
			case TOKEN_LITERAL_UINT64: literal->type = TYPE_UINT64; literal->value = IR::Constant(literal->token->literal_int); break;

			case TOKEN_LITERAL_FLOAT:
			case TOKEN_LITERAL_FLOAT32: {
				literal->type = TYPE_FLOAT32;
				literal->value = IR::Constant((float32)literal->token->literal_float);
			} break;

			case TOKEN_LITERAL_FLOAT64: {
				literal->type = TYPE_FLOAT64;
				literal->value = IR::Constant(literal->token->literal_float);
			} break;

			case TOKEN_TRUE:  literal->type = TYPE_BOOL; literal->value = IR::Constant(1); break;
			case TOKEN_FALSE: literal->type = TYPE_BOOL; literal->value = IR::Constant(0); break;
			case TOKEN_NULL:  literal->type = TYPE_BYTE.GetPointer(); literal->value = IR::Constant(0); break;

			case TOKEN_LITERAL_STRING: {
				literal->type = TYPE_UINT8.GetFixedArray(literal->token->literal_string.length);
				literal->type = literal->type.GetReference();  // String literals are lvalues
				literal->value = IR::Constant(literal->token->literal_string.ToArray());
			} break;

			default: Assert(); Unreachable();
		}

		token += 1;
		left = literal;
	}
	else if (IsIdentifier(token->kind)) {
		Ast::Expression_Terminal* term = stack.New<Ast::Expression_Terminal>(token);
		token += 1;
		left = term;
	}
	else if (*token == TOKEN_OPEN_BRACKET) {
		Token* closure = token->closure;

		Ast::Expression_Array* array = stack.New<Ast::Expression_Array>();
		token += 1;

		if (!IsExpressionStarter(token->kind))
			Error("Expected expression after '['\n");

		CheckScope(token, indent, module);
		array->left = ParseExpression(indent+1);

		if (*token != TOKEN_DOT_DOT)
			Error("Expected '..' operator, not: \n", token);

		CheckScope(token, indent, module);
		token += 1;

		if (!IsExpressionStarter(token->kind))
			Error("Array extent not specified\n");

		CheckScope(token, indent+1, module);
		array->right = ParseExpression(indent+1);

		if (*token != TOKEN_CLOSE_BRACKET)
			Error("End of array expression missing, expected ']', not: '%'\n", token);

		CheckScope(token, indent, module);
		token += 1;

		left = array;
	}
	else if (*token == TOKEN_OPEN_PAREN) {
		Ast::Expression_Tuple* tuple = stack.New<Ast::Expression_Tuple>();

		Token* closure = token->closure;

		token += 1;

		ArrayBuffer<Ast::Expression*> elements = CreateArrayBuffer<Ast::Expression*>();

		if (closure[-1] == TOKEN_COMMA)
			Error("Expected expression after ','\n");

		while (token < closure) {
			CheckScope(token, indent+1, module);
			elements.Add(ParseExpression(indent+1));

			if (*token == TOKEN_COMMA) {
				CheckScope(token, indent, module);
				token += 1;
			}
			else if (token < closure) Error("Invalid expression, unexpected token: '%'\n", token);
		}

		tuple->elements = elements.Lock();

		CheckScope(token, indent, module);
		token = closure+1;
		left = tuple;
	}
	else if (*token == TOKEN_OPEN_BRACE) {
		Ast::Expression_Fixed_Array* fixed_array = stack.New<Ast::Expression_Fixed_Array>();

		Token* closure = token->closure;
		ArrayBuffer<Ast::Expression*> elements = CreateArrayBuffer<Ast::Expression*>();

		CheckScope(closure, indent, module);

		if (token+1 == closure)
			Error("Empty arrays literals aren't allowed.\n");

		token += 1;

		while (token < closure) {
			CheckScope(token, indent+1, module);
			Ast::Expression* expression = ParseExpression(indent+1, false);
			elements.Add(expression);
			CheckScope(token, indent, module);

			if (*token == TOKEN_COMMA) {
				token += 1;

				if (token == closure)
					Error("Expected expression after ',', not: '%'\n", token);
			}
		}

		fixed_array->elements = elements.Lock();

		token = closure + 1;
		left = fixed_array;
	}
	else {
		if (token->indent != indent)
			CheckScope(token, indent, module);

		Error("Invalid expression, expected term, got: '%'\n", token);
	}

	while (CanTakeNextOp(token, assignment_break, parent_precedence) && IsCorrectScope(token, indent)) {
		// @Indent does unary operators need to be treated differently? (Error check)
		if (*token == TOKEN_IF) {
			Token* if_token = token;
			token += 1;
			CheckScope(token, indent, module);

			Ast::Expression* middle = ParseExpression(indent, false);

			if (*token != TOKEN_ELSE)
				Error("Invalid 'if' expression, missing 'else' clause. Unexpected: '%'\n", token);

			Token* else_token = token;
			CheckScope(token, indent, module);
			token += 1;

			CheckScope(token, indent, module);
			Ast::Expression* right = ParseExpression(indent, assignment_break, GetTernaryPrecedence(TOKEN_IF));

			Ast::Expression_Ternary* if_else = stack.New<Ast::Expression_Ternary>(Ast::Expression::IF_ELSE, if_token, else_token);
			if_else->left = left;
			if_else->middle = middle;
			if_else->right = right;
			left = if_else;
		}
		else if (*token == TOKEN_AS) {
			Ast::Expression_As* as = stack.New<Ast::Expression_As>(token);
			as->expr = left;

			CheckScope(token, indent, module);
			token++;

			as->ast_type = ParseType(indent);

			left = as;
		}
		else if (*token == TOKEN_OPEN_PAREN) {
			Ast::Expression_Call* call = stack.New<Ast::Expression_Call>();
			call->function = left;

			Token* open = token;
			Token* closure = open->closure;

			CheckScope(token, indent, module);
			call->params = (Ast::Expression_Tuple*)ParseExpression(indent, false, GetPostfixPrecedence(token->kind, token->IsLeftTight()));

			token = closure + 1;
			left = call;
		}
		else if (*token == TOKEN_OPEN_BRACKET) {
			Token* open = token++;
			Token* closure = open->closure;

			Ast::Expression_Subscript* subscript = stack.New<Ast::Expression_Subscript>();
			subscript->array = left;

			if (token != closure) {
				CheckScope(token, indent+1, module);
				subscript->index = ParseExpression(indent+1);

				if (token != closure)
					Error("Expected ']', not: '%'\n", token);
			}
			else subscript->index = null;

			CheckScope(token, indent, module);
			token = closure + 1;

			left = subscript;
		}
		else {
			// @Indent I think the CheckScope needs to be here instead of at the start of ParseExpression (where we consume the term).
			Ast::Expression::Kind kind;
			switch (token->kind) {
				case TOKEN_EQUAL:            kind = Ast::Expression::BINARY_COMPARE_EQUAL;            break;
				case TOKEN_NOT_EQUAL:        kind = Ast::Expression::BINARY_COMPARE_NOT_EQUAL;        break;
				case TOKEN_LESS:             kind = Ast::Expression::BINARY_COMPARE_LESS;             break;
				case TOKEN_LESS_OR_EQUAL:    kind = Ast::Expression::BINARY_COMPARE_LESS_OR_EQUAL;    break;
				case TOKEN_GREATER:          kind = Ast::Expression::BINARY_COMPARE_GREATER;          break;
				case TOKEN_GREATER_OR_EQUAL: kind = Ast::Expression::BINARY_COMPARE_GREATER_OR_EQUAL; break;
				case TOKEN_AND:              kind = Ast::Expression::BINARY_AND;                      break;
				case TOKEN_OR:               kind = Ast::Expression::BINARY_OR;                       break;
				case TOKEN_DOT:              kind = Ast::Expression::BINARY_DOT;                      break;
				// case TOKEN_DOT_DOT:          kind = Ast::Expression::BINARY_RANGE;                    break;
				case TOKEN_PLUS:             kind = Ast::Expression::BINARY_ADD;                      break;
				case TOKEN_MINUS:            kind = Ast::Expression::BINARY_SUBTRACT;                 break;
				case TOKEN_ASTERISK:         kind = Ast::Expression::BINARY_MULTIPLY;                 break;
				case TOKEN_DIVIDE:           kind = Ast::Expression::BINARY_DIVIDE;                   break;
				case TOKEN_MOD:              kind = Ast::Expression::BINARY_MODULO;                   break;
				case TOKEN_CARET:            kind = Ast::Expression::BINARY_BITWISE_XOR;              break;
				case TOKEN_BAR:              kind = Ast::Expression::BINARY_BITWISE_OR;               break;
				case TOKEN_AMPERSAND:        kind = Ast::Expression::BINARY_BITWISE_AND;              break;
				case TOKEN_LEFT_SHIFT:       kind = Ast::Expression::BINARY_LEFT_SHIFT;               break;
				case TOKEN_RIGHT_SHIFT:      kind = Ast::Expression::BINARY_RIGHT_SHIFT;              break;
				default: Assert(); Unreachable();
			}

			Ast::Expression_Binary* binary = stack.New<Ast::Expression_Binary>(kind, token);
			binary->left = left;
			token++;
			CheckScope(token, indent, module);
			binary->right = ParseExpression(indent, assignment_break, GetBinaryPrecedence(binary->op->kind, binary->op->IsLeftTight() || binary->op->IsRightTight()));
			left = binary;
		}

		left->begin = begin;
		left->end = token;
	}

	return left;
}

static Token* GetEndOfTypeIfValid(Token* token) {
	while (IsSpecifier(token->kind)) {
		if (*token == TOKEN_OPEN_BRACKET) token = token->closure;
		token += 1;
	}

	if (IsPrimitive(token->kind) || *token == TOKEN_IDENTIFIER_FORMAL)
		return token + 1;

	if (*token == TOKEN_OPEN_PAREN) {
		token = token->closure + 1;

		if (*token == TOKEN_ARROW)
			token = GetEndOfTypeIfValid(token + 1);

		return token;
	}

	return null;
}

Ast::Type Parser::ParseType(u32 indent) {
	Ast::Type type = { };

	ArrayBuffer<Ast::Specifier> specifiers = CreateArrayBuffer<Ast::Specifier>();

	while (IsSpecifier(token->kind)) {
		Ast::Specifier specifier;
		specifier.token = token;
		specifier.size_expression = null;
		CheckScope(token, indent, module);

		if (*token == TOKEN_OPEN_BRACKET) {
			specifier.kind = Ast::SPECIFIER_ARRAY;

			Token* closure = token->closure;
			CheckScope(closure, indent, module);
			token += 1;

			if (token != closure) {
				specifier.kind = Ast::SPECIFIER_FIXED_ARRAY;
				specifier.size_expression = ParseExpression(indent+1);
			}

			if (token != closure)
				Error("Expected ']', not: %\n", token);

			token = closure + 1;
		}
		else if (*token == TOKEN_ASTERISK) {
			specifier.kind = Ast::SPECIFIER_POINTER;
			token += 1;
		}
		else if (*token == TOKEN_QUESTION_MARK) {
			specifier.kind = Ast::SPECIFIER_OPTIONAL;
			token += 1;
		}

		specifiers.Add(specifier);
	}

	type.specifiers = specifiers.Lock();
	type.basetype.token = token;

	if (IsPrimitive(token->kind)) {
		CheckScope(token, indent, module);
		type.basetype.kind = Ast::BASETYPE_PRIMITIVE;
		token += 1;
	}
	else if (*token == TOKEN_IDENTIFIER_FORMAL) {
		CheckScope(token, indent, module);
		type.basetype.kind = Ast::BASETYPE_USERTYPE;
		token += 1;
	}
	else if (*token == TOKEN_OPEN_PAREN && token->closure[1] == TOKEN_ARROW) {
		Token* closure = token->closure;
		CheckScope(token, indent, module);
		CheckScope(closure, indent, module);
		CheckScope(closure+1, indent, module);
		type.basetype.kind = Ast::BASETYPE_FUNCTION;
		type.basetype.function.input = null;
		type.basetype.function.output = null;
		token += 1;

		List<Ast::Type> params = null;

		while (token != closure) {
			Ast::Type param = ParseType(indent+1);
			params.Add(param);

			if (*token == TOKEN_COMMA) {
				CheckScope(token, indent, module);
				token += 1;

				if (token == closure)
					Error("Expected type after ','\n");
			}
		}

		token = closure+2;

		if (params.count == 0) {
			type.basetype.function.input = null;
		}
		else {
			type.basetype.function.input = stack.Allocate<Ast::Type>();
			*type.basetype.function.input = {
				.basetype = { .kind = Ast::BASETYPE_TUPLE, .tuple = params.ToArray() },
			};
		}

		// () -> () -> XXX
		if (*token == TOKEN_OPEN_PAREN && token[1] == TOKEN_CLOSE_PAREN && token->closure[1] != TOKEN_ARROW) {
			type.basetype.function.output = null;
			token = token->closure+1;
		}
		else {
			type.basetype.function.output = stack.Allocate<Ast::Type>();
			*type.basetype.function.output = ParseType(indent);
		}

	}
	else if (*token == TOKEN_OPEN_PAREN) {
		Token* closure = token->closure;
		CheckScope(token, indent, module);
		CheckScope(closure, indent, module);
		type.basetype.kind = Ast::BASETYPE_TUPLE;
		type.basetype.tuple = null;
		token += 1;

		List<Ast::Type> members = null;

		while (token != closure) {
			members.Add(ParseType(indent+1));

			if (*token == TOKEN_COMMA) {
				CheckScope(token, indent, module);
				token += 1;

				if (token == closure)
					Error("Expected type after ','\n");
			}
		}

		type.basetype.tuple = members.ToArray();

		token = closure+1;
	}
	else
		Error("Expected type, not: '%'\n", token);

	return type;
}

void Parser::ParseParameters(Ast::Function* function, Token* open_paren, u32 indent) {
	Token* closure = open_paren->closure;
	Token* cursor = open_paren+1;
	ArrayBuffer<Ast::Variable> params = CreateArrayBuffer<Ast::Variable>(); // @Todo: Get lexer to count commas in parens?

	CheckScope(open_paren, indent, module);
	CheckScope(closure, indent, module);

	while (cursor < closure) {
		Ast::Variable param = { .flags = Ast::VARIABLE_FLAG_PARAMETER };

		if (*cursor == TOKEN_COMMA)
			::Error(module, cursor->location,"Empty parameters not allowed. (Remove redundant ',')\n");

		if (*cursor != TOKEN_IDENTIFIER_CASUAL) {
			if (*cursor == TOKEN_IDENTIFIER_FORMAL)
				::Error(module, cursor->location,"Parameter names must start with a lowercase letter, not: '%'\n", cursor);
			else
				::Error(module, cursor->location,"Parameter name missing, unexpected: '%'\n", cursor);
		}

		CheckScope(cursor, indent+1, module);
		param.name_token = cursor;
		param.name = cursor->identifier_string;
		cursor += 1;

		if (*cursor != TOKEN_COLON)
			::Error(module, cursor->location,"Parameter type missing.\n");

		CheckScope(cursor, indent+1, module);
		cursor += 1;

		param.ast_type = stack.Allocate<Ast::Type>();
		Token* saved = token;
		token = cursor;
		*param.ast_type = ParseType(indent+2);
		cursor = token;
		token = saved;

		params.Add(param);

		if (*cursor != TOKEN_COMMA && cursor != closure)
			::Error(module, cursor->location,"Expected ',' or ')', not: '%'\n", cursor);

		if (*cursor == TOKEN_COMMA) {
			CheckScope(cursor, indent, module);
			cursor += 1;
		}
	}

	function->params = params.Lock();
}

Ast::BranchBlock Parser::ParseBranchBlock(u32 indent) {
	Ast::BranchBlock branch_block = { };
	ArrayBuffer<Ast::Branch> branches = CreateArrayBuffer<Ast::Branch>();

	Token* branch_block_begin_token = token;

	do {
		Ast::Branch branch = { };

		Token* clause_token = token;

		if (*token == TOKEN_ELSE) {
			branch.clause = Ast::BRANCH_CLAUSE_ELSE;
			token += 1;
		}
		else if (*token == TOKEN_THEN) {
			branch.clause = Ast::BRANCH_CLAUSE_THEN;
			token += 1;
		}
		else branch.clause = Ast::BRANCH_CLAUSE_INIT;

		switch (token->kind) {
			case TOKEN_IF: {
				branch.kind = Ast::BRANCH_IF;
				CheckScope(token, indent, module);
				token += 1;
				branch.if_condition = ParseExpression(indent+1, false);
			} break;

			case TOKEN_WHILE: {
				branch.kind = Ast::BRANCH_WHILE;
				CheckScope(token, indent, module);
				token += 1;
				branch.while_condition = ParseExpression(indent+1, false);
			} break;

			case TOKEN_FOR: {
				CheckScope(token, indent, module);
				token += 1;

				if (token[0] == TOKEN_IDENTIFIER_FORMAL && token[1] == TOKEN_COLON)
					Error("Variable names must start with a lowercase letter.\n");

				if (token[0] == TOKEN_IDENTIFIER_FORMAL && token[1] == TOKEN_IN)
					Error("Iterator names must start with a lowercase letter.\n");

				// Todo: Allow user to declare the type
				// for n : uint in signed_nums:

				if (token[0] == TOKEN_IDENTIFIER_CASUAL && token[1] == TOKEN_IN) {
					branch.kind = Ast::BRANCH_FOR_RANGE;

					Ast::Variable* iterator = stack.Allocate<Ast::Variable>();
					*iterator = {
						.name       = token->identifier_string,
						.name_token = token,
						.flags      = Ast::VARIABLE_FLAG_ITERATOR,
					};
					branch.for_range.iterator = iterator;

					CheckScope(token, indent+1, module);
					token += 1;

					CheckScope(token, indent, module);
					token += 1;

					CheckScope(token, indent+1, module);
					branch.for_range.range = ParseExpression(indent+1, false);

					if (*token == TOKEN_WHERE) {
						CheckScope(token, indent, module);
						token += 1;

						CheckScope(token, indent, module);
						branch.for_range.filter = ParseExpression(indent+1, false);
					}
				}
				else if (token[0] == TOKEN_IDENTIFIER_CASUAL && token[1] == TOKEN_COLON) {
					branch.kind = Ast::BRANCH_FOR_VERBOSE;

					Ast::Variable* variable = stack.Allocate<Ast::Variable>();
					*variable = {
						.name       = token->identifier_string,
						.name_token = token,
					};

					CheckScope(token, indent+1, module);
					token += 1;

					CheckScope(token, indent, module);
					token += 1;

					if (*token != TOKEN_EQUAL) {
						CheckScope(token, indent+1, module);
						variable->ast_type = stack.Allocate<Ast::Type>();
						*variable->ast_type = ParseType(indent);
					}

					branch.for_verbose.variable = variable;

					if (*token == TOKEN_EQUAL) {
						CheckScope(token, indent, module);
						token += 1;

						CheckScope(token, indent+1, module);
						variable->assignment = ParseExpression(indent+1, false);
					}

					if (*token != TOKEN_COMMA)
						Error("Expected ',' and loop condition, not: '%'\n", token);

					CheckScope(token, indent, module);
					token += 1;

					if (*token == TOKEN_COLON)
						Error("For loop missing condition expression.\n");

					CheckScope(token, indent+1, module);
					branch.for_verbose.condition = ParseExpression(indent+1, false);

					if (*token == TOKEN_COMMA) {
						CheckScope(token, indent, module);
						token += 1;

						if (*token == TOKEN_COLON)
							Error("For loop stride missing\n");

						CheckScope(token, indent+1, module);
						branch.for_verbose.next = ParseExpression(indent+1, false);
					}
				}
			} break;

			case TOKEN_COLON: branch.kind = Ast::BRANCH_NAKED; break;

			default: Error("Expected 'if', 'while', 'for' or ':' after '%' clause, not: '%'\n", clause_token, token);
		}

		if (*token != TOKEN_COLON)
			Error("Expected ':' after branch, not: '%'\n", token);

		CheckScope(token, indent, module);
		token += 1;

		branch.code = ParseCode(indent+1);

		branches.Add(branch);
	} while ((*token == TOKEN_ELSE || *token == TOKEN_THEN) && IsCorrectScope(token, indent));

	branch_block.branches = branches.Lock();

	Ast::Branch* else_branch = null;
	Ast::Branch* then_branch = null;

	for (Ast::Branch* branch = branch_block.branches.End()-1; branch >= branch_block.branches.Begin(); branch--) {
		branch->else_branch = else_branch;
		branch->then_branch = then_branch;

		if (branch->clause == Ast::BRANCH_CLAUSE_ELSE) else_branch = branch;
		if (branch->clause == Ast::BRANCH_CLAUSE_THEN) then_branch = branch;
	}

	return branch_block;
}

Ast::Statement Parser::ParseExpressionStatement(u32 indent) {
	Ast::Expression* expression = ParseExpression(indent+1, true);
	Ast::Statement statement = { };

	if (!IsAssignment(token->kind)) {
		statement.kind = Ast::STATEMENT_EXPRESSION;
		statement.expression = expression;
		return statement;
	}

	if (*token == TOKEN_EQUAL)        statement.kind = Ast::STATEMENT_ASSIGNMENT;
	if (*token == TOKEN_PLUS_EQUAL)   statement.kind = Ast::STATEMENT_ASSIGNMENT_ADD;
	if (*token == TOKEN_MINUS_EQUAL)  statement.kind = Ast::STATEMENT_ASSIGNMENT_SUBTRACT;
	if (*token == TOKEN_TIMES_EQUAL)  statement.kind = Ast::STATEMENT_ASSIGNMENT_MULTIPLY;
	if (*token == TOKEN_DIVIDE_EQUAL) statement.kind = Ast::STATEMENT_ASSIGNMENT_DIVIDE;
	if (*token == TOKEN_CARET_EQUAL)  statement.kind = Ast::STATEMENT_ASSIGNMENT_XOR;

	CheckScope(token, indent+1, module);
	token += 1;

	statement.assignment.left  = expression;
	statement.assignment.token = token;

	CheckScope(token, indent+1, module);
	statement.assignment.right = ParseExpression(indent+1, false);

	return statement;
}

Ast::Statement Parser::ParseVariableDeclaration(u32 indent) {
	Ast::Statement statement = { };
	CheckScope(token+1, indent, module);

	statement.kind = Ast::STATEMENT_VARIABLE_DECLARATION;
	statement.vardecl.name_token = token;
	statement.vardecl.name = token->identifier_string;

	token += 2;

	if (*token != TOKEN_EQUAL) {
		CheckScope(token, indent+1, module);
		statement.vardecl.ast_type = stack.Allocate<Ast::Type>();
		*statement.vardecl.ast_type = ParseType(indent+1);
	}

	if (*token == TOKEN_EQUAL) {
		CheckScope(token, indent+1, module);
		token += 1;
		CheckScope(token, indent+1, module);
		statement.vardecl.assignment = ParseExpression(indent+1);
	}

	return statement;
}

Ast::Statement Parser::ParseBranchBlockStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = Ast::STATEMENT_BRANCH_BLOCK;
	statement.branch_block = ParseBranchBlock(indent);
	return statement;
}

Ast::Statement Parser::ParseIncDecStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = *token == TOKEN_INC ? Ast::STATEMENT_INCREMENT : Ast::STATEMENT_DECREMENT;
	statement.increment.token = token;
	token += 1;

	if (!IsCorrectScope(token, indent+1) || *token == TOKEN_SEMICOLON)
		::Error(module, statement.increment.token->location,"Expected expression after '%' keyword\n", statement.increment.token);

	statement.increment.expression = ParseExpression(indent+1);
	return statement;
}

Ast::Statement Parser::ParseDeferStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = Ast::STATEMENT_DEFER;
	statement.defer.token = token;
	token += 1;

	if (*token != TOKEN_COLON)
		Error("Invalid 'defer' statement, Expected ':', not: '%'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	statement.defer.code = ParseCode(indent+1);
	return statement;
}

Ast::Statement Parser::ParseBreakStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = Ast::STATEMENT_BREAK;
	statement.brk.token = token;
	token += 1;
	return statement;
}

Ast::Statement Parser::ParseReturnStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = Ast::STATEMENT_RETURN;
	statement.ret.token = token;
	statement.ret.expr = null;
	token += 1;

	if (IsCorrectScope(token, indent+1))
		statement.ret.expr = ParseExpression(indent+1, false);

	return statement;
}

Ast::Statement Parser::ParseClaimStatement(u32 indent) {
	Ast::Statement statement = { };
	statement.kind = Ast::STATEMENT_CLAIM;
	statement.claim.token = token;
	token += 1;

	if (!IsCorrectScope(token, indent+1) || *token == TOKEN_SEMICOLON)
		::Error(module, statement.increment.token->location,"Expected expression after '%' keyword\n", statement.increment.token);

	statement.claim.expr = ParseExpression(indent+1, false);
	return statement;
}

Ast::Statement Parser::ParseStatement(u32 indent) {
	if (token[0] == TOKEN_IDENTIFIER_FORMAL && token[1] == TOKEN_COLON)
		Error("Variable names must start with a lowercase letter.\n");

	if (token[0] == TOKEN_IDENTIFIER_CASUAL && token[1] == TOKEN_COLON)
		return ParseVariableDeclaration(indent);

	if (IsExpressionStarter(token->kind))
		return ParseExpressionStatement(indent);

	if (*token == TOKEN_IF)     return ParseBranchBlockStatement(indent);
	if (*token == TOKEN_FOR)    return ParseBranchBlockStatement(indent);
	if (*token == TOKEN_WHILE)  return ParseBranchBlockStatement(indent);
	if (*token == TOKEN_INC)    return ParseIncDecStatement(indent);
	if (*token == TOKEN_DEC)    return ParseIncDecStatement(indent);
	if (*token == TOKEN_DEFER)  return ParseDeferStatement(indent);
	if (*token == TOKEN_BREAK)  return ParseBreakStatement(indent);
	if (*token == TOKEN_RETURN) return ParseReturnStatement(indent);
	if (*token == TOKEN_CLAIM)  return ParseClaimStatement(indent);

	Error("Invalid statement starting with '%'\n", token);
}

static bool IsScopeTerminator(TokenKind kind) {
	return kind == TOKEN_SEMICOLON || kind == TOKEN_ELSE || kind == TOKEN_THEN;
}

Ast::Code Parser::ParseCode(u32 indent) {
	Ast::Code code = { };

	ArrayBuffer<Ast::Statement> statements = CreateArrayBuffer<Ast::Statement>();
	ArrayBuffer<Ast::Struct>    structs    = CreateArrayBuffer<Ast::Struct>();
	ArrayBuffer<Ast::Enum>      enums      = CreateArrayBuffer<Ast::Enum>();
	ArrayBuffer<Ast::Function>  functions  = CreateArrayBuffer<Ast::Function>();

	while (IsCorrectScope(token, indent) && !IsScopeTerminator(token->kind)) {
		if (*token == TOKEN_STRUCT) {
			Ast::Struct structure = ParseStruct(indent);
			structs.Add(structure);
			continue;
		}

		if (*token == TOKEN_ENUM) {
			Ast::Enum enumeration = ParseEnum(indent);
			enums.Add(enumeration);
			continue;
		}

		if (IsIdentifier(token->kind) && token[1] == TOKEN_OPEN_PAREN && (token[1].closure[1] == TOKEN_COLON || token[1].closure[1] == TOKEN_ARROW)) {
			if (*token != TOKEN_IDENTIFIER_FORMAL)
				Error("Function names must start with an uppercase letter.\n", token);

			Ast::Function function = ParseFunction(indent);
			function.is_global = false;
			functions.Add(function);
			continue;
		}

		Ast::Statement statement = ParseStatement(indent);
		statements.Add(statement);

		if (!token->IsNewLine() && !IsScopeTerminator(token->kind))
			Error("Expected ';' before end of statement, not: '%'.\n", token);

		if (*token == TOKEN_SEMICOLON && IsCorrectScope(token, indent))
			token += 1;
	}

	if (token->indent > indent)
		Error("Token '%' is on the wrong indentation.\n", token);

	code.statements = statements.Lock();
	code.scope.enums = enums.Lock();
	code.scope.structs = structs.Lock();
	code.scope.functions = functions.Lock();

	return code;
}

Ast::Function Parser::ParseFunction(u32 indent) {
	Ast::Function function = {
		.name       = token->identifier_string,
		.name_token = token,
	};

	token += 1;
	ParseParameters(&function, token, indent);
	token = token->closure+1;

	if (*token != TOKEN_ARROW && *token != TOKEN_COLON)
		Error("Expected '->' or ':', not '%'\n", token);

	if (*token == TOKEN_ARROW) {
		CheckScope(token, indent+1, module);
		token += 1;
		function.ast_return_type = stack.Allocate<Ast::Type>();
		*function.ast_return_type = ParseType(indent);
	}

	if (*token != TOKEN_COLON)
		Error("Expected ':', not '%'\n", token);

	CheckScope(token, indent, module);
	token += 1;

	function.code = ParseCode(indent+1);

	return function;
}

Ast::Import Parser::ParseImport(u32 indent) {
	Ast::Import import;
	import.token = token;
	token += 1;

	if (*token != TOKEN_IDENTIFIER_FORMAL)
		Error("Expected identifier after import token, instead got: '%'\n", token);

	CheckScope(token, 1, module);
	import.module = token;
	token += 1;

	if (*token == TOKEN_SEMICOLON) {
		CheckScope(token, 1, module);
		token += 1;
	}
	else if (IsCorrectScope(token, 1))
		Error("Unexpected token after import statement: '%'\n", token);

	CheckScope(token, 0, module);

	return import;
}

void Parser::ParseGlobalScope() {
	ArrayBuffer<Ast::Import>   imports    = CreateArrayBuffer<Ast::Import>();
	ArrayBuffer<Ast::Struct>   structs    = CreateArrayBuffer<Ast::Struct>();
	ArrayBuffer<Ast::Enum>     enums      = CreateArrayBuffer<Ast::Enum>();
	ArrayBuffer<Ast::Function> functions  = CreateArrayBuffer<Ast::Function>();

	while (*token != TOKEN_EOF) {
		if (*token == TOKEN_IMPORT) {
			Ast::Import import = ParseImport(0);
			imports.Add(import);
			continue;
		}

		if (*token == TOKEN_STRUCT) {
			Ast::Struct structure = ParseStruct(0);
			structs.Add(structure);
			continue;
		}

		if (*token == TOKEN_ENUM) {
			Ast::Enum enumeration = ParseEnum(0);
			enums.Add(enumeration);
			continue;
		}

		if (IsIdentifier(token->kind) && token[1] == TOKEN_OPEN_PAREN) {
			if (*token != TOKEN_IDENTIFIER_FORMAL)
				Error("Function names must start with an uppercase letter.\n", token);

			Ast::Function func = ParseFunction(0);
			func.is_global = true;
			functions.Add(func);
			continue;
		}

		Error("Unexpected token in global scope: '%'\n", token);
	}

	module->imports = imports.Lock();
	module->scope.functions = functions.Lock();
	module->scope.structs = structs.Lock();
	module->scope.enums = enums.Lock();
}


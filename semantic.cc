#include "semantic.h"

static Intrinsic intrinsics[INTRINSIC_COUNT];

static void InitIntrinsics() {
	intrinsics[INTRINSIC_SYSTEM_CALL] = {
		"SystemCall",
		GetFunctionType(
			GetTuple({ TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64 }),
			TYPE_INT64
		),
		[](void* input, void* output) { *(s64*)output = 0; },
	};
}

static IntrinsicID FindIntrinsic(String name, TypeID input_type) {
	for (u64 id = 0; id < INTRINSIC_COUNT; id++) {
		TypeInfo* info = intrinsics[id].type.GetInfo();

		if (name == intrinsics[id].name && input_type.CanCast(CAST_IMPLICIT, info->function_info.input))
			return (IntrinsicID)id;
	}

	return INTRINSIC_INVALID;
}

void Scanner::SemanticParse() {
	ScanScope(&module->scope);
}

void Scanner::ScanScope(Ast::Scope* scope) {
	for (auto& s : scope->structs)
		for (auto& member : s.members)
			if (member.ast_type.specifiers.length)
				for (auto& spec : member.ast_type.specifiers)
					if (spec.size_expression)
						ScanExpression(spec.size_expression);

	for (auto& e : scope->enums)
		for (auto& member : e.members)
			if (member.expression)
				ScanExpression(member.expression);

	for (auto& f : scope->functions)
		ScanFunction(&f);
}

void Scanner::ScanFunction(Ast::Function* function) {
	for (auto& param : function->params)
		if (param.assignment)
			ScanExpression(param.assignment);

	ScanCode(&function->code);
}

void Scanner::ScanCode(Ast::Code* code) {
	ScanScope(&code->scope);

	for (auto& statement : code->statements)
		ScanStatement(&statement);
}

void Scanner::ScanStatement(Ast::Statement* statement) {
	switch (statement->kind) {
		case Ast::STATEMENT_EXPRESSION:
			ScanExpression(statement->expression);
			break;

		case Ast::STATEMENT_VARIABLE_DECLARATION:
			if (statement->vardecl.assignment)
				ScanExpression(statement->vardecl.assignment);
			break;

		case Ast::STATEMENT_ASSIGNMENT:
		case Ast::STATEMENT_ASSIGNMENT_ADD:
		case Ast::STATEMENT_ASSIGNMENT_SUBTRACT:
		case Ast::STATEMENT_ASSIGNMENT_MULTIPLY:
		case Ast::STATEMENT_ASSIGNMENT_DIVIDE:
		case Ast::STATEMENT_ASSIGNMENT_XOR:
			ScanExpression(statement->assignment.left);
			ScanExpression(statement->assignment.right);
			break;

		case Ast::STATEMENT_INCREMENT:
		case Ast::STATEMENT_DECREMENT:
			ScanExpression(statement->increment.expression);
			break;

		case Ast::STATEMENT_RETURN:
			if (statement->ret.expr)
				ScanExpression(statement->ret.expr);
			break;

		case Ast::STATEMENT_BREAK:
			break;

		case Ast::STATEMENT_CLAIM:
			ScanExpression(statement->claim.expr);
			break;

		case Ast::STATEMENT_BRANCH_BLOCK:
			for (auto& branch : statement->branch_block.branches) {
				switch (branch.kind) {
					case Ast::BRANCH_IF:
						ScanExpression(branch.if_condition);
						break;
					case Ast::BRANCH_WHILE:
						ScanExpression(branch.while_condition);
						break;
					case Ast::BRANCH_FOR_RANGE:
						ScanExpression(branch.for_range.range);
						if (branch.for_range.filter)
							ScanExpression(branch.for_range.filter);
						break;
					case Ast::BRANCH_FOR_VERBOSE:
						if (branch.for_verbose.condition)
							ScanExpression(branch.for_verbose.condition);
						if (branch.for_verbose.next)
							ScanExpression(branch.for_verbose.next);
						break;
					case Ast::BRANCH_NAKED:
						break;
				}
				ScanCode(&branch.code);
			}
			break;

		case Ast::STATEMENT_DEFER:
			ScanCode(&statement->defer.code);
			break;
	}
}

void Scanner::ScanExpression(Ast::Expression* expression) {
	using Kind = Ast::Expression::Kind;

	switch (expression->kind) {
		// Terminals - leaf nodes, nothing to recurse into.
		case Kind::TERMINAL_NAME:
		case Kind::TERMINAL_FUNCTION:
		case Kind::TERMINAL_INTRINSIC:
		case Kind::TERMINAL_LITERAL:
		case Kind::TERMINAL_VARIABLE:
		case Kind::TERMINAL_STRUCT:
		case Kind::TERMINAL_ENUM:
		case Kind::TERMINAL_PRIMITIVE:
		case Kind::TERMINAL_STRUCT_MEMBER:
		case Kind::TERMINAL_ENUM_MEMBER:
		case Kind::TERMINAL_ARRAY_LENGTH:
		case Kind::TERMINAL_ARRAY_BEGIN:
		case Kind::TERMINAL_ARRAY_END:
			break;

		// Unary
		case Kind::UNARY_BITWISE_NOT:
		case Kind::UNARY_NOT:
		case Kind::UNARY_MINUS:
		case Kind::UNARY_PLUS:
		case Kind::UNARY_REFERENCE_OF:
		case Kind::UNARY_ADDRESS_OF:
			ScanExpression(((Ast::Expression_Unary*)expression)->subexpr);
			break;

		case Kind::IMPLICIT_CAST:
			ScanExpression(((Ast::Expression_Implicit_Cast*)expression)->subexpr);
			break;

		// Binary
		case Kind::BINARY_COMPARE_EQUAL:
		case Kind::BINARY_COMPARE_NOT_EQUAL:
		case Kind::BINARY_COMPARE_LESS:
		case Kind::BINARY_COMPARE_LESS_OR_EQUAL:
		case Kind::BINARY_COMPARE_GREATER:
		case Kind::BINARY_COMPARE_GREATER_OR_EQUAL:
		case Kind::BINARY_DOT:
		case Kind::BINARY_ADD:
		case Kind::BINARY_SUBTRACT:
		case Kind::BINARY_MULTIPLY:
		case Kind::BINARY_DIVIDE:
		case Kind::BINARY_MODULO:
		case Kind::BINARY_BITWISE_OR:
		case Kind::BINARY_BITWISE_XOR:
		case Kind::BINARY_BITWISE_AND:
		case Kind::BINARY_LEFT_SHIFT:
		case Kind::BINARY_RIGHT_SHIFT:
		case Kind::BINARY_AND:
		case Kind::BINARY_OR: {
			auto* binary = (Ast::Expression_Binary*)expression;
			ScanExpression(binary->left);
			ScanExpression(binary->right);
			break;
		}

		// Ternary
		case Kind::IF_ELSE: {
			auto* ternary = (Ast::Expression_Ternary*)expression;
			ScanExpression(ternary->left);
			ScanExpression(ternary->middle);
			ScanExpression(ternary->right);
			break;
		}

		// Call
		case Kind::CALL: {
			auto* call = (Ast::Expression_Call*)expression;
			ScanExpression(call->function);
			ScanExpression(call->params);
			break;
		}

		case Kind::DOT_CALL: {
			auto* dot_call = (Ast::Expression_Dot_Call*)expression;
			ScanExpression(dot_call->dot);
			ScanExpression(dot_call->params);
			break;
		}

		// Subscript
		case Kind::SUBSCRIPT: {
			auto* subscript = (Ast::Expression_Subscript*)expression;
			ScanExpression(subscript->array);
			ScanExpression(subscript->index);
			break;
		}

		// Compound literals
		case Kind::TUPLE: {
			auto* tuple = (Ast::Expression_Tuple*)expression;
			for (auto* element : tuple->elements)
				ScanExpression(element);
			break;
		}

		case Kind::FIXED_ARRAY: {
			auto* fixed_array = (Ast::Expression_Fixed_Array*)expression;
			for (auto* element : fixed_array->elements)
				ScanExpression(element);
			break;
		}

		case Kind::ARRAY: {
			auto* array = (Ast::Expression_Array*)expression;
			ScanExpression(array->left);
			ScanExpression(array->right);
			break;
		}

		// Cast
		case Kind::AS: {
			auto* as = (Ast::Expression_As*)expression;
			ScanExpression(as->expr);
			break;
		}

		// Lambda
		case Kind::LAMBDA:
			// @Todo: Scan lambda body when lambdas are fully supported.
			break;
	}
}

#include "ast.h"
#include "print.h"

static const char* GetExpressionKindName(Ast::Expression::Kind kind) {
	switch (kind) {
		case Ast::Expression::TERMINAL_NAME:                   return "TERMINAL_NAME";
		case Ast::Expression::TERMINAL_FUNCTION:               return "TERMINAL_FUNCTION";
		case Ast::Expression::TERMINAL_INTRINSIC:              return "TERMINAL_INTRINSIC";
		case Ast::Expression::TERMINAL_LITERAL:                return "TERMINAL_LITERAL";
		case Ast::Expression::TERMINAL_VARIABLE:               return "TERMINAL_VARIABLE";
		case Ast::Expression::TERMINAL_STRUCT:                 return "TERMINAL_STRUCT";
		case Ast::Expression::TERMINAL_ENUM:                   return "TERMINAL_ENUM";
		case Ast::Expression::TERMINAL_PRIMITIVE:              return "TERMINAL_PRIMITIVE";
		case Ast::Expression::TERMINAL_STRUCT_MEMBER:          return "TERMINAL_STRUCT_MEMBER";
		case Ast::Expression::TERMINAL_ENUM_MEMBER:            return "TERMINAL_ENUM_MEMBER";
		case Ast::Expression::TERMINAL_ARRAY_LENGTH:           return "TERMINAL_ARRAY_LENGTH";
		case Ast::Expression::TERMINAL_ARRAY_BEGIN:            return "TERMINAL_ARRAY_BEGIN";
		case Ast::Expression::TERMINAL_ARRAY_END:              return "TERMINAL_ARRAY_END";
		case Ast::Expression::UNARY_BITWISE_NOT:               return "UNARY_BITWISE_NOT";
		case Ast::Expression::UNARY_NOT:                       return "UNARY_NOT";
		case Ast::Expression::UNARY_MINUS:                     return "UNARY_MINUS";
		case Ast::Expression::UNARY_PLUS:                      return "UNARY_PLUS";
		case Ast::Expression::UNARY_REFERENCE_OF:              return "UNARY_REFERENCE_OF";
		case Ast::Expression::UNARY_ADDRESS_OF:                return "UNARY_ADDRESS_OF";
		case Ast::Expression::BINARY_COMPARE_EQUAL:            return "BINARY_COMPARE_EQUAL";
		case Ast::Expression::BINARY_COMPARE_NOT_EQUAL:        return "BINARY_COMPARE_NOT_EQUAL";
		case Ast::Expression::BINARY_COMPARE_LESS:             return "BINARY_COMPARE_LESS";
		case Ast::Expression::BINARY_COMPARE_LESS_OR_EQUAL:    return "BINARY_COMPARE_LESS_OR_EQUAL";
		case Ast::Expression::BINARY_COMPARE_GREATER:          return "BINARY_COMPARE_GREATER";
		case Ast::Expression::BINARY_COMPARE_GREATER_OR_EQUAL: return "BINARY_COMPARE_GREATER_OR_EQUAL";
		case Ast::Expression::BINARY_DOT:                      return "BINARY_DOT";
		case Ast::Expression::BINARY_ADD:                      return "BINARY_ADD";
		case Ast::Expression::BINARY_SUBTRACT:                 return "BINARY_SUBTRACT";
		case Ast::Expression::BINARY_MULTIPLY:                 return "BINARY_MULTIPLY";
		case Ast::Expression::BINARY_DIVIDE:                   return "BINARY_DIVIDE";
		case Ast::Expression::BINARY_MODULO:                   return "BINARY_MODULO";
		case Ast::Expression::BINARY_BITWISE_OR:               return "BINARY_BITWISE_OR";
		case Ast::Expression::BINARY_BITWISE_XOR:              return "BINARY_BITWISE_XOR";
		case Ast::Expression::BINARY_BITWISE_AND:              return "BINARY_BITWISE_AND";
		case Ast::Expression::BINARY_LEFT_SHIFT:               return "BINARY_LEFT_SHIFT";
		case Ast::Expression::BINARY_RIGHT_SHIFT:              return "BINARY_RIGHT_SHIFT";
		case Ast::Expression::BINARY_AND:                      return "BINARY_AND";
		case Ast::Expression::BINARY_OR:                       return "BINARY_OR";
		case Ast::Expression::IF_ELSE:                         return "IF_ELSE";
		case Ast::Expression::IMPLICIT_CAST:                   return "IMPLICIT_CAST";
		case Ast::Expression::CALL:                            return "CALL";
		case Ast::Expression::DOT_CALL:                        return "DOT_CALL";
		case Ast::Expression::SUBSCRIPT:                       return "SUBSCRIPT";
		case Ast::Expression::LAMBDA:                          return "LAMBDA";
		case Ast::Expression::TUPLE:                           return "TUPLE";
		case Ast::Expression::ARRAY:                           return "ARRAY";
		case Ast::Expression::FIXED_ARRAY:                     return "FIXED_ARRAY";
		case Ast::Expression::AS:                              return "AS";
	}
}

static void PrintIndent(u32 indent) {
	for (u32 i = 0; i < indent; i++) Print("    ");
}

static void PrintExpression(Ast::Expression* expr, u32 indent);
static void PrintStatements(Array<Ast::Statement> statements, u32 indent);

static void PrintExpression(Ast::Expression* expr, u32 indent) {
	if (!expr) {
		PrintIndent(indent);
		Print("(null)\n");
		return;
	}

	PrintIndent(indent);
	Print("%: value=v%, type=%\n", CString(GetExpressionKindName(expr->kind)), expr->value.handle, expr->type);

	switch (expr->kind) {
		case Ast::Expression::TERMINAL_LITERAL: {
			Ast::Expression_Literal* lit = (Ast::Expression_Literal*)expr;
			PrintIndent(indent + 1);
			Print("literal: %\n", lit->token);
		} break;

		case Ast::Expression::TERMINAL_VARIABLE: {
			Ast::Expression_Variable* var = (Ast::Expression_Variable*)expr;
			PrintIndent(indent + 1);
			Print("variable: %\n", var->variable->name);
		} break;

		case Ast::Expression::TERMINAL_FUNCTION: {
			Ast::Expression_Function* func = (Ast::Expression_Function*)expr;
			PrintIndent(indent + 1);
			Print("function: %\n", func->function->name);
		} break;

		case Ast::Expression::UNARY_BITWISE_NOT:
		case Ast::Expression::UNARY_NOT:
		case Ast::Expression::UNARY_MINUS:
		case Ast::Expression::UNARY_PLUS:
		case Ast::Expression::UNARY_REFERENCE_OF:
		case Ast::Expression::UNARY_ADDRESS_OF:
		case Ast::Expression::IMPLICIT_CAST: {
			Ast::Expression_Unary* unary = (Ast::Expression_Unary*)expr;
			PrintIndent(indent + 1);
			Print("subexpression:\n");
			PrintExpression(unary->subexpr, indent + 2);
		} break;

		case Ast::Expression::BINARY_COMPARE_EQUAL:
		case Ast::Expression::BINARY_COMPARE_NOT_EQUAL:
		case Ast::Expression::BINARY_COMPARE_LESS:
		case Ast::Expression::BINARY_COMPARE_LESS_OR_EQUAL:
		case Ast::Expression::BINARY_COMPARE_GREATER:
		case Ast::Expression::BINARY_COMPARE_GREATER_OR_EQUAL:
		case Ast::Expression::BINARY_DOT:
		case Ast::Expression::BINARY_ADD:
		case Ast::Expression::BINARY_SUBTRACT:
		case Ast::Expression::BINARY_MULTIPLY:
		case Ast::Expression::BINARY_DIVIDE:
		case Ast::Expression::BINARY_MODULO:
		case Ast::Expression::BINARY_BITWISE_OR:
		case Ast::Expression::BINARY_BITWISE_XOR:
		case Ast::Expression::BINARY_BITWISE_AND:
		case Ast::Expression::BINARY_LEFT_SHIFT:
		case Ast::Expression::BINARY_RIGHT_SHIFT:
		case Ast::Expression::BINARY_AND:
		case Ast::Expression::BINARY_OR: {
			Ast::Expression_Binary* binary = (Ast::Expression_Binary*)expr;
			PrintIndent(indent + 1);
			Print("left:\n");
			PrintExpression(binary->left, indent + 2);
			PrintIndent(indent + 1);
			Print("right:\n");
			PrintExpression(binary->right, indent + 2);
		} break;

		case Ast::Expression::IF_ELSE: {
			Ast::Expression_Ternary* ternary = (Ast::Expression_Ternary*)expr;
			PrintIndent(indent + 1);
			Print("condition:\n");
			PrintExpression(ternary->left, indent + 2);
			PrintIndent(indent + 1);
			Print("if_true:\n");
			PrintExpression(ternary->middle, indent + 2);
			PrintIndent(indent + 1);
			Print("if_false:\n");
			PrintExpression(ternary->right, indent + 2);
		} break;

		case Ast::Expression::CALL: {
			Ast::Expression_Call* call = (Ast::Expression_Call*)expr;
			PrintIndent(indent + 1);
			Print("function:\n");
			PrintExpression(call->function, indent + 2);
			PrintIndent(indent + 1);
			Print("parameters:\n");
			PrintExpression((Ast::Expression*)call->params, indent + 2);
		} break;

		case Ast::Expression::TUPLE: {
			Ast::Expression_Tuple* tuple = (Ast::Expression_Tuple*)expr;
			PrintIndent(indent + 1);
			Print("elements: % items\n", tuple->elements.length);
			for (u32 i = 0; i < tuple->elements.length; i++) {
				PrintExpression(tuple->elements[i], indent + 2);
			}
		} break;

		case Ast::Expression::SUBSCRIPT: {
			Ast::Expression_Subscript* subscript = (Ast::Expression_Subscript*)expr;
			PrintIndent(indent + 1);
			Print("array:\n");
			PrintExpression(subscript->array, indent + 2);
			PrintIndent(indent + 1);
			Print("index:\n");
			PrintExpression(subscript->index, indent + 2);
		} break;

		case Ast::Expression::AS: {
			Ast::Expression_As* as = (Ast::Expression_As*)expr;
			PrintIndent(indent + 1);
			Print("expression:\n");
			PrintExpression(as->expr, indent + 2);
		} break;

		case Ast::Expression::TERMINAL_INTRINSIC: {
			Ast::Expression_Intrinsic* intr = (Ast::Expression_Intrinsic*)expr;
			PrintIndent(indent + 1);
			Print("intrinsic: % (id=%)\n", intr->token, intr->intrinsic);
		} break;

		case Ast::Expression::TERMINAL_STRUCT: {
			Ast::Expression_Struct* struc = (Ast::Expression_Struct*)expr;
			PrintIndent(indent + 1);
			Print("struct: %\n", struc->structure->name);
		} break;

		case Ast::Expression::TERMINAL_ENUM: {
			Ast::Expression_Enum* enu = (Ast::Expression_Enum*)expr;
			PrintIndent(indent + 1);
			Print("enum: %\n", enu->enumeration->name);
		} break;

		case Ast::Expression::TERMINAL_STRUCT_MEMBER: {
			Ast::Expression_Struct_Member* mem = (Ast::Expression_Struct_Member*)expr;
			PrintIndent(indent + 1);
			Print("member: %\n", mem->member->name);
		} break;

		case Ast::Expression::TERMINAL_ENUM_MEMBER: {
			Ast::Expression_Enum_Member* mem = (Ast::Expression_Enum_Member*)expr;
			PrintIndent(indent + 1);
			Print("member: % (value=%)\n", mem->member->name, mem->member->value);
		} break;

		case Ast::Expression::TERMINAL_PRIMITIVE:
		case Ast::Expression::TERMINAL_ARRAY_LENGTH:
		case Ast::Expression::TERMINAL_ARRAY_BEGIN:
		case Ast::Expression::TERMINAL_ARRAY_END: {
			Ast::Expression_Literal* lit = (Ast::Expression_Literal*)expr;
			PrintIndent(indent + 1);
			Print("token: %\n", lit->token);
		} break;

		case Ast::Expression::TERMINAL_NAME: {
			Ast::Expression_Terminal* term = (Ast::Expression_Terminal*)expr;
			PrintIndent(indent + 1);
			Print("name: %\n", term->token);
		} break;

		case Ast::Expression::DOT_CALL: {
			Ast::Expression_Dot_Call* call = (Ast::Expression_Dot_Call*)expr;
			PrintIndent(indent + 1);
			Print("dot:\n");
			PrintExpression((Ast::Expression*)call->dot, indent + 2);
			PrintIndent(indent + 1);
			Print("parameters:\n");
			PrintExpression((Ast::Expression*)call->params, indent + 2);
		} break;

		case Ast::Expression::ARRAY: {
			Ast::Expression_Array* arr = (Ast::Expression_Array*)expr;
			if (arr->left) {
				PrintIndent(indent + 1);
				Print("left:\n");
				PrintExpression(arr->left, indent + 2);
			}
			if (arr->right) {
				PrintIndent(indent + 1);
				Print("right:\n");
				PrintExpression(arr->right, indent + 2);
			}
		} break;

		case Ast::Expression::FIXED_ARRAY: {
			Ast::Expression_Fixed_Array* arr = (Ast::Expression_Fixed_Array*)expr;
			PrintIndent(indent + 1);
			Print("elements: % items\n", arr->elements.length);
			for (u32 i = 0; i < arr->elements.length; i++) {
				PrintExpression(arr->elements[i], indent + 2);
			}
		} break;

		case Ast::Expression::LAMBDA: {
			PrintIndent(indent + 1);
			Print("(not implemented)\n");
		} break;
	}
}

static void PrintStatements(Array<Ast::Statement> statements, u32 indent) {
	for (u32 j = 0; j < statements.length; j++) {
		Ast::Statement* stmt = &statements[j];

		switch (stmt->kind) {
			case Ast::STATEMENT_VARIABLE_DECLARATION: {
				PrintIndent(indent);
				Print("VarDecl: %\n", stmt->vardecl.name);
				if (stmt->vardecl.assignment) {
					PrintIndent(indent + 1);
					Print("initializer:\n");
					PrintExpression(stmt->vardecl.assignment, indent + 2);
				}
			} break;

			case Ast::STATEMENT_RETURN: {
				PrintIndent(indent);
				Print("Return:\n");
				if (stmt->ret.expr) {
					PrintExpression(stmt->ret.expr, indent + 1);
				}
			} break;

			case Ast::STATEMENT_ASSIGNMENT:
			case Ast::STATEMENT_ASSIGNMENT_ADD:
			case Ast::STATEMENT_ASSIGNMENT_SUBTRACT:
			case Ast::STATEMENT_ASSIGNMENT_MULTIPLY:
			case Ast::STATEMENT_ASSIGNMENT_DIVIDE:
			case Ast::STATEMENT_ASSIGNMENT_XOR: {
				PrintIndent(indent);
				Print("Assignment:\n");
				PrintIndent(indent + 1);
				Print("left:\n");
				PrintExpression(stmt->assignment.left, indent + 2);
				PrintIndent(indent + 1);
				Print("right:\n");
				PrintExpression(stmt->assignment.right, indent + 2);
			} break;

			case Ast::STATEMENT_BRANCH_BLOCK: {
				PrintIndent(indent);
				Print("BranchBlock:\n");
				for (u32 b = 0; b < stmt->branch_block.branches.length; b++) {
					Ast::Branch* branch = &stmt->branch_block.branches[b];

					switch (branch->kind) {
						case Ast::BRANCH_IF: {
							PrintIndent(indent + 1);
							Print("If:\n");
							PrintIndent(indent + 2);
							Print("condition:\n");
							PrintExpression(branch->if_condition, indent + 3);
							PrintIndent(indent + 2);
							Print("body:\n");
							PrintStatements(branch->code.statements, indent + 3);
						} break;

						case Ast::BRANCH_WHILE: {
							PrintIndent(indent + 1);
							Print("While:\n");
							PrintIndent(indent + 2);
							Print("condition:\n");
							PrintExpression(branch->while_condition, indent + 3);
							PrintIndent(indent + 2);
							Print("body:\n");
							PrintStatements(branch->code.statements, indent + 3);
						} break;

						case Ast::BRANCH_FOR_RANGE: {
							PrintIndent(indent + 1);
							Print("For (range):\n");
							PrintIndent(indent + 2);
							Print("iterator: %\n", branch->for_range.iterator->name);
							PrintIndent(indent + 2);
							Print("range:\n");
							PrintExpression(branch->for_range.range, indent + 3);
							if (branch->for_range.filter) {
								PrintIndent(indent + 2);
								Print("filter:\n");
								PrintExpression(branch->for_range.filter, indent + 3);
							}
							PrintIndent(indent + 2);
							Print("body:\n");
							PrintStatements(branch->code.statements, indent + 3);
						} break;

						case Ast::BRANCH_FOR_VERBOSE: {
							PrintIndent(indent + 1);
							Print("For:\n");
							PrintIndent(indent + 2);
							Print("variable: %\n", branch->for_verbose.variable->name);
							PrintIndent(indent + 2);
							Print("condition:\n");
							PrintExpression(branch->for_verbose.condition, indent + 3);
							PrintIndent(indent + 2);
							Print("next:\n");
							PrintExpression(branch->for_verbose.next, indent + 3);
							PrintIndent(indent + 2);
							Print("body:\n");
							PrintStatements(branch->code.statements, indent + 3);
						} break;

						case Ast::BRANCH_NAKED:
							break;
					}

					if (branch->else_branch) {
						PrintIndent(indent + 1);
						Print("Else:\n");
						PrintStatements(branch->else_branch->code.statements, indent + 2);
					}

					if (branch->then_branch) {
						PrintIndent(indent + 1);
						Print("Then:\n");
						PrintStatements(branch->then_branch->code.statements, indent + 2);
					}
				}
			} break;

			case Ast::STATEMENT_EXPRESSION: {
				PrintIndent(indent);
				Print("Expression:\n");
				PrintExpression(stmt->expression, indent + 1);
			} break;

			case Ast::STATEMENT_INCREMENT: {
				PrintIndent(indent);
				Print("Increment:\n");
				PrintExpression(stmt->increment.expression, indent + 1);
			} break;

			case Ast::STATEMENT_DECREMENT: {
				PrintIndent(indent);
				Print("Decrement:\n");
				PrintExpression(stmt->increment.expression, indent + 1);
			} break;

			case Ast::STATEMENT_BREAK: {
				PrintIndent(indent);
				Print("Break\n");
			} break;

			case Ast::STATEMENT_CLAIM: {
				PrintIndent(indent);
				Print("Claim:\n");
				PrintExpression(stmt->claim.expr, indent + 1);
			} break;

			case Ast::STATEMENT_DEFER: {
				PrintIndent(indent);
				Print("Defer:\n");
				PrintStatements(stmt->defer.code.statements, indent + 1);
			} break;
		}
	}
}

void Ast::PrintAST(Ast::Module* module) {
	Print("\n=== AST for % ===\n\n", module->name);

	for (u32 i = 0; i < module->scope.functions.length; i++) {
		Ast::Function* func = &module->scope.functions[i];
		Print("Function: % (% parameters) -> %\n", func->name, func->params.length, func->return_type);

		for (u32 p = 0; p < func->params.length; p++) {
			Ast::Variable* param = &func->params[p];
			Print("  Parameter: % : %\n", param->name, param->type);
		}

		if (func->code.statements.length > 0) {
			Print("  Statements: %\n", func->code.statements.length);
			PrintStatements(func->code.statements, 1);
		}
		Print("\n");
	}
}

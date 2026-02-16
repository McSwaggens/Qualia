#pragma once

#include "parser.h"

struct Scanner {
	Ast::Module* module;

	Scanner(Ast::Module* module) : module(module) { }

	template<typename... Args>
	[[noreturn]] void Error(SourceLocation location, String format, Args&&... args) {
		::Error(module, location, format, args...);
	}

	template<typename... Args>
	[[noreturn]] void Error(Ast::Expression* expression, String format, Args&&... args) {
		::Error(module, expression, format, args...);
	}

	template<typename... Args>
	[[noreturn]] void Error(Token* token, String format, Args&&... args) {
		::Error(module, token->location, format, args...);
	}

	void SemanticParse();
	void ScanScope(Ast::Scope* scope);
	void ScanFunction(Ast::Function* function, Ast::Scope* scope);
	void ScanCode(Ast::Code* code, Ast::Scope* scope, Ast::Function* function);
	void ScanExpression(Ast::Expression* expression, Ast::Scope* scope);

	// Type resolution
	TypeID GetBaseType(Ast::BaseType basetype, Ast::Scope* scope);
	TypeID GetType(Ast::Type* ast_type, Ast::Scope* scope);
	TypeID GetTypeFromParams(Array<Ast::Variable> params);

	// Expression scanning
	void ScanExpressionTerminalName(Ast::Expression_Terminal* terminal, Ast::Scope* scope);
	void ScanExpressionFixedArray(Ast::Expression_Fixed_Array* fixed_array, Ast::Scope* scope);
	void ScanExpressionArray(Ast::Expression_Array* array, Ast::Scope* scope);
	void ScanExpressionLiteral(Ast::Expression_Literal* literal, Ast::Scope* scope);
	void ScanExpressionTuple(Ast::Expression_Tuple* tuple, Ast::Scope* scope);
	void ScanExpressionCall(Ast::Expression_Call* call, Ast::Scope* scope);
	void ScanExpressionSubscript(Ast::Expression_Subscript* subscript, Ast::Scope* scope);
	void ScanExpressionUnaryAddressOf(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionUnaryReferenceOf(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionUnaryBitwiseNot(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionUnaryMinus(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionUnaryPlus(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionUnaryNot(Ast::Expression_Unary* unary, Ast::Scope* scope);
	void ScanExpressionBinaryDot(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryCompareEquality(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryCompareOrdered(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryArithmetic(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryBitwise(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryShift(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionBinaryLogical(Ast::Expression_Binary* binary, Ast::Scope* scope);
	void ScanExpressionIfElse(Ast::Expression_Ternary* ternary, Ast::Scope* scope);
	void ScanExpressionLambda(Ast::Expression* expression, Ast::Scope* scope);
	void ScanExpressionAs(Ast::Expression_As* as, Ast::Scope* scope);

	// Branch scanning
	void ScanIf(Ast::Function* function, Ast::Code* code, Ast::Branch* branch);
	void ScanWhile(Ast::Function* function, Ast::Code* code, Ast::Branch* branch);
	void ScanRangeFor(Ast::Function* function, Ast::Code* code, Ast::Branch* branch);
	void ScanVerboseFor(Ast::Function* function, Ast::Code* code, Ast::Branch* branch);
	void ScanBranchBlock(Ast::Function* function, Ast::Code* code, Ast::BranchBlock* branch_block);

	// Statement scanning
	void ScanDefer(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanClaim(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanIncDec(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanReturn(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanBreak(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanVarDecl(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanAssignment(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanBinaryAssignment(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
	void ScanStatement(Ast::Statement* statement, Ast::Code* code, Ast::Function* function);
};

static u64 CalculateStackFrameSize(Ast::Function* function);
static u64 CalculateStackFrameSize(Ast::Code* code, u64 offset);

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
	void ScanFunction(Ast::Function* function);
	void ScanCode(Ast::Code* code);
	void ScanStatement(Ast::Statement* statement);
	void ScanExpression(Ast::Expression* expression);
};

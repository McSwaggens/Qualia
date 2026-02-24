#pragma once

#include "string.h"
#include "list.h"
#include "token.h"
#include "type_system.h"
#include "ir.h"
#include "file_system.h"
#include "ast.h"

enum IntrinsicID : u32 {
	INTRINSIC_SYSTEM_CALL = 0,

	INTRINSIC_COUNT,
	INTRINSIC_INVALID = INTRINSIC_COUNT
};

typedef void (*IntrinsicFunc)(void*,void*);

struct Intrinsic {
	String name;
	TypeID type;
	IntrinsicFunc func;
};

struct Line {
	Indent16 indent;
	String   string;
	Token*   tokens_begin;
	Token*   tokens_end;
};

struct Parser {
	Ast::Module* module;
	Token* token;
	Stack stack;

	Parser(Ast::Module* module) :
		module(module),
		token(&module->tokens[0]),
		stack(Stack::Create(1 << 21))
	{ }

	template<typename... Args>
	[[noreturn]] void Error(String format, Args&&... args);

	void ParseGlobalScope();
	Ast::Struct ParseStruct(u32 indent);
	Ast::Enum ParseEnum(u32 indent);
	Ast::Expression* ParseExpression(u32 indent, bool assignment_break = false, s32 parent_precedence = 0);
	Ast::Type ParseType(u32 indent);
	void ParseParameters(Ast::Function* function, Token* open_paren, u32 indent);
	Ast::BranchBlock ParseBranchBlock(u32 indent);
	Ast::Statement ParseExpressionStatement(u32 indent);
	Ast::Statement ParseVariableDeclaration(u32 indent);
	Ast::Statement ParseBranchBlockStatement(u32 indent);
	Ast::Statement ParseIncDecStatement(u32 indent);
	Ast::Statement ParseDeferStatement(u32 indent);
	Ast::Statement ParseBreakStatement(u32 indent);
	Ast::Statement ParseReturnStatement(u32 indent);
	Ast::Statement ParseClaimStatement(u32 indent);
	Ast::Statement ParseStatement(u32 indent);
	Ast::Code ParseCode(u32 indent);
	Ast::Function ParseFunction(u32 indent);
	Ast::Import ParseImport(u32 indent);
};

static void GenericWrite(OutputBuffer* buffer, Ast::Expression* expression);
static void GenericWrite(OutputBuffer* buffer, Ast::Type* type);
static void GenericWrite(OutputBuffer* buffer, Ast::Type type);

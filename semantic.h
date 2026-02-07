#pragma once

#include "parser.h"

static void SemanticParse(Ast::Module* module);
static u64 CalculateStackFrameSize(Ast::Function* function);
static u64 CalculateStackFrameSize(Ast::Code* code, u64 offset);
static void ScanExpression(Ast::Expression* expression, Ast::Scope* scope, Ast::Module* module);
static void ScanScope(Ast::Scope* scope, Ast::Module* module);
static void ScanCode(Ast::Code* code, Ast::Scope* scope, Ast::Function* function, Ast::Module* module);
static void ScanFunction(Ast::Function* function, Ast::Scope* scope, Ast::Module* module);

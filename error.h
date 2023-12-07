#pragma once

#include "token.h"
#include "string.h"

template<typename ...Args>
[[noreturn]]
static void LexerError(struct Ast_Module* module, struct SourceLocation where, struct String format, Args&&... message_args);

template<typename ...Args>
[[noreturn]]
static void Error(struct Ast_Module* module, struct SourceLocation where, struct String format, Args&&... message_args);

template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, struct Ast_Expression* expr, String format, Args&&... message_args);

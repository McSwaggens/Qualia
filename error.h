#pragma once

#include "token.h"
#include "string.h"

namespace Ast { struct Module; struct Expression; }

template<typename... Args>
[[noreturn]]
static void LexerError(Ast::Module* module, SourceLocation where, String format, Args&&... message_args);

template<typename... Args>
[[noreturn]]
static void Error(Ast::Module* module, SourceLocation where, String format, Args&&... message_args);

template<typename... Args>
[[noreturn]]
static void Error(Ast::Module* module, Ast::Expression* expr, String format, Args&&... message_args);

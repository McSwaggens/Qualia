#include "error.h"
#include "parser.h"
#include "general.h"
#include "string.h"

// @Todo: All of this needs to be cleaned up.
// @Todo: Implement better error logging for the user.
//        .. don't just print an error and abort.
//        .. Log and try to continue..

template<typename ...Args>
[[noreturn]]
static void LexerError(Ast_Module* module, SourceLocation where, String format, Args&&... message_args) {
	s64 margin = 2;
	s64 begin = where.line;
	s64 number_of_lines = 1 + margin;

	Print(&unix_error_buffer, "%:%:%: error: ", module->file_path, (where.line+1), (where.offset+1));
	Print(&unix_error_buffer, format, message_args...);

	// Only way to fix this is to continue lexing, which is what I'm going to do soon anyways.

	// for (u64 line = begin; line < begin + number_of_lines && line < module->lines.count; line++)
	// {
	// 	Print(&unix_error_buffer, "%\n", String(module->lines[line], module->lines[line].length));
	// }

	ExitProcess(false);
}

// @RemoveMe? Span<Token> should be sufficient.
template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, SourceLocation where, String format, Args&&... message_args) {
	s64 margin = 2;
	s64 begin = where.line;
	s64 number_of_lines = 1 + margin;

	Print(&unix_error_buffer, "%:%:%: error: ", module->file_path, (where.line+1), (where.offset+1));
	Print(&unix_error_buffer, format, message_args...);

	for (s64 line = begin; line < begin + number_of_lines && line < module->lines.count; line++) {
		Print(&unix_error_buffer, "%\n", module->lines[line].string);
	}

	ExitProcess(false);
}

template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, Ast_Expression* expr, String format, Args&&... message_args) {
	Token* begin = expr->begin;
	Token* end = expr->end;

	s64 margin = 2;
	s64 line_begin = begin->location.line;
	SourceLocation pos_begin = begin->location;
	SourceLocation pos_end = end[-1].location; // @Bug: What if begin = end. Is this invalid input? IDK
	s64 number_of_lines = pos_end.line - pos_begin.line + margin + 1;

	Print(&unix_error_buffer, "%:%:%: error: ", module->file_path, (pos_begin.line+1), (pos_begin.offset+1));
	Print(&unix_error_buffer, format, message_args...);

	// @Todo: Coloring/Highlighting

	for (s64 line = line_begin; line < line_begin + number_of_lines && line < module->lines.count; line++) {
		Print(&unix_error_buffer, "%\n", module->lines[line].string);
	}

	ExitProcess(false);
}


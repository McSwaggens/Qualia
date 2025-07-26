#include "error.h"
#include "general.h"
#include "parser.h"
#include "string.h"
#include "print.h"

// @Todo: All of this needs to be cleaned up.
// @Todo: Implement better error logging for the user.
//        .. don't just print an error and abort.
//        .. Log and try to continue..

[[noreturn]]
static void FlushAndTerminate() {
	output_buffer.Flush();
	error_buffer.Flush();
	OS::Terminate(false);
}

template<typename ...Args>
[[noreturn]]
static void LexerError(Ast_Module* module, SourceLocation where, String format, Args&&... message_args) {
	s64 margin = 2;
	s64 begin = where.line;
	s64 number_of_lines = 1 + margin;

	Print(&error_buffer, "%:%:%: error: ", module->file_path, (where.line+1), (where.offset+1));
	Print(&error_buffer, format, message_args...);

	FlushAndTerminate();
}

// @RemoveMe? Span<Token> should be sufficient.
template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, SourceLocation where, String format, Args&&... message_args) {
	s64 margin = 2;
	s64 begin = where.line;
	s64 number_of_lines = 1 + margin;

	Print(&error_buffer, "%:%:%: error: ", module->file_path, (where.line+1), (where.offset+1));
	Print(&error_buffer, format, message_args...);

	for (s64 line = begin; line < begin + number_of_lines && line < module->lines.length; line++)
		Print(&error_buffer, "%\n", module->lines[line].string);

	FlushAndTerminate();
}

template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, Ast_Expression* expr, String format, Args&&... message_args) {
	Token* begin = expr->begin;
	Token* end   = expr->end;

	s64 margin = 2;
	s64 line_begin = begin->location.line;
	SourceLocation pos_begin = begin->location;
	SourceLocation pos_end = end[-1].location; // @Bug: What if begin = end. Is this invalid input? IDK
	s64 number_of_lines = pos_end.line - pos_begin.line + margin + 1;

	Print(&error_buffer, "%:%:%: error: ", module->file_path, (pos_begin.line+1), (pos_begin.offset+1));
	Print(&error_buffer, format, message_args...);

	// @Todo: Coloring/Highlighting

	for (s64 line = line_begin; line < line_begin + number_of_lines && line < module->lines.length; line++)
		Print(&error_buffer, "%\n", module->lines[line].string);

	FlushAndTerminate();
}


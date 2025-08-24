#ifndef LEXER_H
#define LEXER_H

#include "general.h"
#include "list.h"
#include "parser.h"
#include "string.h"
#include "print.h"
#include "token.h"

struct Lexer {
	Ast_Module* module;
	String file_path;
	Array<Token> tokens;
	Token* current_token;
	List<Line> lines;
	Array<char> code;
	SourceLocation location;
	char* cursor;
	char* end;
	char* line_begin;
	u32 indent;
	s64 line_number;

	void Parse();
	void ParseLiteral();
	bool TestKeyword(TokenKind keyword);
	void PushToken(Token token);
	void PushLine();
};

static Lexer CreateLexer(Ast_Module* module, String file_path) {
	Array<char>  code   = File::Load(file_path, 16, 64);
	Array<Token> tokens = AllocArray<Token>(code.length + 1);
	List<Line>   lines  = AllocateList<Line>(code.length/4);

	return (Lexer){
		.module = module,
		.file_path = file_path,
		.tokens = tokens,
		.current_token = tokens,
		.lines = lines,
		.code = code,
		.cursor = code,
		.end = code.End(),
		.line_begin = code,
		.indent = 0,
		.line_number = 0,
	};
}

#endif // LEXER_H

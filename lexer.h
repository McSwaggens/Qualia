#pragma once

#include "general.h"
#include "list.h"
#include "parser.h"
#include "string.h"
#include "print.h"
#include "token.h"
#include "error.h"

struct Lexer {
	Ast::Module* module;
	String file_path;
	Array<Token> tokens;
	Token* current_token;
	List<Line> lines;
	String code;
	SourceLocation location;
	char* cursor;
	char* end;
	char* line_begin;
	u32 indent;
	s64 line_number;

	Lexer(Ast::Module* module, String file_path) {
		Array<byte> buffer = File::Load(file_path, 16, 64);
		String code = String((char*)buffer.data, buffer.length, buffer.length);
		List<Token> tokens = AllocateList<Token>(code.length + 1);
		List<Line> lines = AllocateList<Line>(code.length / 4);

		this->module = module;
		this->file_path = file_path;
		this->tokens = tokens.ToArray();
		this->current_token = tokens.ToArray();
		this->lines = lines;
		this->code = code;
		this->location = {};
		this->cursor = code;
		this->end = code.End();
		this->line_begin = code;
		this->indent = 0;
		this->line_number = 0;
	}

	Lexer(Ast::Module* module, String code_string, String file_path) {
		String code = code_string.CopyPadded(16, 64);
		List<Token> tokens = AllocateList<Token>(code.length + 1);
		List<Line> lines = AllocateList<Line>(code.length / 4);

		this->module = module;
		this->file_path = file_path;
		this->tokens = tokens.ToArray();
		this->current_token = tokens.ToArray();
		this->lines = lines;
		this->code = code;
		this->location = {};
		this->cursor = code;
		this->end = code.End();
		this->line_begin = code;
		this->indent = 0;
		this->line_number = 0;
	}

	void Parse();
	void ParseLiteral();
	bool TestKeyword(TokenKind keyword);
	void PushToken(Token token);
	void PushLine();

	template<typename... Args>
	[[noreturn]]
	void Error(String format, Args&&... args) {
		LexerError(module, location, format, args...);
	}
};


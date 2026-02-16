#include "file_system.h"
#include "int.h"
#include "ir.h"
#include "list.h"
#include "sort.h"
#include "fixed_buffer.h"

namespace Ast { struct Module; }

static List<Ast::Module*> modules;

#include "general.cc"
#include "thread.cc"
#include "stacktrace.cc"
#include "assert.cc"
#include "file_system.cc"
#include "stack.cc"
#include "error.cc"
#include "type_system.cc"
#include "lexer.cc"
#include "array_buffer.cc"
#include "print.cc"
#include "semantic.cc"
#include "parser.cc"
#include "ir.cc"
#include "binary.cc"
#include "alloc.cc"

#include "ast.cc"

static void ParseModule(Ast::Module* module) {
	Parser parser = Parser(module);
	parser.ParseGlobalScope();
	SemanticParse(module);
}

static Ast::Module* CompileString(String code_string) {
	Ast::Module* module = new Ast::Module("<string>", "<string>");
	modules.Add(module);

	Lexer lexer = Lexer(module, code_string, "<string>");
	lexer.Parse();

	ParseModule(module);

	return module;
}

static Ast::Module* CompileFile(String file_path) {
	if (!File::DoesExist(file_path)) {
		Print("error: File does not exist: %\n", file_path);
		return null;
	}

	Ast::Module* module = new Ast::Module(file_path, GetFileName(file_path));
	modules.Add(module);

	Lexer lexer = Lexer(module, file_path);
	lexer.Parse();

	ParseModule(module);
	Ast::PrintAST(module);

	return module;
}

int main(int argc, char** args) {
	InitCrashHandler();
	InitGlobalAllocator();
	Thread::Init();
	InitEvalSystem();
	InitTypeSystem();
	InitIntrinsics();
	IR::Init();

	if (argc < 2) {
		Print("Usage: qualia <file.q> [file2.q ...]\n");
		output_buffer.Flush();
		OS::Terminate(false);
	}

	for (int i = 1; i < argc; i++) {
		String file = CString(args[i]);
		Print("Compiling: %\n", file);
		CompileFile(file);
	}

	IR::PrintState();

	Print("Compiler finished.\n");

	output_buffer.Flush();
	error_buffer.Flush();

	OS::Terminate(true);
}

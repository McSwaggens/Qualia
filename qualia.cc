#include "file_system.h"
#include "int.h"
#include "ir.h"
#include "list.h"
#include "sort.h"

namespace Ast { struct Module; }

static List<Ast::Module*> modules = null;

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

static void CompileFile(String file_path) {
	if (!File::DoesExist(file_path)) {
		Print("error: File does not exist: %\n", file_path);
		return;
	}

	Ast::Module* module = Alloc<Ast::Module>();
	modules.Add(module);

	*module = {
		.file_path = file_path,
		.name = file_path, // @FixMe get the real name
	};

	Lexer lexer = CreateLexer(module, file_path);
	lexer.Parse();

	Parser parser = {
		.module = module,
		.token = &module->tokens[0],
		.stack = Stack::Create(1 << 21),
	};

	parser.ParseGlobalScope();
	SemanticParse(module);
	Ast::PrintAST(module);
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
		String file = ToString(args[i]);
		Print("Compiling: %\n", file);
		CompileFile(file);
	}

	IR::PrintState();

	Print("Compiler finished.\n");

	output_buffer.Flush();
	error_buffer.Flush();

	OS::Terminate(true);
}

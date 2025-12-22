#include "file_system.h"
#include "int.h"
#include "list.h"
#include "sort.h"

struct Ast_Module;

static List<Ast_Module*> modules = null;

#include "general.cc"
#include "thread.cc"
#include "assert.cc"
#include "file_system.cc"
#include "stack.cc"
#include "error.cc"
#include "type_system.cc"
#include "lexer.cc"
#include "memory.cc"
#include "array_buffer.cc"
#include "print.cc"
#include "semantic.cc"
#include "parser.cc"
#include "interpreter.cc"
#include "ir.cc"

static void CompileFile(String file_path) {
	if (!File::DoesExist(file_path)) {
		Print("error: File does not exist: %\n", file_path);
		return;
	}

	Stack stack = CreateStack(1 << 21);
	Ast_Module* module = stack.Allocate<Ast_Module>();
	ZeroMemory(module);

	modules.Add(module);

	*module = {
		.stack = stack,
		.file_path = file_path,
		.name = file_path, // @FixMe get the real name
	};

	Lexer lexer = CreateLexer(module, file_path);
	lexer.Parse();

	for (Token token : lexer.tokens)
		Print("\t%\n", token);


	ParseGlobalScope(module);
	SemanticParse(module);
}

int main(int argc, char** args) {
	InitGlobalAllocator();
	InitEvalSystem();
	InitTypeSystem();
	InitIntrinsics();
	IR::Init();

	const String files[] = {
		"test.q",
		// "test_literals.q"
	};

	for (u32 i = 0; i < COUNT(files); i++) {
		Print("Compiling: %\n", files[i]);
		CompileFile(files[i]);
	}

	Print("Compiler finished.\n");

	output_buffer.Flush();
	error_buffer.Flush();

	OS::Terminate(true);
}


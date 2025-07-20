#include "int.h"

#include "array.h"
#include "list.h"
#include "memory.h"

struct Ast_Module;

static List<Ast_Module*> modules;

#include "thread.cc"
#include "assert.cc"
#include "file_system.cc"
#include "general.cc"
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
	if (!FileDoesExist(file_path)) {
		Print("File does not exist: %\n", file_path);
		return;
	}

	Stack stack = CreateStack(1 << 21);
	Ast_Module* module = StackAllocate<Ast_Module>(&stack);
	ZeroMemory(module);

	modules.Add(module);

	module->stack = stack;
	module->file_path = file_path;
	module->name = file_path; // @FixMe

	LexerParse(module);
	ParseGlobalScope(module);
	SemanticParse(module);
	GenerateIR(module);
}

int main(int argc, char** args) {
	modules = null;

	InitGlobalArena();
	InitArrayBufferPool();
	InitThread();
	InitTypeSystem();
	InitIntrinsics();

	// @Todo: Process user args.
	// @Todo: Find files in current directory or the directory that the user specified.

	const String files[] = {
		"test.q",
		// "test_literals.q"
	};

	for (u32 i = 0; i < COUNT(files); i++) {
		Print("Compiling: %\n", files[i]);
		CompileFile(files[i]);
	}

	Print("Compiler finished.\n");

	BufferFlush(&unix_output_buffer);
	BufferFlush(&unix_error_buffer);

	ExitProcess(true);
}


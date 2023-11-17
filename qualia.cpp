#include "int.h"

#include "array.h"
#include "list.h"

struct Ast_Module;

struct Qualia
{
	// Settings
	// Target
	List<Ast_Module*> modules;
} qualia;

#include "thread.cpp"
#include "file_system.cpp"
#include "general.cpp"
#include "error.cpp"
#include "type_system.cpp"
#include "lexer.cpp"
#include "memory.cpp"
#include "array_buffer.cpp"
#include "print.cpp"
#include "semantic.cpp"
#include "parser.cpp"
#include "interpreter.cpp"
#include "ir.cpp"

static void CompileFile(String file_path)
{
	if (!FileDoesExist(file_path))
	{
		Print("File does not exist: %\n", file_path);
		return;
	}

	Stack stack = CreateStack(1 << 21);
	Ast_Module* module = StackAllocate<Ast_Module>(&stack);
	ZeroMemory(module);

	qualia.modules.Add(module);

	module->stack = stack;
	module->file_path = file_path;
	module->name = file_path; // @FixMe

	LexerParse(module);
	ParseGlobalScope(module);
	SemanticParse(module);
	GenerateIR(module);
}

struct Linux_Booty
{
	char* path;
	Array<char*> arguments;
};

// extern "C" void Start_Linux(Linux_Booty* booty)

int main(int argc, char** args)
{
	ZeroMemory(&qualia);

	InitPageCache();
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

	for (uint32 i = 0; i < COUNT(files); i++)
	{
		Print("Compiling: %\n", files[i]);
		CompileFile(files[i]);
	}

	Print("Compiler finished.\n");

	BufferFlush(&unix_output_buffer);
	BufferFlush(&unix_error_buffer);

	ExitProcess(true);
}


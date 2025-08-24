#include "file_system.h"
#include "int.h"
#include "list.h"

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

static inline u64 GoldenHash(u64 n) { return n * 11400714819323198485llu; } // Golden/Fibinacci hash: (2^64)/((1+sqrt(5))/2)

#include "sort.h"

static void GenerateRandomNumbers(u64* numbers, u64 n) {
	static u64 state = 2293724;

	for (u64 i = 0; i < n; i++)
	{
		state ^= state << 13;
		state ^= state >>  7;
		state ^= state << 17;

		numbers[i] = state & 0xffff;
	}
}

int main(int argc, char** args) {
	InitGlobalAllocator();
	InitTypeSystem();
	InitIntrinsics();
	IR::Init();

	u64 max_numbers = (1llu<<37) / sizeof(u64);
	u64* numbers = Alloc<u64>(max_numbers);
	for (u64 n = max_numbers; n <= max_numbers; n++) {
		Print("Generating % random numbers...", n);
		GenerateRandomNumbers(numbers, n);
		// Print("\nrandom = %\n", Array<u64>(numbers, n));

		Print(" Sorting...");
		RadixSort(Array<u64>(numbers, numbers + n));

		Print(" Verifying...");
		bool is_sorted = IsSorted(numbers, numbers + n);
		if (!is_sorted) {
			Print(" FAILURE!\n");
			output_buffer.Flush();
			break;
		}

		Print(" SUCCESS!\n");
		output_buffer.Flush();

	}

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


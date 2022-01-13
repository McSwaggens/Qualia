
#include "int.h"

struct Qualia
{
} qualia;

#include "file.cpp"
#include "interpreter.cpp"
#include "ir.cpp"
#include "lexer.cpp"
#include "memory.cpp"
#include "parser.cpp"
#include "pooled_array.cpp"
#include "print.cpp"
#include "semantic.cpp"
#include "util.cpp"
#include "random.cpp"
#include "type.cpp"

#include "util.h"

s32 main(s32 argc, const char** argv)
{
	InitPageCache();
	InitTime();
	InitGlobalArena();
	InitArrayBufferPool();
	InitTypeSystem();
	InitIntrinsicFunctions();

	if (true)
	{
		String file_path = "test.q";
		Ast_Module* module = ParseFile(file_path);
		GenerateIR(module);
	}

	standard_output_buffer.Flush();
	standard_error_buffer.Flush();
	return 0;
}


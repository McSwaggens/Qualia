#include "int.h"
#include "string.h"
#include "print.h"
#include "parser.h"
#include "pooled_array.h"
#include "memory.h"
#include "ir.h"

static void HandleProgramArguements(Array<String> arguments)
{
	for (u32 i = 0; i < arguments.count; i++)
	{
	}
}

s32 main(s32 argc, const char** argv)
{
	auto x = 1 + true;
	InitArrayPool();
	InitGlobalArena();
	InitTypeSystem();

	if (true)
	{
		s32 param_count = argc-1;
		String params[param_count];
		for (s32 i = 0; i < param_count; i++) params[i] = ToString(argv[i+1]);
		HandleProgramArguements(Array<String>(params, param_count));

		// @RemoveMe
		String file_path = "test.q";
		if (param_count)
		{
			file_path = params[0];
		}

		Parse_Info info = ParseFile(file_path);
		Ast_Root* root = info.ast_root;

		for (u32 i = 0; i < root->scope.functions.count; i++)
		{
			Ast_Function* function = &root->scope.functions[i];
			String name = function->name;

			// if (Compare(name, "Test") && !function->parameters.count)
			// {
			// 	Interpreter* interpreter = CreateInterpreter(&info);
			// 	u64 data_size = 0;

			// 	if (function->return_type)
			// 	{
			// 		data_size = function->return_type->size;
			// 	}

			// 	char data[data_size];
			// 	ZeroMemory(data, data_size);
			// 	Interpret(function, null, data, interpreter);
			// }

		}

		ConvertToIR(root);
	}

	standard_output_buffer.Flush();
	standard_error_buffer.Flush();
	return 0;
}


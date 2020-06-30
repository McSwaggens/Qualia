#include "int.h"
#include "string.h"
#include "print.h"
#include "parser.h"
#include "semantic.h"

s32 main(s32 argc, const char** argv)
{
	s32 param_count = argc-1;
	String params[param_count];
	for (s32 i = 0; i < param_count; i++) params[i] = ToString(argv[i+1]);

	for (s32 i = 0; i < param_count; i++)
	{
	}

	String file_path = "test.q";
	if (param_count) {
		file_path = params[0];
	}

	Parse_Info info = ParseFile(file_path);
	SemanticParse(info);

	standard_output_buffer.Flush();
	standard_error_buffer.Flush();
	return 0;
}


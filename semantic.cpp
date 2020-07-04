#include "semantic.h"
#include "memory.h"
#include "print.h"

#define For(n, a) for (auto* n = a.Begin(); n < a.End(); n++)

template<typename ...Args>
[[noreturn]]
static void Error(Semantic* semantic, SourceLocation where, String format, Args&&... message_args)
{
	u32 margin = 2;
	u32 start = where.line;
	u32 number_of_lines = 1 + margin;

	Print("%:%:%: error: ", semantic->info.file_path, (where.line+1), (where.offset+1));
	Print(format, message_args...);

	for (u32 line = start; line < start + number_of_lines && line < semantic->info.lines.count; line++)
	{
		Print("%\n", String(semantic->info.lines[line], semantic->info.lines[line].Length()));
	}

	Fail();
}

void SemanticParse(Parse_Info info)
{
	Ast_Root* root = info.ast_root;
	Semantic semantic;
	ZeroMemory(&semantic);
	semantic.info = info;
	semantic.scope.parent_scope = null;

	Print("Semantic Parse finished successfully.\n");
}


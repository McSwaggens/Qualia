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

static Struct* FindStruct(String name, Scope* scope)
{
	while (scope)
	{
		for (Struct* s = scope->structs; s < scope->structs.End(); s++)
		{
			if (CompareStrings(name, s->name))
			{
				return s;
			}
		}

		scope = scope->parent_scope;
	}

	return null;
}

static Enum* FindEnum(String name, Scope* scope)
{
	while (scope)
	{
		for (Enum* e = scope->enums; e < scope->enums.End(); e++)
		{
			if (CompareStrings(name, e->name))
			{
				return e;
			}
		}

		scope = scope->parent_scope;
	}

	return null;
}

static Type ParseType(Ast_TypeExpression* ast_type, Scope* scope, Semantic* semantic);
static BaseType ParseBaseType(Ast_TypeExpression* ast_type, Scope* scope, Semantic* semantic);

static bool Compare(Type a, Type b);

static bool Compare(Specifier a, Specifier b)
{
	return a.kind == b.kind;
}

static bool Compare(BaseType a, BaseType b)
{
	return a.kind == b.kind && (
		(a.kind == BASETYPE_PRIMITIVE && a.primitive   == b.primitive)   ||
		(a.kind == BASETYPE_STRUCT    && a.structure   == b.structure)   ||
		(a.kind == BASETYPE_ENUM      && a.enumeration == b.enumeration) ||
		(a.kind == BASETYPE_TUPLE     && Compare(a.tuple, b.tuple))      ||
		(a.kind == BASETYPE_FUNCTION  && Compare(a.input_params, b.input_params) && (a.output == b.output || Compare(*a.output, *b.output)))
	);
}

static bool Compare(Type a, Type b)
{
	return Compare(a.specifiers, b.specifiers) && Compare(a.base_type, b.base_type);
}

static BaseType ParseBaseType(Ast_TypeExpression* ast_type, Scope* scope, Semantic* semantic)
{
	BaseType base_type;

	if (ast_type->kind == AST_TYPE_EXPRESSION_PRIMITIVE)
	{
		base_type.kind = BASETYPE_PRIMITIVE;

		if      (ast_type->token->kind == Token_Bool)    base_type.primitive = PRIMITIVE_BOOL;
		else if (ast_type->token->kind == Token_Uint)    base_type.primitive = PRIMITIVE_UINT;
		else if (ast_type->token->kind == Token_Uint8)   base_type.primitive = PRIMITIVE_UINT8;
		else if (ast_type->token->kind == Token_Uint16)  base_type.primitive = PRIMITIVE_UINT16;
		else if (ast_type->token->kind == Token_Uint32)  base_type.primitive = PRIMITIVE_UINT32;
		else if (ast_type->token->kind == Token_Uint64)  base_type.primitive = PRIMITIVE_UINT64;
		else if (ast_type->token->kind == Token_Int)     base_type.primitive = PRIMITIVE_INT;
		else if (ast_type->token->kind == Token_Int8)    base_type.primitive = PRIMITIVE_INT8;
		else if (ast_type->token->kind == Token_Int16)   base_type.primitive = PRIMITIVE_INT16;
		else if (ast_type->token->kind == Token_Int32)   base_type.primitive = PRIMITIVE_INT32;
		else if (ast_type->token->kind == Token_Int64)   base_type.primitive = PRIMITIVE_INT64;
		else if (ast_type->token->kind == Token_Float16) base_type.primitive = PRIMITIVE_FLOAT16;
		else if (ast_type->token->kind == Token_Float32) base_type.primitive = PRIMITIVE_FLOAT32;
		else if (ast_type->token->kind == Token_Float64) base_type.primitive = PRIMITIVE_FLOAT64;
	}
	else if (ast_type->kind == AST_TYPE_EXPRESSION_FUNCTION)
	{
		base_type.kind = BASETYPE_FUNCTION;
		base_type.input_params = semantic->info.stack.CreateArray<Type>(ast_type->input_params.count);

		for (u32 i = 0; i < base_type.input_params.count; i++)
		{
			base_type.input_params[i] = ParseType(ast_type->input_params[i], scope, semantic);
		}

		base_type.output = null;

		if (ast_type->output_type)
		{
			base_type.output = semantic->info.stack.Allocate<Type>();
			*base_type.output = ParseType(ast_type->output_type, scope, semantic);
		}
	}
	else if (ast_type->kind == AST_TYPE_EXPRESSION_USERTYPE)
	{
		if (Struct* s = FindStruct(ast_type->token->info.string, scope))
		{
			base_type.kind = BASETYPE_STRUCT;
			base_type.structure = s;
		}
		else if (Enum* e = FindEnum(ast_type->token->info.string, scope))
		{
			base_type.kind = BASETYPE_ENUM;
			base_type.enumeration = e;
		}
		else
		{
			Error(semantic, ast_type->token->location, "Unknown type: %\n", ast_type->token->info.string);
		}
	}
	else if (ast_type->kind == AST_TYPE_EXPRESSION_TUPLE)
	{
		base_type.kind = BASETYPE_TUPLE;
		base_type.tuple = semantic->info.stack.CreateArray<Type>(ast_type->tuple.count);

		for (u32 i = 0; i < base_type.tuple.count; i++)
		{
			base_type.tuple[i] = ParseType(ast_type->tuple[i], scope, semantic);
		}
	}

	return base_type;
}

static Type ParseType(Ast_TypeExpression* ast_type, Scope* scope, Semantic* semantic)
{
	Type type;
	u32 count = 0;

	for (Ast_TypeExpression* t = ast_type;
		t->kind == AST_TYPE_EXPRESSION_ARRAY ||
		t->kind == AST_TYPE_EXPRESSION_OPTIONAL ||
		t->kind == AST_TYPE_EXPRESSION_POINTER;
		count++, t = t->subtype);

	type.specifiers = semantic->info.stack.CreateArray<Specifier>(count);

	for (Specifier* specifier = type.specifiers; specifier < type.specifiers.End(); specifier++)
	{
		specifier->array_size = 0;
		if (ast_type->kind == AST_TYPE_EXPRESSION_ARRAY)
		{
			if (ast_type->size_expression)
			{
				specifier->kind = SPECIFIER_FIXED_ARRAY;
			}
			else specifier->kind = SPECIFIER_DYNAMIC_ARRAY;

			specifier->array_size = 0;
		}
		else if (ast_type->kind == AST_TYPE_EXPRESSION_POINTER)  specifier->kind = SPECIFIER_POINTER;
		else if (ast_type->kind == AST_TYPE_EXPRESSION_OPTIONAL) specifier->kind = SPECIFIER_OPTIONAL;
		ast_type = ast_type->subtype;
	}

	type.base_type = ParseBaseType(ast_type, scope, semantic);
	return type;
}

static void GenerateStructs(List<Ast_Struct> ast_structs, Scope* scope, Semantic* semantic)
{
	scope->structs = semantic->info.stack.CreateArray<Struct>(ast_structs.count);

	for (u32 i = 0; i < ast_structs.count; i++)
	{
		Ast_Struct* ast_struct = &ast_structs[i];
		Struct* s = &scope->structs[i];
		s->ast = ast_struct;
		s->name = ast_struct->name->info.string;
		s->members = semantic->info.stack.CreateArray<Struct_Member>(s->members.count);

		for (u32 j = 0; j < s->members.count; j++)
		{
			Struct_Member* member = &s->members[j];
			member->ast = &ast_struct->members[j];
			member->name = member->ast->name->info.string;
		}
	}
}

static void GenerateEnums(List<Ast_Enum> ast_enums, Scope* scope, Semantic* semantic)
{
	scope->enums = semantic->info.stack.CreateArray<Enum>(ast_enums.count);

	for (u32 i = 0; i < ast_enums.count; i++)
	{
		Ast_Enum* ast_enum = &ast_enums[i];
		Enum* e = &scope->enums[i];
		e->ast = ast_enum;
		e->name = ast_enum->name->info.string;
		e->members = semantic->info.stack.CreateArray<Enum_Member>(ast_enum->members.count);

		for (u32 j = 0; j < e->members.count; j++)
		{
			Enum_Member* member = &e->members[j];
			member->ast = &ast_enum->members[j];
			member->name = member->ast->name->info.string;
		}
	}
}

static void GenerateFunctions(List<Ast_Function> ast_functions, Scope* scope, Semantic* semantic)
{
	scope->functions = semantic->info.stack.CreateArray<Function>(ast_functions.count);

	for (u32 i = 0; i < scope->functions.count; i++)
	{
		Function* function = &scope->functions[i];
		function->ast = &ast_functions[i];
		function->name = function->ast->name->info.string;
		function->parameters = semantic->info.stack.CreateArray<Parameter>(function->ast->params.count);

		for (u32 j = 0; j < function->parameters.count; j++)
		{
			Parameter* param = &function->parameters[j];
			param->ast  = &function->ast->params[j];
			param->name = param->ast->name->info.string;
			param->type = ParseType(param->ast->type, scope, semantic);
		}
	}
}

static Code GenerateCode(Ast_Code* ast_code, Scope* scope, Semantic* semantic)
{
	Code code;
	code.ast = ast_code;
	code.scope.parent_scope = scope;
	GenerateStructs(ast_code->structs,     &code.scope, semantic);
	GenerateEnums(ast_code->enums,         &code.scope, semantic);
	GenerateFunctions(ast_code->functions, &code.scope, semantic);

	for (Function* function = code.scope.functions; function < code.scope.functions.End(); function++)
	{
		function->code = GenerateCode(&function->ast->code, &code.scope, semantic);
	}

	return code;
}

static void GenerateExpression(Ast_Expression* ast_expression, Semantic* semantic)
{
	Expression expression;
	expression.ast = ast_expression;
}

void SemanticParse(Parse_Info info)
{
	Ast_Root* root = info.ast_root;
	Semantic semantic;
	ZeroMemory(&semantic);
	semantic.info = info;
	semantic.scope.parent_scope = null;

	GenerateStructs(root->structs,     &semantic.scope, &semantic);
	GenerateEnums(root->enums,         &semantic.scope, &semantic);
	GenerateFunctions(root->functions, &semantic.scope, &semantic);

	for (Function* function = semantic.scope.functions; function < semantic.scope.functions.End(); function++)
	{
		function->code = GenerateCode(&function->ast->code, &semantic.scope, &semantic);
	}

	Print("Semantic Parse finished successfully.\n");
}


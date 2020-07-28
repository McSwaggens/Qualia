#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"

template<typename ...Args>
[[noreturn]]
static void Error(Parse_Info* info, SourceLocation where, String format, Args&&... message_args)
{
	u32 margin = 2;
	u32 start = where.line;
	u32 number_of_lines = 1 + margin;

	Print("%:%:%: error: ", info->file_path, (where.line+1), (where.offset+1));
	Print(format, message_args...);

	for (u32 line = start; line < start + number_of_lines && line < info->lines.count; line++)
	{
		Print("%\n", String(info->lines[line], info->lines[line].Length()));
	}

	Fail();
}

static void ParseExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info);
static void ParseScope(Ast_Scope* scope, Parse_Info* info);
static void ParseFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info);
static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Parse_Info* info);

static void InitSpecifiers(Type* type, Parse_Info* info)
{
	type->specifiers = info->stack.Allocate<Type>(3);
	ZeroMemory(type->specifiers, 3);

	type->specifiers[0].kind = TYPE_SPECIFIER_POINTER;
	type->specifiers[0].subtype = type;
	type->specifiers[1].kind = TYPE_SPECIFIER_OPTIONAL;
	type->specifiers[1].subtype = type;
	type->specifiers[2].kind = TYPE_SPECIFIER_DYNAMIC_ARRAY;
	type->specifiers[2].subtype = type;
}

static Type* GetPointer(Type* type, Parse_Info* info)
{
	if (!type->specifiers)
	{
		InitSpecifiers(type, info);
	}

	return type->specifiers + 0;
}

static Type* GetOptional(Type* type, Parse_Info* info)
{
	if (!type->specifiers)
	{
		InitSpecifiers(type, info);
	}

	return type->specifiers + 1;
}

static Type* GetDynamicArray(Type* type, Parse_Info* info)
{
	if (!type->specifiers)
	{
		InitSpecifiers(type, info);
	}

	return type->specifiers + 2;
}

static Type* GetFixedArray(Type* type, u64 size, Parse_Info* info)
{
	for (u32 i = 0; i < type->fixed_arrays.count; i++)
	{
		if (type->fixed_arrays[i]->size == size)
		{
			return type->fixed_arrays[i];
		}
	}

	Type* new_type = info->stack.Allocate<Type>();
	ZeroMemory(new_type);
	new_type->kind = TYPE_SPECIFIER_FIXED_ARRAY;
	new_type->size = size;
	type->fixed_arrays.Add(new_type);

	return new_type;
}

static consteval Type NewPrimitiveType(Token_Kind kind)
{
	Type type;
	type.kind = TYPE_BASETYPE_PRIMITIVE;
	type.primitive = kind;
	type.size = 0;
	type.specifiers = null;
	type.fixed_arrays = null;
	type.tuple_extensions = null;
	type.function_extensions = null;
	return type;
}

static consteval Type NewEmptyTupleType()
{
	Type type;
	type.kind = TYPE_BASETYPE_TUPLE;
	type.size = 0;
	type.specifiers = null;
	type.fixed_arrays = null;
	type.tuple_extensions = null;
	type.function_extensions = null;
	return type;
}

Type empty_tuple       = NewEmptyTupleType();
Type primitive_bool    = NewPrimitiveType(TOKEN_BOOL);
Type primitive_int8    = NewPrimitiveType(TOKEN_INT8);
Type primitive_int16   = NewPrimitiveType(TOKEN_INT16);
Type primitive_int32   = NewPrimitiveType(TOKEN_INT32);
Type primitive_int64   = NewPrimitiveType(TOKEN_INT64);
Type primitive_uint8   = NewPrimitiveType(TOKEN_UINT8);
Type primitive_uint16  = NewPrimitiveType(TOKEN_UINT16);
Type primitive_uint32  = NewPrimitiveType(TOKEN_UINT32);
Type primitive_uint64  = NewPrimitiveType(TOKEN_UINT64);
Type primitive_float16 = NewPrimitiveType(TOKEN_FLOAT16);
Type primitive_float32 = NewPrimitiveType(TOKEN_FLOAT32);
Type primitive_float64 = NewPrimitiveType(TOKEN_FLOAT64);

static Type* GetPrimitiveType(Token_Kind kind)
{
	if      (kind == TOKEN_BOOL)    return &primitive_bool;
	else if (kind == TOKEN_INT)     return &primitive_int64;
	else if (kind == TOKEN_INT8)    return &primitive_int8;
	else if (kind == TOKEN_INT16)   return &primitive_int16;
	else if (kind == TOKEN_INT32)   return &primitive_int32;
	else if (kind == TOKEN_INT64)   return &primitive_int64;
	else if (kind == TOKEN_UINT)    return &primitive_uint64;
	else if (kind == TOKEN_UINT8)   return &primitive_uint8;
	else if (kind == TOKEN_UINT16)  return &primitive_uint16;
	else if (kind == TOKEN_UINT32)  return &primitive_uint32;
	else if (kind == TOKEN_UINT64)  return &primitive_uint64;
	else if (kind == TOKEN_FLOAT16) return &primitive_float16;
	else if (kind == TOKEN_FLOAT32) return &primitive_float32;
	else if (kind == TOKEN_FLOAT64) return &primitive_float64;
	else Unreachable();
}

static Type* GetBaseType(Token* token, Ast_Scope* scope)
{
	if (token->kind == TOKEN_IDENTIFIER)
	{
		while (scope)
		{
			for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
			{
				if (CompareStrings(token->info.string, s->name->info.string))
				{
					return &s->type;
				}
			}

			for (Ast_Enum* e = scope->enums; e < scope->enums.End(); e++)
			{
				if (CompareStrings(token->info.string, e->name->info.string))
				{
					return &e->type;
				}
			}

			scope = scope->parent;
		}
	}
	else return GetPrimitiveType(token->kind);
	return null;
}

static Type* GetBaseType(Ast_BaseType basetype, Ast_Scope* scope, Parse_Info* info)
{
	if (basetype.kind == AST_BASETYPE_PRIMITIVE)
	{
		return GetPrimitiveType(basetype.token->kind);
	}
	else if (basetype.kind == AST_BASETYPE_TUPLE)
	{
		u32 tuple_count = basetype.tuple.count;

		for (u32 i = 0; i < tuple_count; i++)
		{
			Ast_Type* t = &basetype.tuple[i];
			t->type = GetType(t, scope, info);
		}

		if (tuple_count == 1)
		{
			return basetype.tuple[0].type;
		}

		Type* front_type = basetype.tuple[0].type;
		for (u32 i = 0; i < front_type->tuple_extensions.count; i++)
		{
			Type* tuple = front_type->tuple_extensions[i];

			if (tuple->tuple.count != tuple_count)
			{
				continue;
			}

			bool fail = false;
			for (u32 j = 0; j < tuple_count; j++)
			{
				if (tuple->tuple[j] != basetype.tuple[j].type)
				{
					fail = true;
					break;
				}
			}

			if (!fail)
			{
				return tuple;
				Print("Found existing tuple type.\n");
			}
		}

		Type* new_type = info->stack.Allocate<Type>();
		Type** subtypes = info->stack.Allocate<Type*>(tuple_count);
		ZeroMemory(new_type);
		new_type->kind = TYPE_BASETYPE_TUPLE;
		new_type->tuple.data = subtypes;
		new_type->tuple.count = tuple_count;

		for (u32 i = 0; i < tuple_count; i++)
		{
			subtypes[i] = basetype.tuple[i].type;
		}

		front_type->tuple_extensions.Add(new_type);
		Print("Pushed new tuple type.\n");

		return new_type;
	}
	else if (basetype.kind == AST_BASETYPE_FUNCTION)
	{
		Type* input_tuple = GetType(basetype.function.input, scope, info);
		Type* output_type = null;

		if (basetype.function.output)
		{
			output_type = GetType(basetype.function.output, scope, info);
		}

		for (u32 i = 0; i < input_tuple->function_extensions.count; i++)
		{
			Type* func = input_tuple->function_extensions[i];
			if (func->function.output == output_type)
			{
				Print("Found existing function type.\n");
				return func;
			}
		}

		Type* new_func = info->stack.Allocate<Type>();
		ZeroMemory(new_func);
		new_func->kind = TYPE_BASETYPE_FUNCTION;
		new_func->function.input = input_tuple;
		new_func->function.output = output_type;
		input_tuple->function_extensions.Add(new_func);
		Print("Pushed new function type.\n");

		return new_func;
	}

	Assert(basetype.kind == AST_BASETYPE_USERTYPE);
	return GetBaseType(basetype.token, scope);
}

static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Parse_Info* info)
{
	if (!ast_type)
	{
		return &empty_tuple;
	}

	Type* type = GetBaseType(ast_type->basetype, scope, info);

	if (type && ast_type->specifiers)
	{
		for (Ast_Specifier* specifier = ast_type->specifiers.End()-1; specifier >= ast_type->specifiers.Begin(); specifier--)
		{
			if (specifier->kind == AST_SPECIFIER_POINTER)
			{
				type = GetPointer(type, info);
			}
			else if (specifier->kind == AST_SPECIFIER_OPTIONAL)
			{
				type = GetOptional(type, info);
			}
			else if (specifier->kind == AST_SPECIFIER_ARRAY)
			{
				if (specifier->size_expression == null)
				{
					type = GetDynamicArray(type, info);
				}
				else
				{
					ParseExpression(specifier->size_expression, scope, info);
					Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL); // @RemoveMe @Todo: Need to be removed.
					u64 size = specifier->size_expression->token->info.integer.value;
					type = GetFixedArray(type, size, info);
				}
			}
		}
	}

	return type;
}

static Ast_VariableDeclaration* GetVariable(Token* token, Ast_Scope* scope)
{
	while (scope)
	{
		for (Ast_VariableDeclaration** variable = scope->variables; variable < scope->variables.End(); variable++)
		{
			if (CompareStrings(token->info.string, (*variable)->name->info.string))
			{
				return *variable;
			}
		}

		scope = scope->parent;
	}

	return null;
}

static Ast_Function* GetFunction(Token* token, Span<Ast_Expression*> parameters, Ast_Scope* scope)
{
	while (scope)
	{
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
		{
			if (parameters.Length() == function->parameters.count && CompareStrings(token->info.string, function->name->info.string))
			{
				bool failed = false;
				for (u32 i = 0; i < parameters.Length(); i++)
				{
					if (parameters[i]->type != function->parameters[i].type)
					{
						failed = true;
						break;
					}
				}

				if (!failed)
				{
					return function;
				}
			}
		}

		scope = scope->parent;
	}

	return null;
}

static void ParseExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info)
{
	if (expression->kind == AST_EXPRESSION_TERMINAL)
	{
		if (Ast_VariableDeclaration* variable; expression->token->kind == TOKEN_IDENTIFIER && (variable = GetVariable(expression->token, scope)))
		{
			expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
			expression->variable = variable;
			expression->type = variable->type;
		}
		else if (Type* type = GetBaseType(expression->token, scope))
		{
			expression->type = type;

			if (type->kind == TYPE_BASETYPE_STRUCT)
			{
				expression->kind = AST_EXPRESSION_TERMINAL_STRUCT;
			}
			else if (type->kind == TYPE_BASETYPE_ENUM)
			{
				expression->kind = AST_EXPRESSION_TERMINAL_ENUM;
			}
			else if (type->kind == TYPE_BASETYPE_PRIMITIVE)
			{
				expression->kind = AST_EXPRESSION_TERMINAL_PRIMITIVE;
			}
		}
		else
		{
			Error(info, expression->token->location, "Unknown variable '%'\n", expression->token);
		}
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_LITERAL)
	{
		if (expression->token->kind == TOKEN_INTEGER_LITERAL)
		{
			if      (expression->token->info.integer.explicit_bytes == 0 && !expression->token->info.integer.is_unsigned) expression->type = &primitive_int64;
			else if (expression->token->info.integer.explicit_bytes == 1 && !expression->token->info.integer.is_unsigned) expression->type = &primitive_int8;
			else if (expression->token->info.integer.explicit_bytes == 2 && !expression->token->info.integer.is_unsigned) expression->type = &primitive_int16;
			else if (expression->token->info.integer.explicit_bytes == 4 && !expression->token->info.integer.is_unsigned) expression->type = &primitive_int32;
			else if (expression->token->info.integer.explicit_bytes == 8 && !expression->token->info.integer.is_unsigned) expression->type = &primitive_int64;
			else if (expression->token->info.integer.explicit_bytes == 0 &&  expression->token->info.integer.is_unsigned) expression->type = &primitive_uint64;
			else if (expression->token->info.integer.explicit_bytes == 1 &&  expression->token->info.integer.is_unsigned) expression->type = &primitive_uint8;
			else if (expression->token->info.integer.explicit_bytes == 2 &&  expression->token->info.integer.is_unsigned) expression->type = &primitive_uint16;
			else if (expression->token->info.integer.explicit_bytes == 4 &&  expression->token->info.integer.is_unsigned) expression->type = &primitive_uint32;
			else if (expression->token->info.integer.explicit_bytes == 8 &&  expression->token->info.integer.is_unsigned) expression->type = &primitive_uint64;
			else Unreachable();
		}
		else if (expression->token->kind == TOKEN_FLOAT_LITERAL)
		{
			if (expression->token->info.floating_point.explicit_bytes == 0)
			{
				expression->type = &primitive_float32; // @Fixme?
			}
			else if (expression->token->info.floating_point.explicit_bytes == 2)
			{
				expression->type = &primitive_float16;
			}
			else if (expression->token->info.floating_point.explicit_bytes == 4)
			{
				expression->type = &primitive_float32;
			}
			else if (expression->token->info.floating_point.explicit_bytes == 8)
			{
				expression->type = &primitive_float64;
			}
			else Unreachable();
		}
		else if (expression->token->kind == TOKEN_TRUE || expression->token->kind == TOKEN_FALSE)
		{
			expression->type = &primitive_bool;
		}
		else if (expression->token->kind == TOKEN_STRING_LITERAL)
		{
			u64 size = expression->token->info.span.Length();
			bool found = false;

			for (u32 i = 0; i < primitive_uint8.fixed_arrays.count; i++)
			{
				if (primitive_uint8.fixed_arrays[i]->size == size)
				{
					expression->type = primitive_uint8.fixed_arrays[i];
					found = true;
					break;
				}
			}

			if (!found)
			{
				Type* type = info->stack.Allocate<Type>();
				ZeroMemory(type);
				type->kind = TYPE_SPECIFIER_FIXED_ARRAY;
				type->size = size;
				type->subtype = &primitive_uint8;
				primitive_uint8.fixed_arrays.Add(type);
				expression->type = type;
			}
		}
	}
	else if (expression->kind == AST_EXPRESSION_TUPLE)
	{
		for (Ast_Expression** sub = expression->begin; sub < expression->end; sub++)
		{
			ParseExpression(*sub, scope, info);
		}
	}
	else if (expression->kind == AST_EXPRESSION_UNARY)
	{
		ParseExpression(expression->right, scope, info);
	}
	else if (expression->kind == AST_EXPRESSION_BINARY)
	{
		ParseExpression(expression->left,  scope, info);
		ParseExpression(expression->right, scope, info);
	}
	else if (expression->kind == AST_EXPRESSION_IF_ELSE)
	{
		ParseExpression(expression->left,   scope, info);
		ParseExpression(expression->middle, scope, info);
		ParseExpression(expression->right,  scope, info);

		if (expression->left->type != expression->right->type)
		{
			Error(info, expression->token->location, "Type mismatch between % and %.\n", expression->left->type, expression->right->type);
		}
	}
	else if (expression->kind == AST_EXPRESSION_CALL)
	{
		for (Ast_Expression** param = expression->begin; param < expression->end; param++)
		{
			ParseExpression(*param, scope, info);
		}

		if (expression->left->kind == AST_EXPRESSION_TERMINAL && expression->left->token->kind == TOKEN_IDENTIFIER)
		{
			// @Note: expression->left could still be a variable and not a function!

			if (Ast_Function* function = GetFunction(expression->left->token, Span(expression->begin, expression->end), scope); function)
			{
				expression->left->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
				expression->left->function = function;
				expression->type = function->return_type;
			}
			else if (Ast_VariableDeclaration* variable = GetVariable(expression->left->token, scope); variable)
			{
				expression->left->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				expression->left->variable = variable;
				// @Todo @Bug: Need to check if the variable is a function type e.g. ()->()
			}
			else
			{
				Error(info, expression->left->token->location, "Function % not found.\n", expression->left->token);
			}
		}
		else
		{
			ParseExpression(expression->left, scope, info);
		}
	}
	else if (expression->kind == AST_EXPRESSION_LAMBDA)
	{
	}
	else if (expression->kind == AST_EXPRESSION_SUBSCRIPT)
	{
		ParseExpression(expression->left,  scope, info);
		ParseExpression(expression->right, scope, info);
	}
}

static void ParseScope(Ast_Scope* scope, Parse_Info* info)
{
	for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
	{
		ZeroMemory(&s->type);
		s->type.kind = TYPE_BASETYPE_STRUCT;
		s->type.structure = s;

		for (Ast_Struct* so = scope->structs; so < s; so++)
		{
			if (CompareStrings(s->name->info.string, so->name->info.string))
			{
				Error(info, s->name->location, "Duplicate struct called '%'\n", s->name);
			}
		}

		for (Ast_Enum* e = scope->enums; e < scope->enums.End(); e++)
		{
			if (CompareStrings(s->name->info.string, e->name->info.string))
			{
				if (s->name->location.line < e->name->location.line)
				{
					Error(info, e->name->location, "Duplicate type called '%'\n", e->name);
				}
				else
				{
					Error(info, s->name->location, "Duplicate type called '%'\n", s->name);
				}
			}
		}

		for (Ast_Struct_Member* member = s->members; member < s->members.End(); member++)
		{
			for (Ast_Struct_Member* member_other = s->members; member_other < member; member_other++)
			{
				if (CompareStrings(member->name->info.string, member_other->name->info.string))
				{
					Error(info, member->name->location, "Duplicate member called '%' in struct %\n", member->name, s->name);
				}
			}
		}
	}

	for (Ast_Enum* e = scope->enums; e < scope->enums.End(); e++)
	{
		ZeroMemory(&e->type);
		e->type.kind = TYPE_BASETYPE_ENUM;
		e->type.enumeration = e;

		for (Ast_Enum* eo = scope->enums; eo < e; eo++)
		{
			if (CompareStrings(e->name->info.string, eo->name->info.string))
			{
				Error(info, e->name->location, "Duplicate enum called '%'\n", e->name);
			}
		}

		for (Ast_Enum_Member* member = e->members; member < e->members.End(); member++)
		{
			for (Ast_Enum_Member* member_other = e->members; member_other < member; member_other++)
			{
				if (CompareStrings(member->name->info.string, member_other->name->info.string))
				{
					Error(info, member->name->location, "Duplicate member called '%' in enum %\n", member->name, e->name);
				}
			}
		}
	}

	for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
	{
		for (Ast_Struct_Member* member = s->members; member < s->members.End(); s++)
		{
			member->type.type = GetType(&member->type, scope, info);

			if (!member->type.type)
			{
				Error(info, member->type.basetype.token->location, "Unknown type '%'\n", member->type.basetype.token);
			}
		}
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ParseFunction(f, scope, info);
	}
}

static void ParseCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Parse_Info* info)
{
	code->scope.parent = scope;
	ParseScope(&code->scope, info);

	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		if (statement->kind == AST_STATEMENT_VARIABLE_DECLARATION)
		{
			Ast_VariableDeclaration* variable = &statement->variable_declaration;

			for (Ast_VariableDeclaration** other_variable = code->scope.variables; other_variable < code->scope.variables.End(); other_variable++)
			{
				if (CompareStrings(variable->name->info.string, (*other_variable)->name->info.string))
				{
					Error(info, variable->name->location, "Variable with name '%' already declared in this scope.\n", variable->name);
				}
			}

			if (variable->explicit_type != null)
			{
				variable->type = GetType(variable->explicit_type, &code->scope, info);
				variable->explicit_type->type = variable->type;
			}
			else
			{
				// @Todo: Infer type from expression which is guaranteed not to be null.
			}

			if (variable->assignment != null)
			{
				ParseExpression(variable->assignment, &code->scope, info);
			}

			code->scope.variables.Add(variable);
		}
		else if (statement->kind == AST_STATEMENT_ASSIGNMENT)
		{
			ParseExpression(statement->assignment.right, &code->scope, info);
		}
		else if (statement->kind == AST_STATEMENT_BRANCH_BLOCK)
		{
			for (Ast_Branch* branch = statement->branch.branches; branch < statement->branch.branches.End(); branch++)
			{
				ParseCode(&branch->code, &code->scope, function, info);
			}
		}
		else if (statement->kind == AST_STATEMENT_BREAK)
		{
			// @Todo: Check if in loop
		}
		else if (statement->kind == AST_STATEMENT_DEFER)
		{
			ParseCode(&statement->defer.code, &code->scope, function, info);
		}
		else if (statement->kind == AST_STATEMENT_EXPRESSION)
		{
			ParseExpression(statement->expression, &code->scope, info);
		}
		else if (statement->kind == AST_STATEMENT_RETURN)
		{
			code->does_return = true;

			if (statement->ret.expression)
			{
				ParseExpression(statement->ret.expression, &code->scope, info);

				if (!function->return_type)
				{
					Error(info, statement->ret.token->location, "Unexpected return value for function that doesn't return anything.\n");
				}

				if (statement->ret.expression->type != function->return_type)
				{
					Error(info, statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
				}
			}
			else if (function->return_type)
			{
				Error(info, statement->ret.token->location, "Expected return value with type: %\n", function->return_type);
			}
		}
		else if (statement->kind == AST_STATEMENT_ALIAS)
		{
		}
	}
}

static void ParseFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info)
{
	for (Ast_VariableDeclaration* param = function->parameters; param < function->parameters.End(); param++)
	{
		param->explicit_type->type = GetType(param->explicit_type, scope, info);
		param->type = param->explicit_type->type;
		function->code.scope.variables.Add(param);

		if (!param->explicit_type->type)
		{
			Error(info, param->explicit_type->basetype.token->location, "Unknown type '%'\n", param->explicit_type->basetype.token);
		}

		for (Ast_VariableDeclaration* param_other = function->parameters; param_other < param; param_other++)
		{
			if (CompareStrings(param_other->name->info.string, param->name->info.string))
			{
				Error(info, param->name->location, "Duplicate parameter called '%'\n", param->name->info.string);
			}
		}
	}

	if (function->ast_return_type)
	{
		function->ast_return_type->type = GetType(function->ast_return_type, scope, info);
		function->return_type = function->ast_return_type->type;

		if (function->ast_return_type->type == null)
		{
			Error(info, function->ast_return_type->basetype.token->location, "Unknown type: %\n", function->ast_return_type);
		}
	}

	ParseCode(&function->code, scope, function, info);
}

void SemanticParse(Parse_Info* info)
{
	Ast_Root* root = info->ast_root;
	root->scope.parent = null;
	ParseScope(&root->scope, info);
}


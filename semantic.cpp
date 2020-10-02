#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"
#include "util.h"

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

template<typename ...Args>
[[noreturn]]
static void Error(Parse_Info* info, Span<Token> where, String format, Args&&... message_args)
{
	u32 margin = 2;
	u32 start = where[0].location.line;
	SourceLocation loc_begin = where.Begin()->location;
	SourceLocation loc_end = (where.End()-1)->location; // @Bug: What if begin = end. Is this invalid input? IDK
	u32 number_of_lines = loc_end.line - loc_begin.line + margin + 1;

	Print("%:%:%: error: ", info->file_path, (loc_begin.line+1), (loc_begin.offset+1));
	Print(format, message_args...);

	// @Todo: Coloring/Highlighting

	for (u32 line = start; line < start + number_of_lines && line < info->lines.count; line++)
	{
		Print("%\n", String(info->lines[line], info->lines[line].Length()));
	}

	Fail();
}

static bool IsTerminalExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_TERMINAL:
		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_LITERAL:
		case AST_EXPRESSION_TERMINAL_VARIABLE:
		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
			return true;
		default:
			return false;
	}
}

static bool IsUnaryExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_UNARY_BINARY_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_VALUE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
			return true;
		default:
			return false;
	}
}

static bool IsBinaryExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		case AST_EXPRESSION_BINARY_DOT:
		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
			return true;
		default:
			return false;
	}
}

struct MemoryBlock
{
	char* head;
	u64   size;
	MemoryBlock* prev;
	MemoryBlock* next;
	char  data[];
};

struct Interpreter
{
	MemoryBlock* block;
	Ast_Statement* statement;
};

static void ParseExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info);
static void ParseScope(Ast_Scope* scope, Parse_Info* info);
static void ParseCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Parse_Info* info);
static void ParseFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info);
static Type* GetTypeFromTupleExpression(Array<Ast_Expression*> expressions, Parse_Info* info);
static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Parse_Info* info);

static MemoryBlock* CreateMemoryBlock(u64 min_size, MemoryBlock* prev = null)
{
	u64 size = 0x1000;
	u64 header_size = sizeof(MemoryBlock);

	if (prev)
	{
		size = prev->size * 2;
	}

	if (size - header_size <= min_size)
	{
		size = NextPow2(min_size + header_size);
	}

	MemoryBlock* block = Cast(AllocateVirtualPage(size), MemoryBlock*);
	block->size = size - header_size;
	block->head = block->data;
	block->prev = prev;
	block->next = null;

	if (prev)
	{
		MemoryBlock* pn = prev->next;
		prev->next = block;
		pn->prev = block;
		block->next = pn;
	}

	return block;
}

// @Note: This doesn't calculate the minimum memory needed to represent the stackframe which would be ideal for producing optimized binaries.
//        Another function needs to created for that.
static u64 CalculateStackFrameSize(Ast_Code* code, u64 offset = 0)
{
	u64 initial_offset = offset;

	for (u32 i = 0; i < code->scope.variables.count; i++)
	{
		Ast_VariableDeclaration* variable = code->scope.variables[i];
		variable->offset = offset;
		offset += variable->type->size;
	}

	for (u32 i = 0; i < code->statements.count; i++)
	{
		Ast_Statement* statement = &code->statements[i];

		if (statement->kind == AST_STATEMENT_BRANCH_BLOCK)
		{
			for (Ast_Branch* branch = statement->branch_block.branches; branch < statement->branch_block.branches.End(); branch++)
			{
				offset = CalculateStackFrameSize(&branch->code, offset);
			}
		}
		else if (statement->kind == AST_STATEMENT_DEFER)
		{
			offset = CalculateStackFrameSize(&statement->defer.code, offset);
		}
	}

	code->frame_size = offset - initial_offset;

	return offset;
}

static u64 GetFreeSpace(MemoryBlock* block)
{
	return block->data + block->size - block->head;
}

static u64 GetUsedSpace(MemoryBlock* block)
{
	return block->head - block->data;
}

static Value* Push(Type* type, Interpreter* interpreter)
{
	u64 size = type->size;

	if (GetFreeSpace(interpreter->block) < size)
	{
		if (interpreter->block->next && interpreter->block->next->size > size)
		{
			interpreter->block = interpreter->block->next;
		}
		else
		{
			interpreter->block = CreateMemoryBlock(size, interpreter->block);
		}
	}

	void* result = interpreter->block->head;
	interpreter->block->head += size;

	return Cast(result, Value*);
}

static void Pop(Type* type, Interpreter* interpreter)
{
	u64 size = type->size;

	Assert(GetFreeSpace(interpreter->block) >= size);

	interpreter->block->head -= size;

	if (interpreter->block->head == interpreter->block->data && interpreter->block->prev)
	{
		interpreter->block = interpreter->block->prev;
	}
}

static StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter)
{
	StackFrame frame;
	u64 size = CalculateStackFrameSize(&function->code);

	if (!interpreter->block || size > GetFreeSpace(interpreter->block))
	{
		interpreter->block = CreateMemoryBlock(size, interpreter->block);
	}

	frame.data = Span(interpreter->block->head, size);
	interpreter->block->head += size;

	ZeroMemory(frame.data);
	return frame;
}

static Interpreter* CreateInterpreter(Parse_Info* info)
{
	Interpreter* interpreter = info->stack.Allocate<Interpreter>();
	interpreter->block = CreateMemoryBlock(0x10000);
	return interpreter;
}

static void InitSpecifiers(Type* type, Parse_Info* info)
{
	type->specifiers = info->stack.Allocate<Type>(3);
	ZeroMemory(type->specifiers, 3);

	type->specifiers[0].kind = TYPE_SPECIFIER_POINTER;
	type->specifiers[0].subtype = type;
	type->specifiers[0].size = 8;

	type->specifiers[1].kind = TYPE_SPECIFIER_OPTIONAL;
	type->specifiers[1].subtype = type;
	type->specifiers[1].size = type->size + 1;

	type->specifiers[2].kind = TYPE_SPECIFIER_DYNAMIC_ARRAY;
	type->specifiers[2].subtype = type;
	type->specifiers[2].size = 16;
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

static Type* GetFixedArray(Type* type, u64 length, Parse_Info* info)
{
	for (u32 i = 0; i < type->fixed_arrays.count; i++)
	{
		if (type->fixed_arrays[i]->length == length)
		{
			return type->fixed_arrays[i];
		}
	}

	Type* new_type = info->stack.Allocate<Type>();
	ZeroMemory(new_type);
	new_type->kind = TYPE_SPECIFIER_FIXED_ARRAY;
	new_type->length = length;
	type->fixed_arrays.Add(new_type);

	return new_type;
}

static consteval Type NewPrimitiveType(Token_Kind kind, u64 size)
{
	Type type;
	type.kind = TYPE_BASETYPE_PRIMITIVE;
	type.primitive = kind;
	type.size = size;
	type.length = 0;
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
	type.length = 0;
	type.specifiers = null;
	type.fixed_arrays = null;
	type.tuple_extensions = null;
	type.function_extensions = null;
	return type;
}

Type empty_tuple       = NewEmptyTupleType();
Type primitive_bool    = NewPrimitiveType(TOKEN_BOOL,    1);
Type primitive_int8    = NewPrimitiveType(TOKEN_INT8,    1);
Type primitive_int16   = NewPrimitiveType(TOKEN_INT16,   2);
Type primitive_int32   = NewPrimitiveType(TOKEN_INT32,   4);
Type primitive_int64   = NewPrimitiveType(TOKEN_INT64,   8);
Type primitive_uint8   = NewPrimitiveType(TOKEN_UINT8,   1);
Type primitive_uint16  = NewPrimitiveType(TOKEN_UINT16,  2);
Type primitive_uint32  = NewPrimitiveType(TOKEN_UINT32,  4);
Type primitive_uint64  = NewPrimitiveType(TOKEN_UINT64,  8);
Type primitive_float16 = NewPrimitiveType(TOKEN_FLOAT16, 2);
Type primitive_float32 = NewPrimitiveType(TOKEN_FLOAT32, 4);
Type primitive_float64 = NewPrimitiveType(TOKEN_FLOAT64, 8);

static constexpr Type* GetPrimitiveType(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_BOOL:    return &primitive_bool;
		case TOKEN_INT:     return &primitive_int64;
		case TOKEN_INT8:    return &primitive_int8;
		case TOKEN_INT16:   return &primitive_int16;
		case TOKEN_INT32:   return &primitive_int32;
		case TOKEN_INT64:   return &primitive_int64;
		case TOKEN_UINT:    return &primitive_uint64;
		case TOKEN_UINT8:   return &primitive_uint8;
		case TOKEN_UINT16:  return &primitive_uint16;
		case TOKEN_UINT32:  return &primitive_uint32;
		case TOKEN_UINT64:  return &primitive_uint64;
		case TOKEN_FLOAT16: return &primitive_float16;
		case TOKEN_FLOAT32: return &primitive_float32;
		case TOKEN_FLOAT64: return &primitive_float64;
		default: Unreachable();
	}
}

static bool IsConvertableToBool(Type* type)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE
		|| type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_SPECIFIER_OPTIONAL
		|| type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsIntegerType(Type* type)
{
	return type == &primitive_int8
		|| type == &primitive_int16
		|| type == &primitive_int32
		|| type == &primitive_int64
		|| type == &primitive_uint8
		|| type == &primitive_uint16
		|| type == &primitive_uint32
		|| type == &primitive_uint64;
}

static bool IsFloatType(Type* type)
{
	return type == &primitive_float16
		|| type == &primitive_float32
		|| type == &primitive_float64;
}

static bool IsNumericalType(Type* type)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE
		|| type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_BASETYPE_ENUM;
}

static bool AreTypesCompatible(Type* a, Type* b)
{
	return a == b || (a->kind == TYPE_BASETYPE_PRIMITIVE && b->kind == TYPE_BASETYPE_PRIMITIVE);
}

static void Interpret(Ast_Function* function, char* output, Interpreter interpreter, Parse_Info* info);

static void Interpret(Ast_Expression* expression, char* output)
{
	Print("Interpreting expression: %\n", expression);
	if (IsBinaryExpression(expression->kind))
	{
		u64 left_size = Max(expression->left->type->size, 8llu);
		u64 right_size = Max(expression->right->type->size, 8llu);

		char left[left_size];
		char right[right_size];

		ZeroMemory(left, left_size);
		ZeroMemory(right, right_size);

		Interpret(expression->left,  left);
		Interpret(expression->right, right);

		if (IsIntegerType(expression->left->type) && IsIntegerType(expression->right->type))
		{
			u64 n = *Cast(left, u64*) + *Cast(right, u64*);
			CopyMemory(output, (char*)&n, expression->type->size);
			Print("Intermediate value: %\n", n);
		}
		else Assert();
	}
	else if (expression->kind == AST_EXPRESSION_TERMINAL_LITERAL)
	{
		if (expression->token->kind == TOKEN_INTEGER_LITERAL)
		{
			CopyMemory(output, (char*)&expression->token->info.integer.value, expression->type->size);
		}
		else if (expression->token->kind == TOKEN_FLOAT_LITERAL)
		{
			if (expression->type->primitive == TOKEN_FLOAT64)
			{
				*(f64*)output = (f64)expression->token->info.floating_point.value;
			}
			else if (expression->type->primitive == TOKEN_FLOAT32)
			{
				*(f32*)output = (f32)expression->token->info.floating_point.value;
			}
			else Assert();
		}
		else if (expression->token->kind == TOKEN_TRUE)
		{
			*(bool*)output = true;
		}
		else if (expression->token->kind == TOKEN_FALSE)
		{
			*(bool*)output = false;
		}
		else Assert();
	}
	else Assert();
}

static void Interpret(Ast_Code* code, char* output, StackFrame frame, Interpreter* interpreter, Parse_Info* info)
{
	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_EXPRESSION:
			{
				Ast_Expression* expression = statement->expression;
				char data[Max(expression->type->size, 8llu)];
				Interpret(expression, data);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* variable = &statement->variable_declaration;
				char* variable_data = frame.GetData(variable);
				ZeroMemory(frame.GetData(variable), variable->type->size);

				if (variable->assignment)
				{
					if (variable->assignment->is_referential_value)
					{
						union { char array[8]; char* pointer; } ref;
						Interpret(variable->assignment, ref.array);
						CopyMemory(frame.GetData(variable), ref.pointer, Min(variable->type->size, variable->assignment->type->size));
					}
					else
					{
						char data[variable->assignment->type->size];
						Interpret(variable->assignment, data);
						CopyMemory(frame.GetData(variable), data, Min(variable->type->size, variable->assignment->type->size));
					}
				}

			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				union { char array[8]; char* pointer; } ref;
				Interpret(assignment->left, ref.array);
				Interpret(assignment->right, ref.pointer);
			} break;

			case AST_STATEMENT_BRANCH_BLOCK:
			case AST_STATEMENT_DEFER:
			case AST_STATEMENT_CLAIM:
			case AST_STATEMENT_ALIAS:
			case AST_STATEMENT_RETURN:
			case AST_STATEMENT_BREAK:
			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
				Assert();
		}
	}
}

static void Interpret(Ast_Function* function, char* output, Interpreter* interpreter, Parse_Info* info)
{
	Print("Interpreting function %\n", function->name);
	StackFrame frame = CreateStackFrame(function, interpreter);
	Interpret(&function->code, output, frame, interpreter, info);
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

		if (!tuple_count)
		{
			Error(info, basetype.token->location, "Empty tuple is an invalid type.\n");
		}

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
			new_type->size += basetype.tuple[i].type->size;
		}

		front_type->tuple_extensions.Add(new_type);

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
				return func;
			}
		}

		Type* new_func = info->stack.Allocate<Type>();
		ZeroMemory(new_func);
		new_func->kind = TYPE_BASETYPE_FUNCTION;
		new_func->function.input = input_tuple;
		new_func->function.output = output_type;
		new_func->size = 8;
		input_tuple->function_extensions.Add(new_func);

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
					u64 length = specifier->size_expression->token->info.integer.value;
					type = GetFixedArray(type, length, info);
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
			if (function->parameters.count == parameters.Length() && CompareStrings(token->info.string, function->name->info.string))
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

static Ast_Struct_Member* FindStructMember(Ast_Struct* ast_struct, String name)
{
	for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
	{
		if (CompareStrings(name, member->name->info.string))
		{
			return member;
		}
	}

	return null;
}

static Ast_Enum_Member* FindEnumMember(Ast_Enum* ast_enum, String name)
{
	for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++)
	{
		if (CompareStrings(name, member->name->info.string))
		{
			return member;
		}
	}

	return null;
}

static bool IsBaseType(Type* type)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE
		|| type->kind == TYPE_BASETYPE_STRUCT
		|| type->kind == TYPE_BASETYPE_ENUM
		|| type->kind == TYPE_BASETYPE_FUNCTION
		|| type->kind == TYPE_BASETYPE_TUPLE;
}

static Type* FindBaseType(Type* type)
{
	while (!IsBaseType(type)) type = type->subtype;
	return type;
}

static void ParseExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL:
		{
			if (Ast_VariableDeclaration* variable; expression->token->kind == TOKEN_IDENTIFIER && (variable = GetVariable(expression->token, scope)))
			{
				expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				expression->variable = variable;
				expression->type = variable->type;
				expression->is_referential_value = true;
				expression->is_pure = variable->is_pure;
				expression->can_constantly_evaluate = variable->can_constantly_evaluate;
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
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			if (expression->token->kind == TOKEN_INTEGER_LITERAL)
			{
				if (expression->token->info.integer.is_unsigned)
				{
					switch (expression->token->info.integer.explicit_bytes)
					{
						case 0: expression->type = &primitive_uint64; break;
						case 1: expression->type = &primitive_uint8;  break;
						case 2: expression->type = &primitive_uint16; break;
						case 4: expression->type = &primitive_uint32; break;
						case 8: expression->type = &primitive_uint64; break;
						default: Unreachable();
					}
				}
				else
				{
					switch (expression->token->info.integer.explicit_bytes)
					{
						case 0: expression->type = &primitive_int64; break;
						case 1: expression->type = &primitive_int8;  break;
						case 2: expression->type = &primitive_int16; break;
						case 4: expression->type = &primitive_int32; break;
						case 8: expression->type = &primitive_int64; break;
						default: Unreachable();
					}
				}
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
				u64 length = expression->token->info.span.Length();
				bool found = false;

				for (u32 i = 0; i < primitive_uint8.fixed_arrays.count; i++)
				{
					if (primitive_uint8.fixed_arrays[i]->length == length)
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
					type->length = length;
					type->subtype = &primitive_uint8;
					primitive_uint8.fixed_arrays.Add(type);
					expression->type = type;
				}
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			expression->can_constantly_evaluate = true;
			expression->is_pure = true;
			expression->is_referential_value = true;

			if (!expression->begin)
			{
				Error(info, expression->span, "Empty tuples aren't allowed.\n");
			}

			for (u32 i = 0; i < expression->end - expression->begin; i++)
			{
				Ast_Expression* sub = expression->begin[i];
				ParseExpression(sub, scope, info);

				if (!sub->can_constantly_evaluate)
				{
					expression->can_constantly_evaluate = false;
				}

				if (!sub->is_pure)
				{
					expression->is_pure = false;
				}

				if (!sub->is_referential_value)
				{
					expression->is_referential_value = false;
				}
			}

			expression->type = GetTypeFromTupleExpression(Array(expression->begin, expression->end - expression->begin), info);
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			ParseExpression(expression->right, scope, info);
			expression->type = GetPointer(expression->right->type, info);
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = false;

			if (!expression->right->is_referential_value)
			{
				Error(info, expression->span, "Cannot get address of a non-referential value.\n");
			}
		} break;

		case AST_EXPRESSION_UNARY_VALUE_OF:
		{
			ParseExpression(expression->right, scope, info);
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = true;

			if (expression->right->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(info, expression->span, "Cannot dereference type: %\n", expression->right->type);
			}

			expression->type = expression->right->type->subtype;
		} break;

		case AST_EXPRESSION_UNARY_BINARY_NOT:
		{
			ParseExpression(expression->right, scope, info);
			expression->type = expression->right->type;
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = false;
			// @Todo: Check if numeric type
			// @Todo: Enforce integer type?
		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			ParseExpression(expression->right, scope, info);
			expression->type = expression->right->type;
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = false;
			// @Todo: Check if numeric type
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			ParseExpression(expression->right, scope, info);
			expression->type = expression->right->type;
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = false;
			// @Todo: Check if numeric type
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			ParseExpression(expression->right, scope, info);
			expression->type = &primitive_bool;
			expression->can_constantly_evaluate = expression->right->can_constantly_evaluate;
			expression->is_pure = expression->right->is_pure;
			expression->is_referential_value = false;
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			ParseExpression(expression->left,  scope, info);
			expression->is_referential_value = true;

			// @Todo: Handle inferred function calls.

			Type* type = expression->left->type;

			if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY || TYPE_SPECIFIER_FIXED_ARRAY)
			{
				if (expression->right->kind == AST_EXPRESSION_TERMINAL)
				{
					if (expression->right->token->kind != TOKEN_IDENTIFIER)
					{
						Error(info, expression->span, "Invalid dot expression on array.\n");
					}

					if (CompareStrings(expression->right->token->info.string, "length"))
					{
						expression->type = GetPrimitiveType(TOKEN_UINT);
					}
					else
					{
						Error(info, expression->span, "Invalid array member: %\n", expression->right->token);
					}
				}
				else
				{
					Error(info, expression->span, "Invalid dot expression on array.\n");
				}
			}
			else
			{
				while (type->kind == TYPE_SPECIFIER_POINTER) type = type->subtype;

				if (type->kind == TYPE_BASETYPE_STRUCT)
				{
					Assert(expression->right->kind == AST_EXPRESSION_TERMINAL && expression->right->token->kind == TOKEN_IDENTIFIER);
					Ast_Struct* ast_struct = type->structure;
					Ast_Struct_Member* member = FindStructMember(ast_struct, expression->right->token->info.string);

					if (!member)
					{
						Error(info, expression->span, "Struct % does not have a member named %\n", ast_struct->name, expression->right->token);
					}

					expression->type = member->type.type;
					expression->right->struct_member = member;
				}
				else
				{
					Error(info, expression->span, "Cannot dot into type: %\n", expression->left->type);
				}
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = &primitive_bool;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!AreTypesCompatible(expression->left->type, expression->right->type))
			{
				Error(info, expression->token->location, "% and % are incompatible types.\n", expression->left->type, expression->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = &primitive_bool;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!IsNumericalType(expression->left->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->left->type);
			}

			if (!IsNumericalType(expression->right->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = expression->left->type;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!IsNumericalType(expression->left->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->left->type);
			}

			if (!IsNumericalType(expression->right->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = expression->left->type;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!IsNumericalType(expression->left->type) || IsFloatType(expression->left->type))
			{
				Error(info, expression->span, "Cannot use bitwise AND with type: %\n", expression->left->type);
			}

			if (!IsNumericalType(expression->right->type) || IsFloatType(expression->right->type))
			{
				Error(info, expression->span, "Cannot use bitwise AND with type: %\n", expression->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = expression->left->type;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!IsNumericalType(expression->left->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->left->type);
			}

			if (!IsNumericalType(expression->right->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);
			expression->type = expression->left->type;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (!IsNumericalType(expression->left->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->left->type);
			}

			if (!IsNumericalType(expression->right->type))
			{
				Error(info, expression->token->location, "% is not a numerical type.\n", expression->right->type);
			}
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			ParseExpression(expression->left,   scope, info);
			ParseExpression(expression->middle, scope, info);
			ParseExpression(expression->right,  scope, info);
			expression->type = expression->left->type;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->middle->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->middle->is_pure && expression->right->is_pure;
			expression->is_referential_value = false;

			if (expression->left->type != expression->right->type)
			{
				Error(info, expression->token->location, "Type mismatch between % and %.\n", expression->left->type, expression->right->type);
			}
		} break;

		case AST_EXPRESSION_CALL:
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
					expression->type = variable->type->function.output;
					expression->is_referential_value = true;
					expression->is_pure = variable->is_pure;
					expression->can_constantly_evaluate = variable->can_constantly_evaluate;

					if (variable->type->kind != TYPE_BASETYPE_FUNCTION)
					{
						Error(info, expression->left->token->location, "Variable % with type % cannot be called like a function.\n", variable->name, variable->type);
					}
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
		} break;

		case AST_EXPRESSION_LAMBDA:
		case AST_EXPRESSION_SUBSCRIPT:
		{
			ParseExpression(expression->left,  scope, info);
			ParseExpression(expression->right, scope, info);

			if (expression->left->type->kind != TYPE_SPECIFIER_FIXED_ARRAY   &&
				expression->left->type->kind != TYPE_SPECIFIER_DYNAMIC_ARRAY &&
				expression->left->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(info, expression->left->span, "Expression with type % is not a valid array.\n", expression->left->span);
			}

			if (!IsIntegerType(expression->right->type))
			{
				Error(info, expression->right->span, "Subscript index must be an integer, not: %\n", expression->right->type);
			}

			expression->type = expression->left->type->subtype;
			expression->can_constantly_evaluate = expression->left->can_constantly_evaluate && expression->right->can_constantly_evaluate;
			expression->is_pure = expression->left->is_pure && expression->right->is_pure;
			expression->is_referential_value = true;
		} break;

		default: Unreachable();
	}
}

static void GenerateClosure(Ast_Struct* target, Ast_Struct* ast_struct);
static void GenerateClosure(Ast_Struct* target, Array<Type*> tuple);

static void GenerateClosure(Ast_Struct* target, Array<Type*> tuple)
{
	for (u32 i = 0; i < tuple.count; i++)
	{
		Type* type = tuple[i];
		if (type->kind == TYPE_BASETYPE_STRUCT)
		{
			if (target->closure.AddIfUnique(type->structure))
			{
				GenerateClosure(target, type->structure);
			}
		}
		else if (type->kind == TYPE_BASETYPE_TUPLE)
		{
			GenerateClosure(target, type->tuple);
		}
	}
}

static void GenerateClosure(Ast_Struct* target, Ast_Struct* ast_struct)
{
	for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
	{
		Type* type = member->type.type;
		if (type->kind == TYPE_BASETYPE_STRUCT)
		{
			if (target->closure.AddIfUnique(type->structure))
			{
				GenerateClosure(target, type->structure);
			}
		}
		else if (type->kind == TYPE_BASETYPE_TUPLE)
		{
			GenerateClosure(target, type->tuple);
		}
	}
}

static void CheckForCircularDependencies(Ast_Struct* ast_struct, Parse_Info* info)
{
	GenerateClosure(ast_struct, ast_struct);

	if (ast_struct->closure.Contains(ast_struct))
	{
		Error(info, ast_struct->name->location, "The closure of struct % contains % (circularly dependent)\n", ast_struct->name, ast_struct->name);
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
		for (Ast_Struct_Member* member = s->members; member < s->members.End(); member++)
		{
			member->type.type = GetType(&member->type, scope, info);

			if (!member->type.type)
			{
				Error(info, member->type.basetype.token->location, "Unknown type '%'\n", member->type.basetype.token);
			}
		}
	}

	for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
	{
		CheckForCircularDependencies(s, info);
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ParseFunction(f, scope, info);
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ParseCode(&f->code, scope, f, info);
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

			if (variable->assignment)
			{
				ParseExpression(variable->assignment, &code->scope, info);

				if (!variable->assignment->type)
				{
					Error(info, variable->assignment->span, "Expression does not have a type.\n");
				}
			}

			if (variable->explicit_type != null)
			{
				variable->type = GetType(variable->explicit_type, &code->scope, info);
				variable->explicit_type->type = variable->type;

				if (variable->assignment)
				{
					if (!AreTypesCompatible(variable->type, variable->assignment->type))
					{
						Error(info, variable->name->location, "Cannot assign expression with type % to variable with type %\n", variable->assignment->type, variable->type);
					}
				}
			}
			else
			{
				variable->type = variable->assignment->type;
			}

			// Print("Variable % has type %\n", variable->name, variable->type);

			code->scope.variables.Add(variable);
		}
		else if (statement->kind == AST_STATEMENT_ASSIGNMENT
			||   statement->kind == AST_STATEMENT_ASSIGNMENT_ADD
			||   statement->kind == AST_STATEMENT_ASSIGNMENT_SUBTRACT
			||   statement->kind == AST_STATEMENT_ASSIGNMENT_MULTIPLY
			||   statement->kind == AST_STATEMENT_ASSIGNMENT_DIVIDE
			||   statement->kind == AST_STATEMENT_ASSIGNMENT_POWER)
		{
			ParseExpression(statement->assignment.right, &code->scope, info);
			ParseExpression(statement->assignment.left,  &code->scope, info);

			if (!statement->assignment.left->is_referential_value)
			{
				Error(info, statement->assignment.left->span, "Expression is not referential.\n");
			}

			if (!AreTypesCompatible(statement->assignment.left->type, statement->assignment.right->type))
			{
				Error(info, statement->assignment.token->location, "Left and right types are incompatible.\n");
			}
		}
		else if (statement->kind == AST_STATEMENT_BRANCH_BLOCK)
		{
			for (Ast_Branch* branch = statement->branch_block.branches; branch < statement->branch_block.branches.End(); branch++)
			{
				ParseExpression(branch->condition, &code->scope, info);
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
		else if (statement->kind == AST_STATEMENT_CLAIM)
		{
			ParseExpression(statement->claim.expression, &code->scope, info);
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

				if (!AreTypesCompatible(statement->ret.expression->type, function->return_type))
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

static Type* GetTypeFromParams(Array<Ast_VariableDeclaration> params, Parse_Info* info)
{
	if (params.count == 1)
	{
		return params[0].type;
	}

	Type* first;
	if (params.count == 0)
	{
		first = &empty_tuple;
	}
	else
	{
		first = params[0].type;
	}

	for (u32 i = 0; i < first->tuple_extensions.count; i++)
	{
		Type* tuple = first->tuple_extensions[i];
		bool fail = false;

		for (u32 j = 0; j < tuple->tuple.count; j++)
		{
			if (tuple->tuple[j] != params[j].type)
			{
				fail = true;
				break;
			}
		}

		if (!fail)
		{
			return tuple;
		}
	}

	Type* tuple = info->stack.Allocate<Type>();
	Type** tuple_members = info->stack.Allocate<Type*>(params.count);
	ZeroMemory(tuple);
	tuple->kind = TYPE_BASETYPE_TUPLE;

	for (u32 i = 0; i < params.count; i++)
	{
		tuple_members[i] = params[i].type;
	}

	tuple->tuple = Array(tuple_members, params.count);
	first->tuple_extensions.Add(tuple);
	return tuple;
}

static Type* GetTypeFromTupleExpression(Array<Ast_Expression*> expressions, Parse_Info* info)
{
	if (expressions.count == 1)
	{
		return expressions[0]->type;
	}

	Type* first = expressions[0]->type;

	for (u32 i = 0; i < first->tuple_extensions.count; i++)
	{
		Type* tuple = first->tuple_extensions[i];
		bool fail = false;

		for (u32 j = 0; j < tuple->tuple.count; j++)
		{
			if (tuple->tuple[j] != expressions[j]->type)
			{
				fail = true;
				break;
			}
		}

		if (!fail)
		{
			return tuple;
		}
	}

	Type* tuple = info->stack.Allocate<Type>();
	Type** tuple_members = info->stack.Allocate<Type*>(expressions.count);
	ZeroMemory(tuple);
	tuple->kind = TYPE_BASETYPE_TUPLE;

	for (u32 i = 0; i < expressions.count; i++)
	{
		tuple_members[i] = expressions[i]->type;
	}

	tuple->tuple = Array(tuple_members, expressions.count);
	first->tuple_extensions.Add(tuple);
	return tuple;
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

	Type* param_type = GetTypeFromParams(function->parameters.ToArray(), info);

	for (u32 i = 0; i < param_type->function_extensions.count; i++)
	{
		if (param_type->function_extensions[i]->function.output == function->return_type)
		{
			function->type = param_type->function_extensions[i];
			break;
		}
	}

	if (!function->type)
	{
		Type* func_type = info->stack.Allocate<Type>();
		ZeroMemory(func_type);
		func_type->kind = TYPE_BASETYPE_FUNCTION;
		func_type->function.input = param_type;
		func_type->function.output = function->return_type;
		param_type->function_extensions.Add(func_type);
		function->type = func_type;
	}
}

void SemanticParse(Parse_Info* info)
{
	Ast_Root* root = info->ast_root;
	root->scope.parent = null;
	ParseScope(&root->scope, info);

	for (u32 i = 0; i < root->scope.functions.count; i++)
	{
		Ast_Function* func = &root->scope.functions[i];

		if (Compare(func->name->info.string, "Test"))
		{
			Interpreter* interpreter = CreateInterpreter(info);
			u64 data_size = 0;

			if (func->return_type)
			{
				data_size = func->return_type->size;
			}

			char data[data_size];
			ZeroMemory(data, data_size);
			Interpret(func, data, interpreter, info);
		}
	}
}


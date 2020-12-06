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

static void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info);
static void ScanScope(Ast_Scope* scope, Parse_Info* info);
static void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Parse_Info* info);
static void ScanFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info);
static Type* GetTypeFromTupleExpression(Ast_Expression_Tuple* tuple, Parse_Info* info);
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

static u64 CalculateStackFrameSize(Ast_Function* function);
static u64 CalculateStackFrameSize(Ast_Code* code, u64 offset);

static u64 CalculateStackFrameSize(Ast_Function* function)
{
	return CalculateStackFrameSize(&function->code, 0);
}

// @Note: This doesn't calculate the minimum memory needed to represent the stackframe which would be ideal for producing optimized binaries.
//        Another function needs to created for that.
static u64 CalculateStackFrameSize(Ast_Code* code, u64 offset)
{
	u64 initial_offset = offset;

	for (u32 i = 0; i < code->scope.variables.count; i++)
	{
		Ast_VariableDeclaration* variable = code->scope.variables[i];
		variable->offset = offset;
		offset += variable->type->size;
		// Print("Variable %:\n\tsize = %\n\toffset = %\n", variable->name, variable->type->size, variable->offset);
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

StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter)
{
	StackFrame frame;
	ZeroMemory(&frame);

	u64 size = CalculateStackFrameSize(function);

	if (!interpreter->block || size > GetFreeSpace(interpreter->block))
	{
		interpreter->block = CreateMemoryBlock(size, interpreter->block);
	}

	frame.data = interpreter->block->head;
	frame.function = function;
	interpreter->block->head += size;

	ZeroMemory(frame.data, size);
	return frame;
}

static Interpreter* CreateInterpreter(Parse_Info* info)
{
	Interpreter* interpreter = info->stack.Allocate<Interpreter>();
	ZeroMemory(interpreter);
	interpreter->block = CreateMemoryBlock(0x10000);
	return interpreter;
}

static void InitSpecifiers(Type* type, Parse_Info* info)
{
	type->specifiers = info->stack.Allocate<Type>(3);
	ZeroMemory(type->specifiers, 3);

	type->specifiers[0].kind = TYPE_SPECIFIER_POINTER;
	type->specifiers[0].subtype = type;
	type->specifiers[0].size = 8; // @FixMe

	type->specifiers[1].kind = TYPE_SPECIFIER_OPTIONAL;
	type->specifiers[1].subtype = type;
	type->specifiers[1].size = type->size + 1;

	type->specifiers[2].kind = TYPE_SPECIFIER_DYNAMIC_ARRAY;
	type->specifiers[2].subtype = type;
	type->specifiers[2].size = 16; // @FixMe
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
			Ast_Type* type = &basetype.tuple[i];
			type->type = GetType(type, scope, info);
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
		Type* output_type = &empty_tuple;

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
					ScanExpression(specifier->size_expression, scope, info);
					Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL); // @RemoveMe @Todo: Need to be removed.
					u64 length = specifier->size_expression->GetLiteral()->token->info.integer.value;
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
		for (u32 i = 0; i < scope->variables.count; i++)
		{
			Ast_VariableDeclaration* variable = scope->variables[i];
			if (CompareStrings(token->info.string, variable->name->info.string))
			{
				return variable;
			}
		}

		scope = scope->parent;
	}

	return null;
}

static Ast_Function* GetFunction(Token* token, Type* input_type, Ast_Scope* scope)
{
	while (scope)
	{
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
		{
			if (input_type == function->type->function.input && CompareStrings(token->info.string, function->name->info.string))
			{
				return function;
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

u32 GetTypePrecedence(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_PRIMITIVE:
			switch (type->primitive)
			{
				case TOKEN_UINT8:   return 1;
				case TOKEN_UINT16:  return 2;
				case TOKEN_UINT32:  return 3;
				case TOKEN_UINT64:  return 4;

				case TOKEN_INT8:    return 5;
				case TOKEN_INT16:   return 6;
				case TOKEN_INT32:   return 7;
				case TOKEN_INT64:   return 8;

				case TOKEN_FLOAT16: return 9;
				case TOKEN_FLOAT32: return 10;
				case TOKEN_FLOAT64: return 11;

				default:
					Assert();
			}

		case TYPE_BASETYPE_ENUM:
		case TYPE_SPECIFIER_POINTER:
			return 12;

		case TYPE_BASETYPE_TUPLE:
		case TYPE_BASETYPE_FUNCTION:
		case TYPE_BASETYPE_STRUCT:
		case TYPE_SPECIFIER_OPTIONAL:
		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
		case TYPE_SPECIFIER_FIXED_ARRAY:
			// return 13;
			Assert();
	}

	Assert();
	Unreachable();
}

Type* GetDominantType(Type* a, Type* b)
{
	return GetTypePrecedence(a) >= GetTypePrecedence(b) ? a : b;
}

static void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL:
		{
			Ast_Expression_Terminal* terminal = expression->GetTerminal();
			if (Ast_VariableDeclaration* variable; terminal->token->kind == TOKEN_IDENTIFIER && (variable = GetVariable(terminal->token, scope)))
			{
				expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				expression->GetVariable()->variable = variable;
				expression->type = variable->type;
				expression->is_referential_value = true;
				expression->is_pure = variable->is_pure;
				expression->can_constantly_evaluate = variable->can_constantly_evaluate;
			}
			else if (Type* type = GetBaseType(terminal->token, scope))
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
				Error(info, terminal->token->location, "Unknown variable '%'\n", terminal->token);
			}
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = expression->GetLiteral();
			if (literal->token->kind == TOKEN_INTEGER_LITERAL)
			{
				if (literal->token->info.integer.is_unsigned)
				{
					switch (literal->token->info.integer.explicit_bytes)
					{
						case 0: literal->type = &primitive_uint64; break;
						case 1: literal->type = &primitive_uint8;  break;
						case 2: literal->type = &primitive_uint16; break;
						case 4: literal->type = &primitive_uint32; break;
						case 8: literal->type = &primitive_uint64; break;
						default: Unreachable();
					}
				}
				else
				{
					switch (literal->token->info.integer.explicit_bytes)
					{
						case 0: literal->type = &primitive_int64; break;
						case 1: literal->type = &primitive_int8;  break;
						case 2: literal->type = &primitive_int16; break;
						case 4: literal->type = &primitive_int32; break;
						case 8: literal->type = &primitive_int64; break;
						default: Unreachable();
					}
				}
			}
			else if (literal->token->kind == TOKEN_FLOAT_LITERAL)
			{
				if (literal->token->info.floating_point.explicit_bytes == 0)
				{
					literal->type = &primitive_float32; // @Fixme?
				}
				else if (literal->token->info.floating_point.explicit_bytes == 2)
				{
					literal->type = &primitive_float16;
				}
				else if (literal->token->info.floating_point.explicit_bytes == 4)
				{
					literal->type = &primitive_float32;
				}
				else if (literal->token->info.floating_point.explicit_bytes == 8)
				{
					literal->type = &primitive_float64;
				}
				else Unreachable();
			}
			else if (literal->token->kind == TOKEN_TRUE || literal->token->kind == TOKEN_FALSE)
			{
				literal->type = &primitive_bool;
			}
			else if (literal->token->kind == TOKEN_NULL)
			{
				literal->type = GetPointer(&empty_tuple, info);
			}
			else if (literal->token->kind == TOKEN_STRING_LITERAL)
			{
				u64 length = literal->token->info.span.Length();
				bool found = false;

				for (u32 i = 0; i < primitive_uint8.fixed_arrays.count; i++)
				{
					if (primitive_uint8.fixed_arrays[i]->length == length)
					{
						literal->type = primitive_uint8.fixed_arrays[i];
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
					literal->type = type;
				}
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = expression->GetTuple();
			tuple->can_constantly_evaluate = true;
			tuple->is_pure = true;
			tuple->is_referential_value = true;

			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				ScanExpression(element, scope, info);

				if (!element->can_constantly_evaluate)
				{
					tuple->can_constantly_evaluate = false;
				}

				if (!element->is_pure)
				{
					tuple->is_pure = false;
				}

				if (!element->is_referential_value)
				{
					tuple->is_referential_value = false;
				}
			}

			tuple->type = GetTypeFromTupleExpression(tuple, info);
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->type = GetPointer(unary->subexpression->type, info);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!unary->subexpression->is_referential_value)
			{
				Error(info, unary->span, "Cannot get address of a non-referential value.\n");
			}
		} break;

		case AST_EXPRESSION_UNARY_VALUE_OF:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = true;

			if (unary->subexpression->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(info, unary->span, "Cannot dereference type: %\n", unary->subexpression->type);
			}

			unary->type = unary->subexpression->type->subtype;
		} break;

		case AST_EXPRESSION_UNARY_BINARY_NOT:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->type = unary->subexpression->type;
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsInteger(unary->subexpression->type))
			{
				Error(info, unary->subexpression->span, "Type % is not an integer.\n", unary->subexpression->type);
			}

		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->type = unary->subexpression->type;
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsNumerical(unary->subexpression->type))
			{
				Error(info, unary->subexpression->span, "Type % is not a numerical type.\n", unary->subexpression->type);
			}
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->type = unary->subexpression->type;
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsNumerical(unary->subexpression->type))
			{
				Error(info, unary->subexpression->span, "Type % is not a numerical type.\n", unary->subexpression->type);
			}
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = expression->GetUnary();
			ScanExpression(unary->subexpression, scope, info);
			unary->type = &primitive_bool;
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsNumerical(unary->subexpression->type))
			{
				Error(info, unary->subexpression->span, "Type % is not a numerical type.\n", unary->subexpression->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			binary->is_referential_value = binary->left->is_referential_value || binary->left->type->kind == TYPE_SPECIFIER_POINTER;

			// @Todo: Handle inferred function calls.

			Type* type = binary->left->type;

			if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY || type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
			{
				if (binary->right->kind == AST_EXPRESSION_TERMINAL)
				{
					Ast_Expression_Terminal* terminal = binary->right->GetTerminal();

					if (terminal->token->kind != TOKEN_IDENTIFIER)
					{
						Error(info, binary->span, "Invalid dot expression on array.\n");
					}

					if (CompareStrings(terminal->token->info.string, "length"))
					{
						binary->type = GetPrimitiveType(TOKEN_UINT);
					}
					else
					{
						Error(info, binary->span, "Invalid array member: %\n", terminal->token);
					}
				}
				else
				{
					Error(info, binary->span, "Invalid dot expression on array.\n");
				}
			}
			else
			{
				Ast_Expression_Terminal* terminal = binary->right->GetTerminal();
				while (type->kind == TYPE_SPECIFIER_POINTER) type = type->subtype;

				if (type->kind == TYPE_BASETYPE_STRUCT)
				{
					Assert(binary->right->kind == AST_EXPRESSION_TERMINAL && terminal->token->kind == TOKEN_IDENTIFIER);
					Ast_Struct* ast_struct = type->structure;
					Ast_Struct_Member* member = FindStructMember(ast_struct, terminal->token->info.string);

					if (!member)
					{
						Error(info, binary->span, "Struct % does not have a member named %\n", ast_struct->name, terminal->token);
					}

					binary->type = member->type.type;
					terminal->kind = AST_EXPRESSION_TERMINAL_STRUCT_MEMBER;
					terminal->GetStructMember()->member = member;
					terminal->type = member->type.type;
				}
				else
				{
					Error(info, binary->span, "Cannot dot into type: %\n", binary->left->type);
				}
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->type = &primitive_bool;
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!AreTypesCompatible(binary->left->type, binary->right->type) && !(IsNumerical(binary->left->type) && IsNumerical(binary->right->type)))
			{
				Error(info, binary->span, "% and % are incompatible types.\n", binary->left->type, binary->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->type = &primitive_bool;
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsNumerical(binary->left->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->left->type);
			}

			if (!IsNumerical(binary->right->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsNumerical(binary->left->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->left->type);
			}

			if (!IsNumerical(binary->right->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsNumerical(binary->left->type) || IsFloat(binary->left->type))
			{
				Error(info, binary->span, "Cannot use bitwise AND with type: %\n", binary->left->type);
			}

			if (!IsNumerical(binary->right->type) || IsFloat(binary->right->type))
			{
				Error(info, binary->span, "Cannot use bitwise AND with type: %\n", binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsInteger(binary->left->type))
			{
				Error(info, binary->span, "% is not an integer.\n", binary->left->type);
			}

			if (!IsInteger(binary->right->type))
			{
				Error(info, binary->span, "% is not an integer.\n", binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = expression->GetBinary();
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsNumerical(binary->left->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->left->type);
			}

			if (!IsNumerical(binary->right->type))
			{
				Error(info, binary->span, "% is not a numerical type.\n", binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = expression->GetTernary();
			ScanExpression(ternary->left,   scope, info);
			ScanExpression(ternary->middle, scope, info);
			ScanExpression(ternary->right,  scope, info);
			ternary->type = ternary->left->type;
			ternary->can_constantly_evaluate = ternary->left->can_constantly_evaluate && ternary->middle->can_constantly_evaluate && ternary->right->can_constantly_evaluate;
			ternary->is_pure = ternary->left->is_pure && ternary->middle->is_pure && ternary->right->is_pure;
			ternary->is_referential_value = false;

			if (ternary->left->type != ternary->right->type)
			{
				Error(info, ternary->span, "Type mismatch between % and %.\n", ternary->left->type, ternary->right->type);
			}
		} break;

		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = expression->GetCall();
			if (call->parameters) // @RemoveMe?
			{
				ScanExpression(call->parameters, scope, info);
			}

			if (call->function->kind == AST_EXPRESSION_TERMINAL && call->function->GetTerminal()->token->kind == TOKEN_IDENTIFIER)
			{
				// @Note: call->function could still be a variable and not a function!

				Ast_Expression_Terminal* terminal = call->function->GetTerminal();

				if (Ast_Function* function = GetFunction(terminal->token, call->parameters->type, scope); function)
				{
					call->function->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
					call->function->GetFunction()->function = function;
					call->type = function->return_type;
					if (!call->type) call->type = &empty_tuple;
				}
				else if (Ast_VariableDeclaration* variable = GetVariable(call->function->GetTerminal()->token, scope); variable)
				{
					call->function->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
					call->function->GetVariable()->variable = variable;
					call->type = variable->type->function.output;
					if (!call->type) call->type = &empty_tuple;
					call->is_referential_value = true;
					call->is_pure = variable->is_pure;
					call->can_constantly_evaluate = variable->can_constantly_evaluate;

					if (variable->type->kind != TYPE_BASETYPE_FUNCTION)
					{
						Error(info, call->function->GetVariable()->token->location, "Variable % with type % cannot be called like a function.\n", variable->name, variable->type);
					}
				}
				else
				{
					Error(info, call->function->GetTerminal()->token->location, "Function % not found.\n", call->function->GetTerminal()->token);
				}
			}
			else
			{
				ScanExpression(call->function, scope, info);
			}
		} break;

		case AST_EXPRESSION_LAMBDA:
		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = expression->GetSubscript();
			ScanExpression(subscript->array,  scope, info);
			ScanExpression(subscript->index, scope, info);

			if (subscript->array->type->kind != TYPE_SPECIFIER_FIXED_ARRAY   &&
				subscript->array->type->kind != TYPE_SPECIFIER_DYNAMIC_ARRAY &&
				subscript->array->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(info, subscript->array->span, "Expression with type % is not a valid array.\n", subscript->array->span);
			}

			if (!IsInteger(subscript->index->type))
			{
				Error(info, subscript->index->span, "Subscript index must be an integer, not: %\n", subscript->index->type);
			}

			subscript->type = subscript->array->type->subtype;
			subscript->can_constantly_evaluate = subscript->array->can_constantly_evaluate && subscript->index->can_constantly_evaluate;
			subscript->is_pure = subscript->array->is_pure && subscript->index->is_pure;
			subscript->is_referential_value = true;
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

static void CalculateStructSize(Ast_Struct* s);
static void CalculateTupleSize(Type* tuple);

static void CalculateTupleSize(Type* tuple)
{
	if (tuple->size) return;

	u64 size = 0;

	Assert(tuple->tuple.count);
	for (u32 i = 0; i < tuple->tuple.count; i++)
	{
		Type* type = tuple->tuple[i];
		if (type->kind == TYPE_BASETYPE_STRUCT)
		{
			CalculateStructSize(type->structure);
		}
		else if (type->kind == TYPE_BASETYPE_TUPLE)
		{
			CalculateTupleSize(type);
		}

		size += type->size;
	}

	tuple->size = size;
}

static void CalculateStructSize(Ast_Struct* s)
{
	if (s->type.size) return;

	u64 size = 0;

	for (Ast_Struct_Member* member = s->members; member < s->members.End(); member++)
	{
		Type* type = member->type.type;
		if (type->kind == TYPE_BASETYPE_STRUCT)
		{
			CalculateStructSize(type->structure);
		}
		else if (type->kind == TYPE_BASETYPE_TUPLE)
		{
			CalculateTupleSize(type);
		}

		member->offset = size;
		size += type->size;
	}

	s->type.size = size;
}

static void ScanScope(Ast_Scope* scope, Parse_Info* info)
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
			Type* type = GetType(&member->type, scope, info);
			member->type.type = type;

			if (!type)
			{
				Error(info, member->type.basetype.token->location, "Unknown type '%'\n", member->type.basetype.token);
			}
		}
	}

	for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
	{
		CheckForCircularDependencies(s, info);
		CalculateStructSize(s);
		// Print("Struct % has size = %\n", s->name, s->type.size);
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ScanFunction(f, scope, info);
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ScanCode(&f->code, scope, f, info);
	}

}

static void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Parse_Info* info)
{
	code->scope.parent = scope;
	ScanScope(&code->scope, info);

	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				for (Ast_Branch* branch = statement->branch_block.branches; branch < statement->branch_block.branches.End(); branch++)
				{
					if (branch->condition)
					{
						ScanExpression(branch->condition, &code->scope, info);
					}

					branch->code.is_inside_loop = code->is_inside_loop;

					if (branch->token->kind == TOKEN_WHILE || branch->token->kind == TOKEN_FOR)
					{
						branch->code.is_inside_loop = true;
					}

					branch->code.has_deferrer_that_returns = code->has_deferrer_that_returns;

					ScanCode(&branch->code, &code->scope, function, info);

					if (branch->code.does_return)
					{
						code->does_return = true;
					}
				}
			} break;

			case AST_STATEMENT_DEFER:
			{
				code->defers.Add(&statement->defer);
				statement->defer.code.is_inside_loop = code->is_inside_loop;
				ScanCode(&statement->defer.code, &code->scope, function, info);

				if (statement->defer.code.does_return)
				{
					code->has_deferrer_that_returns = true;
					code->does_return = true;
				}
			} break;

			case AST_STATEMENT_CLAIM:
			{
				ScanExpression(statement->claim.expression, &code->scope, info);
			} break;

			case AST_STATEMENT_ALIAS:
			{
				Assert();
			} break;

			case AST_STATEMENT_INCREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				ScanExpression(inc->expression, &code->scope, info);

				if (!inc->expression->is_referential_value)
				{
					Error(info, inc->expression->span, "Expression is not a referential value.\n");
				}

				if (!IsInteger(inc->expression->type) && inc->expression->type->kind != TYPE_SPECIFIER_POINTER)
				{
					Error(info, inc->expression->span, "Expression is an integer or pointer.\n");
				}
			} break;

			case AST_STATEMENT_DECREMENT:
			{
				Ast_Decrement* dec = &statement->decrement;
				ScanExpression(dec->expression, &code->scope, info);

				if (!dec->expression->is_referential_value)
				{
					Error(info, dec->expression->span, "Expression is not a referential value.\n");
				}

				if (!IsInteger(dec->expression->type) && dec->expression->type->kind != TYPE_SPECIFIER_POINTER)
				{
					Error(info, dec->expression->span, "Expression is an integer or pointer.\n");
				}
			} break;

			case AST_STATEMENT_RETURN:
			{
				code->does_return = true;

				if (code->has_deferrer_that_returns)
				{
					Error(info, statement->ret.token->location, "A defer in this scope already has a return statement. This isn't allowed.\n");
				}

				if (statement->ret.expression)
				{
					ScanExpression(statement->ret.expression, &code->scope, info);

					if (!function->return_type)
					{
						Error(info, statement->ret.token->location, "Unexpected return value for function that doesn't return anything.\n");
					}

					if (!AreTypesCompatible(statement->ret.expression->type, function->return_type))
					{
						Error(info, statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
					}
				}
				else if (function->does_return)
				{
					Error(info, statement->ret.token->location, "Expected return value with type: %\n", function->return_type);
				}
			} break;

			case AST_STATEMENT_BREAK:
			{
				if (!code->is_inside_loop)
				{
					Error(info, statement->brk.token->location, "break isn't inside of a loop.\n");
				}
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				ScanExpression(statement->expression, &code->scope, info);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
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
					ScanExpression(variable->assignment, &code->scope, info);

					if (!variable->assignment->type)
					{
						Error(info, variable->assignment->span, "Expression does not have a type.\n");
					}
				}

				if (variable->explicit_type != null)
				{
					variable->type = GetType(variable->explicit_type, &code->scope, info);
					variable->explicit_type->type = variable->type;

					if (!variable->type)
					{
						Error(info, variable->name->location, "Unknown type %\n", variable->explicit_type->basetype.token);
					}

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
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				ScanExpression(statement->assignment.right, &code->scope, info);
				ScanExpression(statement->assignment.left,  &code->scope, info);

				if (!statement->assignment.left->is_referential_value)
				{
					Error(info, statement->assignment.left->span, "Expression is not referential.\n");
				}

				if (!AreTypesCompatible(statement->assignment.left->type, statement->assignment.right->type))
				{
					Error(info, statement->assignment.token->location, "Left and right types are incompatible.\n");
				}

				if (statement->kind != AST_STATEMENT_ASSIGNMENT)
				{
					if (!IsNumerical(statement->assignment.left->type))
					{
						Error(info, statement->assignment.left->span, "Expression is not numerical.\n");
					}

					if (!IsNumerical(statement->assignment.right->type))
					{
						Error(info, statement->assignment.right->span, "Expression is not numerical.\n");
					}
				}
			} break;
		}
	}
}

static Type* GetTypeFromParams(Array<Ast_VariableDeclaration> params, Parse_Info* info)
{
	if (params.count == 0)
	{
		return &empty_tuple;
	}
	else if (params.count == 1)
	{
		return params[0].type;
	}

	Type* first = params[0].type;

	for (u32 i = 0; i < first->tuple_extensions.count; i++)
	{
		Type* tuple = first->tuple_extensions[i];
		bool fail = false;

		if (tuple->tuple.count != params.count) continue;

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
		Assert(params[i].type->size); // @Note @Bug: I don't think this will work with recursive tuples or maybe even structs?
		tuple->size += params[i].type->size;
	}

	tuple->tuple = Array(tuple_members, params.count);
	first->tuple_extensions.Add(tuple);
	return tuple;
}

static Type* GetTypeFromTupleExpression(Ast_Expression_Tuple* tuple, Parse_Info* info)
{
	if (tuple->elements.count == 0)
	{
		return &empty_tuple;
	}

	if (tuple->elements.count == 1)
	{
		return tuple->elements[0]->type;
	}

	Type* first = tuple->elements[0]->type;

	for (u32 i = 0; i < first->tuple_extensions.count; i++)
	{
		Type* type = first->tuple_extensions[i];
		bool fail = false;

		if (type->tuple.count != tuple->elements.count) continue;

		for (u32 j = 0; j < type->tuple.count; j++)
		{
			if (type->tuple[j] != tuple->elements[j]->type)
			{
				fail = true;
				break;
			}
		}

		if (!fail)
		{
			return type;
		}
	}

	Type* type = info->stack.Allocate<Type>();
	Type** tuple_members = info->stack.Allocate<Type*>(tuple->elements.count);
	ZeroMemory(type);
	type->kind = TYPE_BASETYPE_TUPLE;

	for (u32 i = 0; i < tuple->elements.count; i++)
	{
		tuple_members[i] = tuple->elements[i]->type;
		type->size += tuple->elements[i]->type->size;
	}

	type->tuple = Array(tuple_members, tuple->elements.count);
	first->tuple_extensions.Add(type);
	return type;
}

static void ScanFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info)
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
	else
	{
		function->return_type = &empty_tuple;
	}

	Type* param_type = GetTypeFromParams(function->parameters, info);

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
	ScanScope(&root->scope, info);

	for (u32 i = 0; i < root->scope.functions.count; i++)
	{
		Ast_Function* function = &root->scope.functions[i];

		if (Compare(function->name->info.string, "Test") && !function->parameters.count)
		{
			Interpreter* interpreter = CreateInterpreter(info);
			u64 data_size = 0;

			if (function->return_type)
			{
				data_size = function->return_type->size;
			}

			char data[data_size];
			ZeroMemory(data, data_size);
			Interpret(function, null, data, interpreter);
		}
	}
}


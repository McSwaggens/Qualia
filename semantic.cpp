#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"
#include "util.h"

template<typename ...Args>
[[noreturn]]
static void Error(Parse_Info* info, SourceLocation where, String format, Args&&... message_args);

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

static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Parse_Info* info);

MemoryBlock* CreateMemoryBlock(u64 min_size, MemoryBlock* prev)
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

	MemoryBlock* block = (MemoryBlock*)AllocateVirtualPage(size);
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

u64 CalculateStackFrameSize(Ast_Function* function)
{
	return CalculateStackFrameSize(&function->code, 0);
}

// @Note: This doesn't calculate the minimum memory needed to represent the stackframe which would be ideal for producing optimized binaries.
//        Another function needs to created for that.
u64 CalculateStackFrameSize(Ast_Code* code, u64 offset)
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
	else return GetPrimitiveTypeFromTokenKind(token->kind);
	return null;
}

static Type* GetBaseType(Ast_BaseType basetype, Ast_Scope* scope, Parse_Info* info)
{
	if (basetype.kind == AST_BASETYPE_PRIMITIVE)
	{
		return GetPrimitiveTypeFromTokenKind(basetype.token->kind);
	}
	else if (basetype.kind == AST_BASETYPE_TUPLE)
	{
		u32 tuple_count = basetype.tuple.count;

		if (!tuple_count)
		{
			Error(info, basetype.token->location, "Empty tuple is an invalid type.\n");
		}

		Type* types[tuple_count];

		for (u32 i = 0; i < tuple_count; i++)
		{
			types[i] = GetType(&basetype.tuple[i], scope, info);
		}

		return GetTuple(Array(types, tuple_count));
	}
	else if (basetype.kind == AST_BASETYPE_FUNCTION)
	{
		Type* input = GetType(basetype.function.input, scope, info);
		Type* output = GetType(basetype.function.output, scope, info);

		return GetFunctionType(input, output);
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
				type = GetPointer(type);
			}
			else if (specifier->kind == AST_SPECIFIER_OPTIONAL)
			{
				type = GetOptional(type);
			}
			else if (specifier->kind == AST_SPECIFIER_ARRAY)
			{
				if (specifier->size_expression == null)
				{
					type = GetDynamicArray(type);
				}
				else
				{
					ScanExpression(specifier->size_expression, scope, info);
					Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL); // @RemoveMe @Todo: Need to be removed.

					if (!specifier->size_expression->can_constantly_evaluate)
					{
						Error(info, specifier->size_expression->span, "Fixed array size must be constantly evaluatable.\n");
					}

					if (!IsInteger(specifier->size_expression->type))
					{
						Error(info, specifier->size_expression->span, "Fixed array size must be an integer.\n");
					}

					Ast_Expression_Literal* literal = (Ast_Expression_Literal*)specifier->size_expression;
					s64 length = literal->token->info.integer.value;

					if (length <= 0) // Shouldn't we just enforce uint?
					{
						Error(info, specifier->size_expression->span, "Fixed array size must be larger than 0.\n");
					}

					type = GetFixedArray(type, length);
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

Intrinsic_Function intrinsic_functions_array[1];
Array<Intrinsic_Function> intrinsic_functions = Array<Intrinsic_Function>(intrinsic_functions_array, 1);

void Intrinsic_SystemCall(u64* input, u64* output)
{
	*output = SystemCall(input[0], input[1], input[2], input[3], input[4], input[5], input[6]);
}

void InitIntrinsicFunctions(Parse_Info* info)
{
	intrinsic_functions[0] = {
		"SystemCall",

		GetTuple({
			&type_uint64, // rax
			&type_uint64, // rdi
			&type_uint64, // rsi
			&type_uint64, // rdx
			&type_uint64, // r10
			&type_uint64, // r8
			&type_uint64, // r9
		}),

		&type_uint64,

		(Intrinsic_Function_Type)&Intrinsic_SystemCall
	};
}

static Intrinsic_Function* GetIntrinsicFunction(String name, Type* input_type)
{
	for (Intrinsic_Function* intrinsic = intrinsic_functions; intrinsic < intrinsic_functions.End(); intrinsic++)
	{
		if (CompareStrings(name, intrinsic->name) && IsConvertableTo(input_type, intrinsic->input))
		{
			return intrinsic;
		}
	}

	return null;
}

static Ast_Function* GetFunction(String name, Type* input_type, Ast_Scope* scope)
{
	while (scope)
	{
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
		{
			if (CompareStrings(name, function->name) && IsConvertableTo(input_type, function->type->function.input))
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

void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Parse_Info* info)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL:
		{
			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)expression;

			if (Ast_VariableDeclaration* variable; terminal->token->kind == TOKEN_IDENTIFIER && (variable = GetVariable(terminal->token, scope)))
			{
				expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				((Ast_Expression_Variable*)expression)->variable = variable;
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
					Ast_Expression_Struct* struct_terminal = (Ast_Expression_Struct*)expression;
					struct_terminal->structure = type->structure;
				}
				else if (type->kind == TYPE_BASETYPE_ENUM)
				{
					expression->kind = AST_EXPRESSION_TERMINAL_ENUM;
					Ast_Expression_Enum* enum_terminal = (Ast_Expression_Enum*)expression;
					enum_terminal->enumeration = type->enumeration;
				}
				else if (IsPrimitive(type))
				{
					expression->kind = AST_EXPRESSION_TERMINAL_PRIMITIVE;
				}
			}
			else
			{
				Error(info, terminal->token->location, "Unknown variable '%'\n", terminal->token);
			}
		} break;

		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Ast_Expression_Fixed_Array* fixed_array = (Ast_Expression_Fixed_Array*)expression;
			fixed_array->is_pure = true;
			fixed_array->can_constantly_evaluate = true;
			fixed_array->is_referential_value = false;
			Type* subtype;

			for (u32 i = 0; i < fixed_array->elements.count; i++)
			{
				Ast_Expression* element = fixed_array->elements[i];
				ScanExpression(element, scope, info);

				if (!element->is_pure)
				{
					fixed_array->is_pure = false;
				}

				if (!element->can_constantly_evaluate)
				{
					fixed_array->can_constantly_evaluate = false;
				}

				if (i)
				{
					if (!IsConvertableTo(element->type, subtype))
					{
						Error(info, element->span, "Expected expression of type %, not: %\n", subtype, element->type); // @FixMe
					}
				}
				else
				{
					subtype = element->type;
					fixed_array->type = GetFixedArray(subtype, fixed_array->elements.count);
				}
			}
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			if (literal->token->kind == TOKEN_INTEGER_LITERAL)
			{
				if (literal->token->info.integer.is_unsigned)
				{
					switch (literal->token->info.integer.explicit_bytes)
					{
						case 0: literal->type = &type_uint64; break;
						case 1: literal->type = &type_uint8;  break;
						case 2: literal->type = &type_uint16; break;
						case 4: literal->type = &type_uint32; break;
						case 8: literal->type = &type_uint64; break;
						default: Unreachable();
					}
				}
				else
				{
					switch (literal->token->info.integer.explicit_bytes)
					{
						case 0: literal->type = &type_int64; break;
						case 1: literal->type = &type_int8;  break;
						case 2: literal->type = &type_int16; break;
						case 4: literal->type = &type_int32; break;
						case 8: literal->type = &type_int64; break;
						default: Unreachable();
					}
				}

				literal->value_uint64 = literal->token->info.integer.value;
			}
			else if (literal->token->kind == TOKEN_FLOAT_LITERAL)
			{
				if (literal->token->info.floating_point.explicit_bytes == 0)
				{
					literal->type = &type_float32; // @Fixme?
					literal->value_float32 = literal->token->info.floating_point.value;
				}
				else if (literal->token->info.floating_point.explicit_bytes == 2)
				{
					literal->type = &type_float16;
					Assert();
				}
				else if (literal->token->info.floating_point.explicit_bytes == 4)
				{
					literal->type = &type_float32;
					literal->value_float32 = literal->token->info.floating_point.value;
				}
				else if (literal->token->info.floating_point.explicit_bytes == 8)
				{
					literal->type = &type_float64;
					literal->value_float64 = literal->token->info.floating_point.value;
				}
				else Unreachable();
			}
			else if (literal->token->kind == TOKEN_TRUE || literal->token->kind == TOKEN_FALSE)
			{
				literal->type = &type_bool;
				literal->value_bool = literal->token->kind == TOKEN_TRUE;
			}
			else if (literal->token->kind == TOKEN_NULL)
			{
				literal->type = GetPointer(&type_byte);
			}
			else if (literal->token->kind == TOKEN_STRING_LITERAL)
			{
				literal->is_referential_value = true;
				literal->type = GetFixedArray(&type_uint8, literal->token->info.string.length);
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;
			tuple->is_referential_value = false;

			tuple->can_constantly_evaluate = true;
			tuple->is_pure = true;

			tuple->recursive_count = 0;

			Type* types[tuple->elements.count];

			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				ScanExpression(element, scope, info);
				types[i] = element->type;

				if (element->kind == AST_EXPRESSION_TUPLE)
				{
					tuple->recursive_count += ((Ast_Expression_Tuple*)element)->recursive_count;
				}
				else
				{
					tuple->recursive_count++;
				}

				if (element->type == &empty_tuple && (element->kind != AST_EXPRESSION_TUPLE || tuple->elements.count > 1))
				{
					Error(info, element->span, "Tuple elements aren't allowed to be of type %.\n", element->type);
				}

				if (!element->can_constantly_evaluate)
				{
					tuple->can_constantly_evaluate = false;
				}

				if (!element->is_pure)
				{
					tuple->is_pure = false;
				}
			}

			tuple->type = GetTuple(Array(types, tuple->elements.count));
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, info);
			unary->type = GetPointer(unary->subexpression->type);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!unary->subexpression->is_referential_value)
			{
				Error(info, unary->span, "Cannot get address of a non-referential value.\n");
			}
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
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

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
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
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
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
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
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
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, info);
			unary->type = &type_bool;
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
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);

			// @Todo: Handle inferred function calls.

			Type* type = binary->left->type;

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM)
			{
				Ast_Enum* ast_enum = ((Ast_Expression_Enum*)binary->left)->enumeration;

				if (binary->right->kind != AST_EXPRESSION_TERMINAL)
				{
					Error(info, binary->right->span, "Expected enum member name.\n");
				}

				binary->right->kind = AST_EXPRESSION_TERMINAL_ENUM_MEMBER;

				Ast_Expression_Enum_Member* member_terminal = (Ast_Expression_Enum_Member*)binary->right;
				String name = member_terminal->token->info.string;

				Assert(member_terminal->token->kind == TOKEN_IDENTIFIER);

				Ast_Enum_Member* member = FindEnumMember(ast_enum, name);

				if (!member)
				{
					Error(info, binary->right->span, "Enum % does not contain a member called \"%\".", ast_enum->name, name);
				}

				member_terminal->member = member;
				member_terminal->type = type;
				member_terminal->can_constantly_evaluate = true;
				member_terminal->is_pure = true;
				member_terminal->is_referential_value = false;

				binary->type = type;
				binary->can_constantly_evaluate = true;
				binary->is_pure = true;
				binary->is_referential_value = false;
			}
			else if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY || type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
			{
				if (binary->right->kind == AST_EXPRESSION_TERMINAL)
				{
					Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;

					if (terminal->token->kind != TOKEN_IDENTIFIER)
					{
						Error(info, binary->span, "Invalid dot expression on array.\n");
					}

					if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY && (
						CompareStrings(terminal->token->info.string, "data") ||
						CompareStrings(terminal->token->info.string, "begin")))
						// @Todo: array.end
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_DATA;
						binary->type = GetPointer(binary->left->type->subtype);
						binary->is_referential_value = binary->left->is_referential_value;
					}
					else if (CompareStrings(terminal->token->info.string, "length") ||
						CompareStrings(terminal->token->info.string, "count"))
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_LENGTH;
						binary->type = &type_uint64;
						binary->is_referential_value = binary->left->is_referential_value && binary->left->type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY;
						binary->can_constantly_evaluate = binary->left->type->kind == TYPE_SPECIFIER_FIXED_ARRAY;
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
				Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;
				binary->is_referential_value = binary->left->is_referential_value || binary->left->type->kind == TYPE_SPECIFIER_POINTER;

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
					((Ast_Expression_Struct_Member*)terminal)->member = member;
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
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->type = &type_bool;
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (binary->left->type == &empty_tuple || binary->right->type == &empty_tuple)
			{
				Error(info, binary->span, "Cannot compare % and %.\n", binary->left->type, binary->right->type);
			}

			if (!IsNumerical(binary->left->type) || !IsNumerical(binary->right->type))
			{
				Error(info, binary->span, "% and % are incompatible types.\n", binary->left->type, binary->right->type);
			}

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (!IsConvertableTo(binary->right->type, dominant) || !IsConvertableTo(binary->left->type, dominant))
			{
				Error(info, binary->span, "% and % are incompatible types.\n", binary->left->type, binary->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->type = &type_bool;
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

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (!IsConvertableTo(binary->left->type, dominant) || !IsConvertableTo(binary->left->type, dominant))
			{
				Error(info, binary->span, "Incompatible types % and %\n", binary->left->type, binary->right->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (IsPointer(binary->left->type) && IsConvertableTo(binary->right->type, &type_int64))
			{
				binary->type = binary->left->type;
			}
			else if (IsPointer(binary->left->type) && IsPointer(binary->right->type))
			{
				if (binary->left->type != binary->right->type && !IsBytePointer(binary->left->type) && !IsBytePointer(binary->right->type))
				{
					Error(info, binary->span,
						"Cannot perform binary operation '%' on two pointers of different types: '%' and '%'.\n",
						binary->op, binary->left->type, binary->right->type);
				}

				if (binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
				{
					binary->type = &type_int64;
				}
				else
				{
					binary->type = binary->left->type;
				}
			}
			else
			{
				binary->type = GetDominantType(binary->left->type, binary->right->type);

				if (!IsNumerical(binary->left->type))
				{
					Error(info, binary->span, "% is not a numerical type.\n", binary->left->type);
				}

				if (!IsNumerical(binary->right->type))
				{
					Error(info, binary->span, "% is not a numerical type.\n", binary->right->type);
				}

				if (!IsConvertableTo(binary->left->type, binary->type) || !IsConvertableTo(binary->right->type, binary->type))
				{
					Error(info, binary->span, "Types % and % are incompatible.\n", binary->left->type, binary->right->type);
				}
			}
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsNumerical(binary->left->type) || IsFloat(binary->left->type))
			{
				Error(info, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);
			}

			if (!IsNumerical(binary->right->type) || IsFloat(binary->right->type))
			{
				Error(info, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
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
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, info);
			ScanExpression(binary->right, scope, info);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsConvertableTo(binary->left->type, &type_bool))
			{
				Error(info, binary->span, "% cannot be converted to bool.\n", binary->left->type);
			}

			if (!IsConvertableTo(binary->right->type, &type_bool))
			{
				Error(info, binary->span, "% cannot be converted to bool.\n", binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			ScanExpression(ternary->left,   scope, info);
			ScanExpression(ternary->middle, scope, info);
			ScanExpression(ternary->right,  scope, info);
			ternary->can_constantly_evaluate = ternary->left->can_constantly_evaluate && ternary->middle->can_constantly_evaluate && ternary->right->can_constantly_evaluate;
			ternary->is_pure = ternary->left->is_pure && ternary->middle->is_pure && ternary->right->is_pure;
			ternary->is_referential_value = false;

			ternary->type = GetDominantType(ternary->left->type, ternary->right->type);

			if (!IsConvertableTo(ternary->left->type, ternary->type) ||
				!IsConvertableTo(ternary->right->type, ternary->type))
			{
				Error(info, ternary->left->span, "Type mismatch between % and %\n", ternary->left->type, ternary->right->type);
			}

			if (!IsConvertableTo(ternary->middle->type, &type_bool))
			{
				Error(info, ternary->middle->span, "Type % not convertable to bool\n", ternary->middle->type);
			}
		} break;

		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;

			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)call->function;
			ScanExpression(call->parameters, scope, info);

			if (call->function->kind == AST_EXPRESSION_TERMINAL && terminal->token->kind == TOKEN_IDENTIFIER)
			{
				if (Ast_Function* function = GetFunction(terminal->token->info.string, call->parameters->type, scope); function)
				{
					Ast_Expression_Function* function_expression = (Ast_Expression_Function*)call->function;
					call->function->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
					function_expression->function = function;
					call->type = function->return_type;
					call->can_constantly_evaluate = false; // @FixMe: Need to figure out how to prove this.
					call->is_pure = false;                 // @FixMe: This too.
					call->is_referential_value = false;
				}
				else if (Intrinsic_Function* intrinsic_function = GetIntrinsicFunction(terminal->token->info.string, call->parameters->type); intrinsic_function)
				{
					Ast_Expression_Intrinsic_Function* intrinsic_function_expression = (Ast_Expression_Intrinsic_Function*)call->function;
					call->function->kind = AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION;
					intrinsic_function_expression->intrinsic_function = intrinsic_function;
					call->type = intrinsic_function->output;
					call->can_constantly_evaluate = false; // @FixMe: Might not always be false.
					call->is_pure = false;                 // @FixMe: This too.
					call->is_referential_value = false;
				}
				else if (Ast_VariableDeclaration* variable = GetVariable(terminal->token, scope); variable)
				{
					Ast_Expression_Variable* variable_expression = (Ast_Expression_Variable*)call->function;
					call->function->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
					variable_expression->variable = variable;

					if (!IsFunctionPointer(variable->type))
					{
						Error(info, variable_expression->token->location, "Variable % with type % cannot be called like a function.\n", variable->name, variable->type);
					}

					Type* function_type = variable->type->subtype;

					call->type = function_type->function.output;
					call->is_pure = false;
					call->can_constantly_evaluate = false;
					call->is_referential_value = false;

					if (!IsConvertableTo(call->parameters->type, variable->type->function.input))
					{
						Error(info, call->function->span, "Function of type % called with invalid arguments: %.\n", call->function->type, call->parameters->type);
					}
				}
				else
				{
					Error(info, terminal->token->location, "Function % not found.\n", terminal->token);
				}
			}
			else
			{
				ScanExpression(call->function, scope, info);

				if (!IsFunctionPointer(call->function->type))
				{
					Error(info, call->function->span, "Expression of type % cannot be called like a function.\n", call->function->type);
				}

				Type* function_type = call->function->type->subtype;
				call->can_constantly_evaluate = false;
				call->is_pure = false;
				call->type = function_type->function.output;

				if (!IsConvertableTo(call->parameters->type, function_type->function.input))
				{
					Error(info, call->function->span, "Function of type % called with invalid arguments: %.\n", call->function->type, call->parameters->type);
				}
			}
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			ScanExpression(subscript->array, scope, info);
			ScanExpression(subscript->index, scope, info);

			if (subscript->array->type->kind != TYPE_SPECIFIER_FIXED_ARRAY   &&
				subscript->array->type->kind != TYPE_SPECIFIER_DYNAMIC_ARRAY &&
				subscript->array->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(info, subscript->array->span, "Expression with type % is not a valid array.\n", subscript->array->span);
			}

			if (!IsConvertableTo(subscript->index->type, &type_int64))
			{
				Error(info, subscript->index->span, "Subscript index must be an integer, not: %\n", subscript->index->type);
			}

			subscript->type = subscript->array->type->subtype;
			subscript->can_constantly_evaluate = subscript->array->can_constantly_evaluate && subscript->index->can_constantly_evaluate;
			subscript->is_pure = subscript->array->is_pure && subscript->index->is_pure;
			subscript->is_referential_value = true;

			if (subscript->array->type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
			{
				subscript->is_referential_value = subscript->array->is_referential_value;
			}
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			ScanExpression(as->expression, scope, info);
			as->type = GetType(&as->ast_type, scope, info);

			as->can_constantly_evaluate = as->expression->can_constantly_evaluate;
			as->is_pure = as->expression->is_pure;
			as->is_referential_value = false;

			if (!IsConvertableTo(as->expression->type, as->type))
			{
				Error(info, as->span, "Type % is not convertable to %\n", as->expression->type, as->type);
			}
		} break;

		default:
			Assert();
			Unreachable();
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

void ScanScope(Ast_Scope* scope, Parse_Info* info)
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
		e->underlying_type = &type_int64;
		e->type.size = e->underlying_type->size;

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

	for (Ast_Enum* e = scope->enums; e < scope->enums.End(); e++)
	{
		for (Ast_Enum_Member* member = e->members; member < e->members.End(); member++)
		{
			ScanExpression(member->expression, scope, info);
		}
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ScanFunction(f, scope, info);

		for (Ast_Function* other = scope->functions; other < f; other++)
		{
			if (f->type == other->type && CompareStrings(f->name, other->name))
			{
				Error(info, f->name_token->location, "Function '%' with type % already exists.\n", f->name, f->type);
			}
		}
	}

	for (Ast_Function* f = scope->functions; f < scope->functions.End(); f++)
	{
		ScanCode(&f->code, scope, f, info);

		if (f->return_type != &empty_tuple)
		{
			if (!f->code.does_return)
			{
				Error(info, f->name_token->location, "Function does not return a value.\n");
			}
		}
	}

}

bool IsAssignable(Ast_Expression* expression)
{
	if (expression->is_referential_value) return true;
	if (expression->type->kind != TYPE_BASETYPE_TUPLE) return false;
	if (expression->type == &empty_tuple) return false;

	Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

	for (u32 i = 0; i < tuple->elements.count; i++)
	{
		Ast_Expression* element = tuple->elements[i];
		if (!IsAssignable(element)) return false;
	}

	return true;
}

void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Parse_Info* info)
{
	code->scope.parent = scope;
	ScanScope(&code->scope, info);

	for (Ast_Statement* statement = code->statements.Begin(); statement < code->statements.End(); statement++)
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

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				ScanExpression(inc->expression, &code->scope, info);

				if (!inc->expression->is_referential_value)
				{
					Error(info, inc->expression->span, "Expression is not a referential value.\n");
				}

				if (!IsInteger(inc->expression->type) && !IsFloat(inc->expression->type) && inc->expression->type->kind != TYPE_SPECIFIER_POINTER)
				{
					Error(info, inc->expression->span, "Expression is an integer, float or pointer.\n");
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

					if (!IsConvertableTo(function->return_type, statement->ret.expression->type))
					{
						Error(info, statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
					}
				}
				else if (function->does_have_return_type_appendage)
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

				if (variable->explicit_type)
				{
					variable->type = GetType(variable->explicit_type, &code->scope, info);
					variable->explicit_type->type = variable->type;

					if (!variable->type)
					{
						Error(info, variable->name->location, "Unknown type %\n", variable->explicit_type->basetype.token);
					}

					if (variable->assignment)
					{
						if (!IsConvertableTo(variable->assignment->type, variable->type))
						{
							Error(info, variable->name->location, "Cannot assign expression with type % to variable with type %\n", variable->assignment->type, variable->type);
						}
					}
				}
				else
				{
					variable->type = variable->assignment->type;
				}

				if (variable->type == &empty_tuple)
				{
					Error(info, variable->assignment->span, "Cannot declare variable with type %\n", variable->type);
				}

				code->scope.variables.Add(variable);
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				ScanExpression(statement->assignment.right, &code->scope, info);
				ScanExpression(statement->assignment.left,  &code->scope, info);

				if (!IsAssignable(statement->assignment.left))
				{
					Error(info, statement->assignment.left->span, "Expression is not assignable.\n");
				}

				if (!IsConvertableTo(statement->assignment.right->type, statement->assignment.left->type))
				{
					Error(info, statement->assignment.token->location, "Left and right types are incompatible.\n");
				}

			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;

				ScanExpression(assignment->right, &code->scope, info);
				ScanExpression(assignment->left,  &code->scope, info);

				if (!assignment->left->is_referential_value)
				{
					Error(info, assignment->left->span, "Expression is not assignable.\n");
				}

				if (!IsInteger(assignment->left->type) &&
					!IsFloat(assignment->left->type) &&
					assignment->left->type->kind != TYPE_SPECIFIER_POINTER)
				{
					Error(info, assignment->left->span, "Arithmetic assignment type must be to an integer, float or pointer, not: %\n", assignment->left->type);
				}

				// ptr += int
				// ptr -= int

				// int += int
				// int -= int
				// int *= int
				// int /= int

				// float += float
				// float -= float
				// float *= float
				// float /= float

				if (assignment->left->type->kind == TYPE_SPECIFIER_POINTER)
				{
					if (statement->kind != AST_STATEMENT_ASSIGNMENT_ADD && statement->kind != AST_STATEMENT_ASSIGNMENT_SUBTRACT)
					{
						Error(info, assignment->left->span, "Pointer arithmetic assignment only allows += and -=.\n");
					}

					if (!IsConvertableTo(assignment->right->type, &type_int64))
					{
						Error(info, assignment->right->span, "Expression type must be an integer.\n");
					}
				}
				else if (!IsConvertableTo(assignment->right->type, assignment->left->type))
				{
					Error(info, assignment->right->span, "Expression is not numerical.\n");
				}
			} break;
		}
	}
}

static Type* GetTypeFromParams(Array<Ast_VariableDeclaration> params, Parse_Info* info)
{
	Type* types[params.count];
	for (u32 i = 0; i < params.count; i++)
	{
		types[i] = params[i].type;
	}

	return GetTuple(Array(types, params.count));
}

void ScanFunction(Ast_Function* function, Ast_Scope* scope, Parse_Info* info)
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

		if (Compare(function->name, "Test") && !function->parameters.count)
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


#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"
#include "util.h"

template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, SourceLocation where, String format, Args&&... message_args);

template<typename ...Args>
[[noreturn]]
static void Error(Ast_Module* module, Span<Token> where, String format, Args&&... message_args)
{
	u32 margin = 2;
	u32 start = where[0].location.line;
	SourceLocation loc_begin = where.Begin()->location;
	SourceLocation loc_end = (where.End()-1)->location; // @Bug: What if begin = end. Is this invalid input? IDK
	u32 number_of_lines = loc_end.line - loc_begin.line + margin + 1;

	Print("%:%:%: error: ", module->file_path, (loc_begin.line+1), (loc_begin.offset+1));
	Print(format, message_args...);

	// @Todo: Coloring/Highlighting

	for (u32 line = start; line < start + number_of_lines && line < module->lines.count; line++)
	{
		Print("%\n", String(module->lines[line], module->lines[line].Length()));
	}

	Fail();
}

static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Ast_Module* module);

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

	MemoryBlock* block = (MemoryBlock*)GetPage(size);
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
	if (token->kind == TOKEN_IDENTIFIER_FORMAL)
	{
		while (scope)
		{
			for (Ast_Struct* s = scope->structs; s < scope->structs.End(); s++)
			{
				if (CompareStrings(token->string, s->name->string))
				{
					return &s->type;
				}
			}

			for (Ast_Enum* e = scope->enums; e < scope->enums.End(); e++)
			{
				if (CompareStrings(token->string, e->name->string))
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

static Type* GetBaseType(Ast_BaseType basetype, Ast_Scope* scope, Ast_Module* module)
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
			Error(module, basetype.token->location, "Empty tuple is an invalid type.\n");
		}

		Type* types[tuple_count];

		for (u32 i = 0; i < tuple_count; i++)
		{
			types[i] = GetType(&basetype.tuple[i], scope, module);
		}

		return GetTuple(Array(types, tuple_count));
	}
	else if (basetype.kind == AST_BASETYPE_FUNCTION)
	{
		Type* input = GetType(basetype.function.input, scope, module);
		Type* output = GetType(basetype.function.output, scope, module);

		return GetFunctionType(input, output);
	}

	Assert(basetype.kind == AST_BASETYPE_USERTYPE);
	return GetBaseType(basetype.token, scope);
}

static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Ast_Module* module)
{
	if (!ast_type)
	{
		return &empty_tuple;
	}

	Type* type = GetBaseType(ast_type->basetype, scope, module);

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
					ScanExpression(specifier->size_expression, scope, module);
					Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL); // @RemoveMe @Todo: Need to be removed.

					if (!specifier->size_expression->can_constantly_evaluate)
					{
						Error(module, specifier->size_expression->span, "Fixed array size must be constantly evaluatable.\n");
					}

					if (!IsInteger(specifier->size_expression->type))
					{
						Error(module, specifier->size_expression->span, "Fixed array size must be an integer.\n");
					}

					Ast_Expression_Literal* literal = (Ast_Expression_Literal*)specifier->size_expression;
					s64 length = literal->token->integer.value;

					if (length <= 0) // Shouldn't we just enforce uint?
					{
						Error(module, specifier->size_expression->span, "Fixed array size must be larger than 0.\n");
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
			if (CompareStrings(token->string, variable->name->string))
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

Intrinsic_Function MakeIntrinsicFunction(String name, Type* input, Type* output, Intrinsic_Function_Type interpreter_call)
{
	Intrinsic_Function intrinsic;
	ZeroMemory(&intrinsic);
	intrinsic.name = name;
	intrinsic.function = interpreter_call;
	intrinsic.input = input;
	intrinsic.output = output;
	intrinsic.type = GetFunctionType(input, output);

	return intrinsic;
}

void InitIntrinsicFunctions()
{
	intrinsic_functions[0] = MakeIntrinsicFunction(
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
	);
}

static Intrinsic_Function* FindIntrinsicFunction(String name, Type* input_type)
{
	for (Intrinsic_Function* intrinsic = intrinsic_functions; intrinsic < intrinsic_functions.End(); intrinsic++)
	{
		if (CompareStrings(name, intrinsic->name) && CanImplicitCast(input_type, intrinsic->input))
		{
			return intrinsic;
		}
	}

	return null;
}

static Ast_Function* FindFunction(String name, Type* input_type, Ast_Scope* scope)
{
	while (scope)
	{
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
		{
			if (CompareStrings(name, function->name) && CanImplicitCast(input_type, function->type->input))
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
		if (CompareStrings(name, member->name->string))
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
		if (CompareStrings(name, member->name->string))
		{
			return member;
		}
	}

	return null;
}

static Type* GetArithmeticType(Type* type)
{
	Assert(IsConvertableToArithmeticType(type));
	if (IsArithmetic(type)) return type;
	if (type->kind == TYPE_BASETYPE_BOOL) return &type_int8;
	if (type->kind == TYPE_BASETYPE_ENUM) return type->enumeration->underlying_type;
	Assert();
	Unreachable();
}

static Ast_Expression* ImplicitCast(Ast_Expression* expression, Type* type, Ast_Module* module)
{
	if (expression->type == type) return expression;

	Ast_Expression_Implicit_Cast* cast = StackAllocate<Ast_Expression_Implicit_Cast>(&module->stack);
	cast->kind = AST_EXPRESSION_IMPLICIT_CAST;
	cast->span = expression->span;
	cast->subexpression = expression;
	cast->type = type;
	cast->is_referential_value = false;
	cast->is_pure = expression->is_pure;
	cast->can_constantly_evaluate = expression->can_constantly_evaluate;

	return cast;
}

static Ast_Expression* TryImplicitCastToArithmeticType(Ast_Expression* expression, Ast_Module* module)
{
	if (IsArithmetic(expression->type)) return expression;
	if (IsConvertableToArithmeticType(expression->type)) return ImplicitCast(expression, GetArithmeticType(expression->type), module);
	return expression;
}

void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Ast_Module* module)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_NAME:
		{
			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)expression;

			if (terminal->token->kind == TOKEN_IDENTIFIER_CASUAL)
			{
				Ast_VariableDeclaration* variable = GetVariable(terminal->token, scope);

				if (!variable)
				{
					Error(module, terminal->token->location, "Variable with name '%' does not exist.\n", terminal->token);
				}

				Ast_Expression_Variable* variable_expression = (Ast_Expression_Variable*)expression;
				variable_expression->variable = variable;

				expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				expression->type = variable->type;
				expression->is_referential_value = true;
				expression->is_pure = variable->is_pure;
				expression->can_constantly_evaluate = variable->can_constantly_evaluate;
			}
			else if (terminal->token->kind == TOKEN_IDENTIFIER_FORMAL)
			{
				Type* type = GetBaseType(terminal->token, scope);

				if (!type)
				{
					Error(module, terminal->token->location, "User type with name '%' does not exist.\n", terminal->token);
				}

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
				Assert();
				Error(module, terminal->token->location, "Unknown variable '%'\n", terminal->token);
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
				ScanExpression(element, scope, module);

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
					if (!CanImplicitCast(element->type, subtype))
					{
						Error(module, element->span, "Expected expression of type %, not: %\n", subtype, element->type); // @FixMe
					}

					fixed_array->elements[i] = ImplicitCast(element, subtype, module);
				}
				else
				{
					subtype = element->type;
					fixed_array->type = GetFixedArray(subtype, fixed_array->elements.count);
				}
			}
		} break;

		case AST_EXPRESSION_DYNAMIC_ARRAY:
		{
			Ast_Expression_Dynamic_Array* array = (Ast_Expression_Dynamic_Array*)expression;
			array->is_pure = array->left->is_pure && array->right->is_pure;
			array->can_constantly_evaluate = array->left->can_constantly_evaluate && array->right->can_constantly_evaluate;
			array->is_referential_value = false;

			ScanExpression(array->left, scope, module);
			ScanExpression(array->right, scope, module);

			if (!IsPointer(array->left->type))
			{
				Error(module, array->span, "Begin expression must be a pointer type, not: %\n", array->left->type);
			}

			if (IsPointer(array->right->type))
			{
				if (!CanImplicitCast(array->right->type, array->left->type))
				{
					Error(module, array->right->span, "Array begin and end types are incompatible: [%..%]\n", array->left->type, array->right->type);
				}

				array->right = ImplicitCast(array->right, array->left->type, module);
			}
			else if (CanImplicitCast(array->right->type, &type_int64))
			{
				array->right = ImplicitCast(array->right, &type_int64, module);
			}
			else
			{
				Error(module, array->right->span, "Array end must be a % or an int, not: %\n", array->left->type, array->right->type);
			}

			array->type = GetDynamicArray(array->left->type->subtype);
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;
			literal->is_referential_value = false;
			literal->can_constantly_evaluate = true;
			literal->is_pure = true;

			if (literal->token->kind == TOKEN_INTEGER_LITERAL)
			{
				if (literal->token->integer.is_unsigned)
				{
					switch (literal->token->integer.explicit_bytes)
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
					switch (literal->token->integer.explicit_bytes)
					{
						case 0: literal->type = &type_int64; break;
						case 1: literal->type = &type_int8;  break;
						case 2: literal->type = &type_int16; break;
						case 4: literal->type = &type_int32; break;
						case 8: literal->type = &type_int64; break;
						default: Unreachable();
					}
				}

				literal->value_uint64 = literal->token->integer.value;
			}
			else if (literal->token->kind == TOKEN_FLOAT_LITERAL)
			{
				if (literal->token->floating_point.explicit_bytes == 0)
				{
					literal->type = &type_float32; // @Fixme?
					literal->value_float32 = literal->token->floating_point.value;
				}
				else if (literal->token->floating_point.explicit_bytes == 2)
				{
					literal->type = &type_float16;
					Assert();
				}
				else if (literal->token->floating_point.explicit_bytes == 4)
				{
					literal->type = &type_float32;
					literal->value_float32 = literal->token->floating_point.value;
				}
				else if (literal->token->floating_point.explicit_bytes == 8)
				{
					literal->type = &type_float64;
					literal->value_float64 = literal->token->floating_point.value;
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
				literal->value_pointer = null;
			}
			else if (literal->token->kind == TOKEN_STRING_LITERAL)
			{
				literal->is_referential_value = true;
				literal->type = GetFixedArray(&type_uint8, literal->token->string.length);
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;
			tuple->is_referential_value = false;
			tuple->can_constantly_evaluate = true;
			tuple->is_pure = true;

			tuple->recursive_count = tuple->elements.count;

			Type* types[tuple->elements.count];

			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				ScanExpression(element, scope, module);
				types[i] = element->type;

				if (element->kind == AST_EXPRESSION_TUPLE)
				{
					tuple->recursive_count += ((Ast_Expression_Tuple*)element)->recursive_count-1;
				}

				if (element->type == &empty_tuple && (element->kind != AST_EXPRESSION_TUPLE || tuple->elements.count > 1)) // @FixMe: ((), int) ????
				{
					Error(module, element->span, "Tuple elements aren't allowed to be of type %.\n", element->type);
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
			ScanExpression(unary->subexpression, scope, module);
			unary->type = GetPointer(unary->subexpression->type);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!unary->subexpression->is_referential_value)
			{
				Error(module, unary->span, "Cannot take address of a non-referential value.\n");
			}
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = true;

			if (!IsPointer(unary->subexpression->type))
			{
				Error(module, unary->span, "Cannot take reference of type: %\n", unary->subexpression->type);
			}

			unary->type = unary->subexpression->type->subtype;
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsInteger(unary->subexpression->type) && !IsPointer(unary->subexpression->type))
			{
				Error(module, unary->subexpression->span, "Type % is not an integer or pointer.\n", unary->subexpression->type);
			}

			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsFloat(unary->subexpression->type) && !IsSignedInteger(unary->subexpression->type) && !IsPointer(unary->subexpression->type))
			{
				if (!CanImplicitCast(unary->subexpression->type, &type_int64))
				{
					Error(module, unary->subexpression->span, "Type % is not an arithmetic type.\n", unary->subexpression->type);
				}

				unary->subexpression = ImplicitCast(unary->subexpression, &type_int64, module);
			}

			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!IsSignedInteger(unary->subexpression->type) && !IsFloat(unary->subexpression->type))
			{
				Error(module, unary->subexpression->span, "Unary plus can only be applied to a signed integer or float.\n", unary->subexpression->type);
			}

			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->can_constantly_evaluate = unary->subexpression->can_constantly_evaluate;
			unary->is_pure = unary->subexpression->is_pure;
			unary->is_referential_value = false;

			if (!CanImplicitCast(unary->subexpression->type, &type_bool))
			{
				Error(module, unary->subexpression->span, "Type % cannot be casted to bool.\n", unary->subexpression->type);
			}

			unary->subexpression = ImplicitCast(unary->subexpression, &type_bool, module);
			unary->type = &type_bool;
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);

			Type* type = binary->left->type;

			if (type == &empty_tuple)
			{
				Error(module, binary->left->span, "Cannot dot into empty tuple.\n");
			}

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM)
			{
				Ast_Enum* ast_enum = ((Ast_Expression_Enum*)binary->left)->enumeration;

				if (binary->right->kind != AST_EXPRESSION_TERMINAL_NAME)
				{
					Error(module, binary->right->span, "Expected enum member name.\n");
				}

				binary->right->kind = AST_EXPRESSION_TERMINAL_ENUM_MEMBER;

				Ast_Expression_Enum_Member* member_terminal = (Ast_Expression_Enum_Member*)binary->right;
				String name = member_terminal->token->string;

				Ast_Enum_Member* member = FindEnumMember(ast_enum, name);

				if (!member)
				{
					Error(module, binary->right->span, "Enum % does not contain a member called \"%\".", ast_enum->name, name);
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
				if (binary->right->kind == AST_EXPRESSION_TERMINAL_NAME)
				{
					Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;

					if (terminal->token->kind != TOKEN_IDENTIFIER_CASUAL)
					{
						Error(module, binary->span, "Invalid dot expression on array.\n");
					}

					if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY && (
						CompareStrings(terminal->token->string, "data") ||
						CompareStrings(terminal->token->string, "begin")))
						// @Todo: array.end
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_DATA;
						binary->type = GetPointer(binary->left->type->subtype);
						binary->is_referential_value = binary->left->is_referential_value;
					}
					else if (CompareStrings(terminal->token->string, "length") ||
						CompareStrings(terminal->token->string, "count"))
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_LENGTH;
						binary->type = &type_uint64;
						binary->is_referential_value = binary->left->is_referential_value && binary->left->type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY;
						binary->can_constantly_evaluate = binary->left->type->kind == TYPE_SPECIFIER_FIXED_ARRAY;
					}
					else
					{
						Error(module, binary->span, "Invalid array member: %\n", terminal->token);
					}
				}
				else
				{
					Error(module, binary->span, "Invalid dot expression on array.\n");
				}
			}
			else
			{
				Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;
				binary->is_referential_value = binary->left->is_referential_value || binary->left->type->kind == TYPE_SPECIFIER_POINTER;

				while (type->kind == TYPE_SPECIFIER_POINTER) type = type->subtype;

				if (type->kind == TYPE_BASETYPE_STRUCT)
				{
					if (binary->right->kind != AST_EXPRESSION_TERMINAL_NAME)
					{
						Error(module, binary->span, "Expected struct member name, not: %\n", terminal->token);
					}

					Ast_Struct* ast_struct = type->structure;
					Ast_Struct_Member* member = FindStructMember(ast_struct, terminal->token->string);

					if (!member)
					{
						Error(module, binary->span, "Struct % does not have a member named %\n", ast_struct->name, terminal->token);
					}

					binary->type = member->type.type;
					terminal->kind = AST_EXPRESSION_TERMINAL_STRUCT_MEMBER;
					((Ast_Expression_Struct_Member*)terminal)->member = member;
					terminal->type = member->type.type;
				}
				else
				{
					Error(module, binary->span, "Cannot dot into type: %\n", binary->left->type);
				}
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (IsArithmetic(binary->left->type) || IsArithmetic(binary->right->type))
			{
				binary->left = TryImplicitCastToArithmeticType(binary->left, module);
				binary->right = TryImplicitCastToArithmeticType(binary->right, module);
			}

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (!CanImplicitCast(binary->left->type, dominant) || !CanImplicitCast(binary->right->type, dominant))
			{
				Error(module, binary->span, "% and % are incompatible types.\n", binary->left->type, binary->right->type);
			}

			binary->left = ImplicitCast(binary->left, dominant, module);
			binary->right = ImplicitCast(binary->right, dominant, module);

			binary->type = &type_bool;
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			Type* dominant = GetDominantType(GetArithmeticType(binary->left->type), GetArithmeticType(binary->right->type));

			if (!IsArithmetic(dominant))
			{
				Error(module, binary->span, "% is not an arithmetic type.\n", dominant);
			}

			if (!CanImplicitCast(binary->left->type, dominant) || !CanImplicitCast(binary->left->type, dominant))
			{
				Error(module, binary->span, "Incompatible types % and %\n", binary->left->type, binary->right->type);
			}

			binary->left = ImplicitCast(binary->left, dominant, module);
			binary->right = ImplicitCast(binary->right, dominant, module);

			binary->type = &type_bool;
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			binary->left = TryImplicitCastToArithmeticType(binary->left, module);
			binary->right = TryImplicitCastToArithmeticType(binary->right, module);

			if (IsPointer(binary->left->type) && IsPointer(binary->right->type))
			{
				if (binary->left->type != binary->right->type && !IsBytePointer(binary->left->type) && !IsBytePointer(binary->right->type))
				{
					Error(module, binary->span,
						"Cannot perform binary expression '%' on two pointers of different types: '%' and '%'.\n",
						binary->op, binary->left->type, binary->right->type);
				}

				binary->type = binary->left->type;

				if (binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
				{
					binary->type = &type_int64;
				}
			}
			else if (IsPointer(binary->left->type) && CanImplicitCast(binary->right->type, &type_int64))
			{
				binary->right = ImplicitCast(binary->right, &type_int64, module);
				binary->type = binary->left->type;
			}
			else
			{
				Type* dominant = GetDominantType(binary->left->type, binary->right->type);

				if (!IsArithmetic(binary->left->type))
				{
					Error(module, binary->span, "% is not an arithmetic type.\n", binary->left->type);
				}

				if (!IsArithmetic(binary->right->type))
				{
					Error(module, binary->span, "% is not an arithmetic type.\n", binary->right->type);
				}

				if (!CanImplicitCast(binary->left->type, dominant) || !CanImplicitCast(binary->right->type, dominant))
				{
					Error(module, binary->span, "Types % and % are incompatible.\n", binary->left->type, binary->right->type);
				}

				binary->left = ImplicitCast(binary->left, dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = dominant;
			}
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!IsInteger(binary->left->type) && !IsPointer(binary->left->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);
			}

			if (!IsInteger(binary->right->type) && !IsPointer(binary->right->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);
			}

			binary->type = GetDominantType(binary->left->type, binary->right->type);
		} break;

		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
			binary->is_pure = binary->left->is_pure && binary->right->is_pure;
			binary->is_referential_value = false;

			if (!CanImplicitCast(binary->left->type, &type_bool))
			{
				Error(module, binary->span, "% cannot be converted to bool.\n", binary->left->type);
			}

			if (!CanImplicitCast(binary->right->type, &type_bool))
			{
				Error(module, binary->span, "% cannot be converted to bool.\n", binary->right->type);
			}

			binary->left = ImplicitCast(binary->left, &type_bool, module);
			binary->right = ImplicitCast(binary->right, &type_bool, module);

			binary->type = &type_bool;
		} break;

		// case AST_EXPRESSION_BINARY_RANGE:
		// {
		// 	Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
		// 	ScanExpression(binary->left,  scope, module);
		// 	ScanExpression(binary->right, scope, module);
		// 	binary->can_constantly_evaluate = binary->left->can_constantly_evaluate && binary->right->can_constantly_evaluate;
		// 	binary->is_pure = binary->left->is_pure && binary->right->is_pure;

		// 	if (IsPointer(binary->left->type))
		// 	{
		// 		if (!CanImplicitCast(binary->left->type, binary->right->type))
		// 		{
		// 			binary->right = ImplicitCast(binary->right, binary->left->type, module);
		// 		}
		// 		else if (!IsInteger(binary->left->type))
		// 		{
		// 			Error(module, binary->right->span, "Invalid range end/offset, expected either % or an integer, not: %\n", binary->left->type, binary->right->type);
		// 		}

		// 		binary->type = GetDynamicArray(binary->left->type);
		// 	}
		// 	// else if (IsInteger(binary->left->type) || IsFloat(binary->left->type))
		// 	// {
		// 	// 	Type* dominant = GetDominantType(binary->left->type, binary->right->type);
		// 	// 	binary->left  = ImplicitCast(binary->left,  dominant, module);
		// 	// 	binary->right = ImplicitCast(binary->right, dominant, module);

		// 	// }
		// 	else
		// 	{
		// 		Error(module, binary->right->span, "Invalid range begin and end types: % .. %\n", binary->left->type, binary->right->type);
		// 	}

		// } break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			ScanExpression(ternary->left,   scope, module);
			ScanExpression(ternary->middle, scope, module);
			ScanExpression(ternary->right,  scope, module);
			ternary->can_constantly_evaluate = ternary->left->can_constantly_evaluate && ternary->middle->can_constantly_evaluate && ternary->right->can_constantly_evaluate;
			ternary->is_pure = ternary->left->is_pure && ternary->middle->is_pure && ternary->right->is_pure;
			ternary->is_referential_value = ternary->left->is_referential_value && ternary->right->is_referential_value && ternary->left->type == ternary->right->type;

			Type* dominant = GetDominantType(ternary->left->type, ternary->right->type);

			if (!CanImplicitCast(ternary->middle->type, &type_bool))
			{
				Error(module, ternary->middle->span, "Type % not convertable to bool\n", ternary->middle->type);
			}

			if (!CanImplicitCast(ternary->left->type, dominant) || !CanImplicitCast(ternary->right->type, dominant))
			{
				Error(module, ternary->left->span, "Type mismatch between % and %\n", ternary->left->type, ternary->right->type);
			}

			ternary->middle = ImplicitCast(ternary->middle, &type_bool, module);
			ternary->left = ImplicitCast(ternary->left, dominant, module);
			ternary->right = ImplicitCast(ternary->right, dominant, module);

			ternary->type = dominant;
		} break;

		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;

			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)call->function;
			Ast_Expression_Binary* dot = (Ast_Expression_Binary*)call->function;

			ScanExpression(call->parameters, scope, module);

			if (call->function->kind == AST_EXPRESSION_BINARY_DOT &&
				dot->right->kind == AST_EXPRESSION_TERMINAL_NAME &&
				((Ast_Expression_Terminal*)dot->right)->token->kind == TOKEN_IDENTIFIER_FORMAL)
			{
				Ast_Expression_Dot_Call* call = (Ast_Expression_Dot_Call*)expression;
				call->kind = AST_EXPRESSION_DOT_CALL;

				Ast_Expression_Function* function_expression = (Ast_Expression_Function*)dot->right;

				ScanExpression(dot->left, scope, module);

				if (dot->left->type == &empty_tuple)
				{
					Error(module, dot->left->span, "Cannot call into an empty tuple.\n");
				}

				Type* full_param_type = MergeTypeRight(dot->left->type, call->parameters->type);
				Ast_Function* function = FindFunction(function_expression->token->string, full_param_type, scope);

				if (!function)
				{
					Error(module, call->span, "No function exists called % with input type: %.\n", function_expression->token->string, full_param_type);
				}

				function_expression->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
				function_expression->function = function;
				function_expression->type = function->type;

				call->type = function->return_type;
				call->can_constantly_evaluate = false; // @FixMe: Need to figure out how to prove this.
				call->is_pure = false;                 // @FixMe: This too.
				call->is_referential_value = false;

				Assert(call->parameters->elements.count == function->parameters.count-1);

				dot->left = ImplicitCast(dot->left, function->parameters[0].type, module);

				for (u32 i = 0; i < call->parameters->elements.count; i++)
				{
					call->parameters->elements[i] = ImplicitCast(call->parameters->elements[i], function->parameters[i+1].type, module);
				}
			}
			else if (call->function->kind == AST_EXPRESSION_TERMINAL_NAME && terminal->token->kind == TOKEN_IDENTIFIER_FORMAL)
			{
				if (Ast_Function* function = FindFunction(terminal->token->string, call->parameters->type, scope); function)
				{
					Ast_Expression_Function* function_expression = (Ast_Expression_Function*)call->function;
					call->function->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
					function_expression->function = function;
					function_expression->type = function->type;

					call->type = function->return_type;
					call->can_constantly_evaluate = false; // @FixMe: Need to figure out how to prove this.
					call->is_pure = false;                 // @FixMe: This too.
					call->is_referential_value = false;

					call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, function->type->input, module);
				}
				else if (Intrinsic_Function* intrinsic_function = FindIntrinsicFunction(terminal->token->string, call->parameters->type); intrinsic_function)
				{
					Ast_Expression_Intrinsic_Function* intrinsic_function_expression = (Ast_Expression_Intrinsic_Function*)call->function;
					intrinsic_function_expression->intrinsic_function = intrinsic_function;
					intrinsic_function_expression->type = intrinsic_function->type;

					call->function->kind = AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION;
					call->type = intrinsic_function->output;
					call->can_constantly_evaluate = false; // @FixMe: Might not always be false.
					call->is_pure = false;                 // @FixMe: This too.
					call->is_referential_value = false;

					call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, intrinsic_function->input, module);
				}
				else
				{
					Error(module, terminal->token->location, "No function exists called % with input type: %.\n", terminal->token->string, call->parameters->type);
				}
			}
			else
			{
				ScanExpression(call->function, scope, module);

				if (!IsFunctionPointer(call->function->type))
				{
					Error(module, call->function->span, "Expression of type % cannot be called like a function.\n", call->function->type);
				}

				Type* function_type = call->function->type->subtype;
				call->can_constantly_evaluate = false;
				call->is_pure = false;
				call->type = function_type->output;

				if (!CanImplicitCast(call->parameters->type, function_type->input))
				{
					Error(module, call->function->span, "Function of type % called with invalid arguments: %.\n", call->function->type, call->parameters->type);
				}

				call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, function_type->input, module);
			}
		} break;

		case AST_EXPRESSION_LAMBDA:
		{
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			ScanExpression(subscript->array, scope, module);
			ScanExpression(subscript->index, scope, module);

			if (subscript->array->type->kind != TYPE_SPECIFIER_FIXED_ARRAY   &&
				subscript->array->type->kind != TYPE_SPECIFIER_DYNAMIC_ARRAY &&
				subscript->array->type->kind != TYPE_SPECIFIER_POINTER)
			{
				Error(module, subscript->array->span, "Expression with type % is not a valid array.\n", subscript->array->span);
			}

			if (!CanImplicitCast(subscript->index->type, &type_int64))
			{
				Error(module, subscript->index->span, "Subscript index must be an integer, not: %\n", subscript->index->type);
			}

			subscript->index = ImplicitCast(subscript->index, &type_int64, module);

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
			ScanExpression(as->expression, scope, module);
			as->type = GetType(&as->ast_type, scope, module);

			as->can_constantly_evaluate = as->expression->can_constantly_evaluate;
			as->is_pure = as->expression->is_pure;
			as->is_referential_value = false;

			if (!CanImplicitCast(as->expression->type, as->type))
			{
				Error(module, as->span, "Type % is not convertable to %\n", as->expression->type, as->type);
			}
		} break;

		default:
			Assert();
			Unreachable();
	}

	Assert(expression->type);
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

static void CheckForCircularDependencies(Ast_Struct* ast_struct, Ast_Module* module)
{
	GenerateClosure(ast_struct, ast_struct);

	if (ast_struct->closure.Contains(ast_struct))
	{
		Error(module, ast_struct->name->location, "The closure of struct % contains % (circularly dependent)\n", ast_struct->name, ast_struct->name);
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

void ScanScope(Ast_Scope* scope, Ast_Module* module)
{
	for (u32 i = 0; i < scope->variables.count; i++) // @RemoveMe?
	{
		Ast_VariableDeclaration* var = scope->variables[i];
		var->stack = IR_NONE;
	}

	// @Yuck: This function is *disgusting*, all of this should be implicit when we try to use a type for the first time or something... Also, what about multi-threading?
	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
	{
		ZeroMemory(&ast_struct->type);
		ast_struct->type.kind = TYPE_BASETYPE_STRUCT;
		ast_struct->type.structure = ast_struct;

		for (Ast_Struct* other = scope->structs; other < ast_struct; other++)
		{
			if (CompareStrings(ast_struct->name->string, other->name->string))
			{
				Error(module, ast_struct->name->location, "Duplicate struct called '%'\n", ast_struct->name);
			}
		}

		for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++)
		{
			if (CompareStrings(ast_struct->name->string, ast_enum->name->string))
			{
				if (ast_struct->name->location.line < ast_enum->name->location.line)
				{
					Error(module, ast_enum->name->location, "Duplicate type called '%'\n", ast_enum->name);
				}
				else
				{
					Error(module, ast_struct->name->location, "Duplicate type called '%'\n", ast_struct->name);
				}
			}
		}

		for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
		{
			for (Ast_Struct_Member* member_other = ast_struct->members; member_other < member; member_other++)
			{
				if (CompareStrings(member->name->string, member_other->name->string))
				{
					Error(module, member->name->location, "Duplicate member called '%' in struct %\n", member->name, ast_struct->name);
				}
			}
		}
	}

	for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++)
	{
		ZeroMemory(&ast_enum->type);
		ast_enum->type.kind = TYPE_BASETYPE_ENUM;
		ast_enum->type.enumeration = ast_enum;
		ast_enum->underlying_type = &type_int64;
		ast_enum->type.size = ast_enum->underlying_type->size;

		for (Ast_Enum* eo = scope->enums; eo < ast_enum; eo++)
		{
			if (CompareStrings(ast_enum->name->string, eo->name->string))
			{
				Error(module, ast_enum->name->location, "Duplicate enum called '%'\n", ast_enum->name);
			}
		}

		for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++)
		{
			for (Ast_Enum_Member* member_other = ast_enum->members; member_other < member; member_other++)
			{
				if (CompareStrings(member->name->string, member_other->name->string))
				{
					Error(module, member->name->location, "Duplicate member called '%' in enum %\n", member->name, ast_enum->name);
				}
			}
		}
	}

	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
	{
		for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
		{
			Type* type = GetType(&member->type, scope, module);
			member->type.type = type;

			if (!type)
			{
				Error(module, member->type.basetype.token->location, "Unknown type '%'\n", member->type.basetype.token);
			}
		}
	}

	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
	{
		CheckForCircularDependencies(ast_struct, module);
		CalculateStructSize(ast_struct);
		// Print("Struct % has size = %\n", ast_struct->name, ast_struct->type.size);
	}

	for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++)
	{
		for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++)
		{
			ScanExpression(member->expression, scope, module);
		}
	}

	for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
	{
		ScanFunction(function, scope, module);

		for (Ast_Function* other = scope->functions; other < function; other++)
		{
			if (function->type == other->type && CompareStrings(function->name, other->name))
			{
				Error(module, function->name_token->location, "Function '%' with type % already exists.\n", function->name, function->type);
			}
		}
	}

	for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
	{
		ScanCode(&function->code, scope, function, module);

		if (function->return_type != &empty_tuple)
		{
			if (!function->code.all_paths_return)
			{
				Error(module, function->name_token->location, "Function does not return a value.\n");
			}
		}
	}

}

bool IsAssignable(Ast_Expression* expression)
{
	if (expression->is_referential_value) return true;

	if (expression->kind == AST_EXPRESSION_TUPLE)
	{
		// @RemoveMe: You can't even declare a variable with empty_tuple anyways, remove check?
		if (expression->type == &empty_tuple) return false;

		Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

		for (u32 i = 0; i < tuple->elements.count; i++)
		{
			Ast_Expression* element = tuple->elements[i];
			if (!IsAssignable(element)) return false;
		}

		return true;
	}

	return false;
}

static bool IsLoop(Ast_Branch* branch)
{
	switch (branch->kind)
	{
		case AST_BRANCH_NAKED:
		case AST_BRANCH_IF:
			return false;

		case AST_BRANCH_WHILE:
		case AST_BRANCH_FOR_COUNT:
		case AST_BRANCH_FOR_RANGE:
		case AST_BRANCH_FOR_VERBOSE:
			return true;
	}
}

static bool DoesBranchAlwaysReturn(Ast_BranchBlock* branch_block, Ast_Branch_Index branch_index)
{
	Ast_Branch* branch = &branch_block->branches[branch_index];
	return branch_index != AST_BRANCH_INDEX_NONE
		&& (branch->code.all_paths_return || (DoesBranchAlwaysReturn(branch_block, branch->else_branch_index) && DoesBranchAlwaysReturn(branch_block, branch->then_branch_index)));
}

void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Ast_Module* module)
{
	code->scope.parent = scope;
	ScanScope(&code->scope, module);

	for (Ast_Statement* statement = code->statements.Begin(); statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* branch_block = (Ast_BranchBlock*)&statement->branch_block;

				for (u32 i = 0; i < branch_block->branches.count; i++)
				{
					Ast_Branch* branch = &branch_block->branches[i];

					branch->code.scope.parent = &code->scope; // @Hack

					switch (branch->kind)
					{
						case AST_BRANCH_NAKED:
						{
							branch->code.is_inside_loop = code->is_inside_loop;
							code->does_break = code->does_break || branch->code.does_break;
						} break;

						case AST_BRANCH_IF:
						{
							ScanExpression(branch->condition, &code->scope, module);

							if (!CanImplicitCast(branch->condition->type, &type_bool))
							{
								Error(module, branch->condition->span, "Cannot implicitly cast condition with type % to bool\n", branch->condition->type);
							}

							branch->condition = ImplicitCast(branch->condition, &type_bool, module);

							branch->code.is_inside_loop = code->is_inside_loop;
							code->does_break = code->does_break || branch->code.does_break;
						} break;

						case AST_BRANCH_WHILE:
						{
							ScanExpression(branch->condition, &code->scope, module);

							if (!CanImplicitCast(branch->condition->type, &type_bool))
							{
								Error(module, branch->condition->span, "Cannot implicitly cast condition with type % to bool\n", branch->condition->type);
							}

							branch->condition = ImplicitCast(branch->condition, &type_bool, module);

							branch->code.is_inside_loop = true;
						} break;

						case AST_BRANCH_FOR_COUNT:
						{
							ScanExpression(branch->count, &code->scope, module);

							if (!CanImplicitCast(branch->count->type, &type_int64))
							{
								Error(module, branch->count->span, "For counter type must be an int, not: %\n", branch->count->type);
							}

							branch->count = ImplicitCast(branch->count, &type_int64, module);

							branch->code.is_inside_loop = true;
						} break;

						case AST_BRANCH_FOR_RANGE:
						{
							branch->code.scope.variables.Add(branch->iterator);
							ScanExpression(branch->range, &code->scope, module);

							if (!IsDynamicArray(branch->range->type) && !IsFixedArray(branch->range->type))
							{
								Error(module, branch->range->span, "For loop cannot range over type: %\n", branch->range->type);
							}

							if (branch->filter)
							{
								ScanExpression(branch->filter, &branch->code.scope, module);

								if (!CanImplicitCast(branch->filter->type, &type_bool))
								{
									Error(module, branch->filter->span, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->filter->type);
								}

								branch->filter = ImplicitCast(branch->filter, &type_bool, module);
							}

							if (branch->stride)
							{
								ScanExpression(branch->stride, &branch->code.scope, module);

								if (!CanImplicitCast(branch->stride->type, &type_int64))
								{
									Error(module, branch->filter->span, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->filter->type);
								}

								branch->stride = ImplicitCast(branch->stride, &type_int64, module);
							}

							branch->code.is_inside_loop = true;
						} break;

						case AST_BRANCH_FOR_VERBOSE:
						{
							if (branch->variable->assignment)
							{
								ScanExpression(branch->variable->assignment, &code->scope, module);
								branch->variable->type = branch->variable->assignment->type;
							}

							if (branch->variable->explicit_type)
							{
								branch->variable->type = GetType(branch->variable->explicit_type, &code->scope, module);
							}

							if (branch->variable->explicit_type && branch->variable->assignment)
							{
								if (!CanImplicitCast(branch->variable->assignment->type, branch->variable->type))
								{
									Error(module, branch->variable->assignment->span, "Cannot assign expression with type % to variable with type %\n", branch->variable->assignment->type, branch->variable->type);
								}

								branch->variable->assignment = ImplicitCast(branch->variable->assignment, branch->variable->type, module);
							}

							Assert(branch->code.scope.parent);
							branch->code.scope.variables.Add(branch->variable);

							ScanExpression(branch->condition, &branch->code.scope, module);

							if (!CanImplicitCast(branch->condition->type, &type_bool))
							{
								Error(module, branch->condition->span, "For loop condition expression of type % cannot be implicitly casted to bool\n", branch->condition->type);
							}

							branch->condition = ImplicitCast(branch->condition, &type_bool, module);

							if (branch->stride)
							{
								ScanExpression(branch->stride, &branch->code.scope, module);

								Type* preferred_stride_type = branch->variable->type;

								if (IsPointer(branch->variable->type))
								{
									preferred_stride_type = &type_int64;
								}

								if (!IsFloat(branch->variable->type) && !IsInteger(branch->variable->type) && !IsPointer(branch->variable->type))
								{
									Error(module, branch->variable->assignment->span, "For loop variable type must be an int, float or pointer, not: %\n", branch->variable->type);
								}

								if (!CanImplicitCast(branch->stride->type, preferred_stride_type))
								{
									Error(module, branch->stride->span, "For loop stride expression of type % cannot be implicitly casted to %\n", branch->stride->type, preferred_stride_type);
								}

								branch->stride = ImplicitCast(branch->stride, preferred_stride_type, module);
							}

							branch->code.is_inside_loop = true;
						} break;
					}

					code->does_return = code->does_return || branch->code.does_return;
					branch->code.has_defer_that_returns = code->has_defer_that_returns;

					ScanCode(&branch->code, &code->scope, function, module);
				}

				code->all_paths_return = DoesBranchAlwaysReturn(branch_block, 0);
			} break;

			case AST_STATEMENT_DEFER:
			{
				code->defers.Add(&statement->defer);
				statement->defer.code.is_inside_loop = code->is_inside_loop;

				ScanCode(&statement->defer.code, &code->scope, function, module);

				if (statement->defer.code.all_paths_return)
				{
					code->has_defer_that_returns = true;
					code->all_paths_return = true;
				}
			} break;

			case AST_STATEMENT_CLAIM:
			{
				Ast_Claim* claim = &statement->claim;

				ScanExpression(claim->expression, &code->scope, module);

				if (!CanImplicitCast(claim->expression->type, &type_bool))
				{
					Error(module, claim->expression->span, "Claim expression type must be implicitly castable to bool\n");
				}

				claim->expression = ImplicitCast(claim->expression, &type_bool, module);
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				ScanExpression(inc->expression, &code->scope, module);

				bool direction = statement->kind == AST_STATEMENT_INCREMENT;

				if (!IsInteger(inc->expression->type) && !IsFloat(inc->expression->type) && !IsPointer(inc->expression->type))
				{
					Error(module, inc->expression->span, "Cannot % type %, expression must be either an integer, float or pointer.\n", (direction ? "increment" : "decrement"), inc->expression->type);
				}

				if (!inc->expression->is_referential_value)
				{
					Error(module, inc->expression->span, "Expression is not a referential value.\n");
				}
			} break;

			case AST_STATEMENT_RETURN:
			{
				code->does_return = true;
				code->all_paths_return = !code->does_break;
				function->returns.Add(&statement->ret);

				if (code->has_defer_that_returns)
				{
					Error(module, statement->ret.token->location, "A defer in this scope already has a return statement. This isn't allowed.\n");
				}

				if (statement->ret.expression)
				{
					ScanExpression(statement->ret.expression, &code->scope, module);

					if (!function->return_type)
					{
						Error(module, statement->ret.token->location, "Unexpected return value for function that doesn't return anything.\n");
					}

					if (!CanImplicitCast(function->return_type, statement->ret.expression->type))
					{
						Error(module, statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
					}
				}
				else if (function->ast_return_type)
				{
					Error(module, statement->ret.token->location, "Expected return value with type: %\n", function->return_type);
				}
			} break;

			case AST_STATEMENT_BREAK:
			{
				code->does_break = true;
				code->all_paths_break = !code->does_return;

				if (!code->is_inside_loop)
				{
					Error(module, statement->brk.token->location, "break statement must be inside of a loop.\n");
				}
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				ScanExpression(statement->expression, &code->scope, module);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* variable = &statement->variable_declaration;

				for (Ast_VariableDeclaration** other_variable = code->scope.variables; other_variable < code->scope.variables.End(); other_variable++)
				{
					if (CompareStrings(variable->name->string, (*other_variable)->name->string))
					{
						Error(module, variable->name->location, "Variable with name '%' already declared in this scope.\n", variable->name);
					}
				}

				if (variable->assignment)
				{
					ScanExpression(variable->assignment, &code->scope, module);

					if (!variable->assignment->type)
					{
						Error(module, variable->assignment->span, "Expression does not have a type.\n");
					}
				}

				if (variable->explicit_type)
				{
					variable->type = GetType(variable->explicit_type, &code->scope, module);
					variable->explicit_type->type = variable->type;

					if (!variable->type)
					{
						Error(module, variable->name->location, "Unknown type %\n", variable->explicit_type->basetype.token);
					}

					if (variable->assignment && !CanImplicitCast(variable->assignment->type, variable->type))
					{
						Error(module, variable->name->location, "Cannot assign expression with type % to variable with type %\n", variable->assignment->type, variable->type);
					}
				}
				else
				{
					variable->type = variable->assignment->type;
				}

				if (variable->type == &empty_tuple)
				{
					Error(module, variable->assignment->span, "Cannot declare variable with type %\n", variable->type);
				}

				code->scope.variables.Add(variable);
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				ScanExpression(statement->assignment.right, &code->scope, module);
				ScanExpression(statement->assignment.left,  &code->scope, module);

				if (!IsAssignable(statement->assignment.left))
				{
					Error(module, statement->assignment.left->span, "Expression is not assignable.\n");
				}

				if (!CanImplicitCast(statement->assignment.right->type, statement->assignment.left->type))
				{
					Error(module, statement->assignment.token->location, "Cannot cast % to %.\n", statement->assignment.right->type, statement->assignment.left->type);
				}

				statement->assignment.right = ImplicitCast(statement->assignment.right, statement->assignment.left->type, module);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;

				ScanExpression(assignment->right, &code->scope, module);
				ScanExpression(assignment->left,  &code->scope, module);

				if (!assignment->left->is_referential_value)
				{
					Error(module, assignment->left->span, "Expression is not assignable.\n");
				}

				if (!IsInteger(assignment->left->type) &&
					!IsFloat(assignment->left->type) &&
					!IsPointer(assignment->left->type))
				{
					Error(module, assignment->left->span, "Arithmetic assignment type must be to an integer, float or pointer, not: %\n", assignment->left->type);
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

				if (IsPointer(assignment->left->type))
				{
					if (statement->kind != AST_STATEMENT_ASSIGNMENT_ADD && statement->kind != AST_STATEMENT_ASSIGNMENT_SUBTRACT)
					{
						Error(module, assignment->left->span, "Arithmetic assignment to pointer only allows += and -=.\n");
					}

					if (!CanImplicitCast(assignment->right->type, &type_int64))
					{
						Error(module, assignment->right->span, "Expression type must be an integer.\n");
					}

					assignment->right = ImplicitCast(assignment->right, &type_int64, module);
				}
				else
				{
					if (!CanImplicitCast(assignment->right->type, assignment->left->type))
					{
						Error(module, assignment->right->span, "Expression is not numerical.\n");
					}

					assignment->right = ImplicitCast(assignment->right, assignment->left->type, module);
				}
			} break;
		}
	}
}

static Type* GetTypeFromParams(Array<Ast_VariableDeclaration> params, Ast_Module* module)
{
	Type* types[params.count];
	for (u32 i = 0; i < params.count; i++)
	{
		types[i] = params[i].type;
	}

	return GetTuple(Array(types, params.count));
}

void ScanFunction(Ast_Function* function, Ast_Scope* scope, Ast_Module* module)
{
	for (Ast_VariableDeclaration* param = function->parameters; param < function->parameters.End(); param++)
	{
		param->explicit_type->type = GetType(param->explicit_type, scope, module);
		param->type = param->explicit_type->type;
		function->code.scope.variables.Add(param);

		if (!param->explicit_type->type)
		{
			Error(module, param->explicit_type->basetype.token->location, "Unknown type '%'\n", param->explicit_type->basetype.token);
		}

		for (Ast_VariableDeclaration* param_other = function->parameters; param_other < param; param_other++)
		{
			if (CompareStrings(param_other->name->string, param->name->string))
			{
				Error(module, param->name->location, "Duplicate parameter called '%'\n", param->name->string);
			}
		}
	}

	if (function->ast_return_type)
	{
		function->ast_return_type->type = GetType(function->ast_return_type, scope, module);
		function->return_type = function->ast_return_type->type;

		if (function->ast_return_type->type == null)
		{
			Error(module, function->ast_return_type->basetype.token->location, "Unknown type: %\n", function->ast_return_type);
		}
	}
	else
	{
		function->return_type = &empty_tuple;
	}

	Type* param_type = GetTypeFromParams(function->parameters, module);
	function->type = GetFunctionType(param_type, function->return_type);
}

void SemanticParse(Ast_Module* module)
{
	ScanScope(&module->scope, module);
}


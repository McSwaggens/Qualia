#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"
#include "general.h"
#include "error.h"

static Type* GetType(Ast_Type* ast_type, Ast_Scope* scope, Ast_Module* module);

static uint64 CalculateStackFrameSize(Ast_Function* function)
{
	return CalculateStackFrameSize(&function->code, 0);
}

// @Note: This doesn't calculate the minimum memory needed to represent the stackframe which would be ideal for producing optimized binaries.
//        Another function needs to created for that.
static uint64 CalculateStackFrameSize(Ast_Code* code, uint64 offset)
{
	uint64 initial_offset = offset;

	for (uint32 i = 0; i < code->scope.variables.count; i++)
	{
		Ast_Variable* variable = code->scope.variables[i];
		variable->offset = offset;
		offset += variable->type->size;
		// Print("Variable %:\n\tsize = %\n\toffset = %\n", variable->name, variable->type->size, variable->offset);
	}

	for (uint32 i = 0; i < code->statements.count; i++)
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
		String name = token->identifier_string;
		while (scope)
		{
			for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
			{
				if (CompareString(name, ast_struct->name))
				{
					return &ast_struct->type;
				}
			}

			for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++)
			{
				if (CompareString(name, ast_enum->name))
				{
					return &ast_enum->type;
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
		uint32 tuple_count = basetype.tuple.count;

		if (!tuple_count)
		{
			Error(module, basetype.token->location, "Empty tuple is an invalid type.\n");
		}

		Type* types[tuple_count];

		for (uint32 i = 0; i < tuple_count; i++)
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
					Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL); // @RemoveMe @Todo: Need to interpret size_expression

					if (!(specifier->size_expression->flags & AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE))
					{
						Error(module, specifier->size_expression->span, "Fixed array size must be constantly evaluatable.\n");
					}

					if (!IsInteger(specifier->size_expression->type))
					{
						Error(module, specifier->size_expression->span, "Fixed array size must be an integer.\n");
					}

					Ast_Expression_Literal* literal = (Ast_Expression_Literal*)specifier->size_expression;
					uint64 length = literal->token->literal_int;

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

static void Intrinsic_SystemCall(uint64* input, uint64* output)
{
}

static const String intrinsic_name_lut[INTRINSIC_COUNT] = {
	[INTRINSIC_SYSTEM_CALL] = "SystemCall",
};

static Type* intrinsic_type_lut[INTRINSIC_COUNT];

static inline Type* GetIntrinsicType(IntrinsicID intrinsic)
{
	return intrinsic_type_lut[intrinsic];
}

static void InitIntrinsics()
{
	intrinsic_type_lut[INTRINSIC_SYSTEM_CALL] = GetFunctionType(
		GetTuple(Array<Type*>((Type*[7]){
			&type_int64, // rax
			&type_int64, // rdi
			&type_int64, // rsi
			&type_int64, // rdx
			&type_int64, // r10
			&type_int64, // r8
			&type_int64, // r9
		}, 7)),
		&type_int64
	);
}

static IntrinsicID FindIntrinsic(String name, Type* input_type)
{
	for (uint64 id = 0; id < INTRINSIC_COUNT; id++)
	{
		if (CompareString(name, intrinsic_name_lut[id]) && CanImplicitCast(input_type, intrinsic_type_lut[id]->input))
		{
			return (IntrinsicID)id;
		}
	}

	return INTRINSIC_INVALID;
}

static Ast_Function* FindFunction(Ast_Scope* scope, String name, Type* input_type)
{
	while (scope)
	{
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
		{
			if (CompareString(name, function->name) && CanImplicitCast(input_type, function->type->input))
			{
				return function;
			}
		}

		scope = scope->parent;
	}

	return null;
}

static Ast_Variable* FindVariable(Ast_Scope* scope, String name)
{
	while (scope)
	{
		for (uint32 i = 0; i < scope->variables.count; i++)
		{
			Ast_Variable* variable = scope->variables[i];
			if (CompareString(name, variable->name))
			{
				return variable;
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
		if (CompareString(name, member->name))
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
		if (CompareString(name, member->name))
		{
			return member;
		}
	}

	return null;
}

static Ast_Expression* ImplicitCast(Ast_Expression* expression, Type* type, Ast_Module* module)
{
	if (expression->type == type) return expression;

	Ast_Expression_Implicit_Cast* cast = StackAllocate<Ast_Expression_Implicit_Cast>(&module->stack);
	cast->kind = AST_EXPRESSION_IMPLICIT_CAST;
	cast->span = expression->span;
	cast->subexpression = expression;
	cast->type = type;
	cast->flags = expression->flags & (AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE | AST_EXPRESSION_FLAG_PURE);
	return cast;
}

static bool IsEffectiveInteger(Type* type)
{
	return IsInteger(type) || type->kind == TYPE_BASETYPE_BOOL || type->kind == TYPE_BASETYPE_ENUM;
}

static Type* GetEffectiveType(Type* type)
{
	if (type->kind == TYPE_BASETYPE_BOOL) return &type_uint8;
	if (type->kind == TYPE_BASETYPE_ENUM) return type->enumeration->underlying_type;
	return type;
}

static Ast_Expression* ImplicitCastToEffectiveType(Ast_Expression* expression, Ast_Module* module)
{
	return ImplicitCast(expression, GetEffectiveType(expression->type), module);
}

static void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Ast_Module* module)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_NAME:
		{
			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)expression;

			if (terminal->token->kind == TOKEN_IDENTIFIER_CASUAL)
			{
				Ast_Variable* variable = FindVariable(scope, terminal->token->identifier_string);

				if (!variable)
				{
					Error(module, terminal->token->location, "Variable with name '%' does not exist.\n", terminal->token);
				}

				Ast_Expression_Variable* variable_expression = (Ast_Expression_Variable*)expression;
				variable_expression->variable = variable;

				variable_expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
				variable_expression->type = variable->type;

				variable_expression->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_REFERENTIAL;

				if (variable->flags & AST_VARIABLE_FLAG_CONSTANT)
				{
					variable_expression->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;
				}
				else if (variable->flags & AST_VARIABLE_FLAG_GLOBAL)
				{
					variable_expression->flags = AST_EXPRESSION_FLAG_REFERENTIAL;
				}
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
					expression->kind = AST_EXPRESSION_TERMINAL_STRUCT; // @FixMe
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
					expression->kind = AST_EXPRESSION_TERMINAL_PRIMITIVE; // @FixMe
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
			Type* subtype;

			Ast_Expression_Flags flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

			for (uint32 i = 0; i < fixed_array->elements.count; i++)
			{
				Ast_Expression* element = fixed_array->elements[i];
				ScanExpression(element, scope, module);

				flags &= element->flags;

				if (i)
				{
					if (!CanImplicitCast(element->type, subtype))
					{
						Error(module, element->span, "Cannot implicitly cast type '%' to '%'\n", subtype, element->type); // @FixMe
					}

					fixed_array->elements[i] = ImplicitCast(element, subtype, module);
				}
				else
				{
					subtype = element->type;
					fixed_array->type = GetFixedArray(subtype, fixed_array->elements.count);
				}
			}

			fixed_array->flags = flags;
		} break;

		case AST_EXPRESSION_DYNAMIC_ARRAY:
		{
			Ast_Expression_Dynamic_Array* array = (Ast_Expression_Dynamic_Array*)expression;

			ScanExpression(array->left, scope, module);
			ScanExpression(array->right, scope, module);

			array->flags = (array->left->flags & array->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

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
			else if (CanImplicitCast(array->right->type, &type_uint64))
			{
				array->right = ImplicitCast(array->right, &type_uint64, module);
			}
			else
			{
				Error(module, array->right->span, "Array end must be a % or an uint, not: %\n", array->left->type, array->right->type);
			}

			array->type = GetDynamicArray(array->left->type->subtype);
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;
			literal->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

			switch (literal->token->kind)
			{
				case TOKEN_LITERAL_INT:    literal->type = &type_int64;  literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_INT8:   literal->type = &type_int8;   literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_INT16:  literal->type = &type_int16;  literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_INT32:  literal->type = &type_int32;  literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_INT64:  literal->type = &type_int64;  literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_UINT:   literal->type = &type_uint64; literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_UINT8:  literal->type = &type_uint8;  literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_UINT16: literal->type = &type_uint16; literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_UINT32: literal->type = &type_uint32; literal->value_int = literal->token->literal_int; break;
				case TOKEN_LITERAL_UINT64: literal->type = &type_uint64; literal->value_int = literal->token->literal_int; break;

				case TOKEN_LITERAL_FLOAT:
				{
					literal->type = &type_float32; // @Fixme?
					literal->value_f32 = literal->token->literal_float;
				} break;

				case TOKEN_LITERAL_FLOAT16:
				{
					literal->type = &type_float16;
					literal->value_f32 = literal->token->literal_float; // @Bug
					Assert();
				} break;

				case TOKEN_LITERAL_FLOAT32:
				{
					literal->type = &type_float32;
					literal->value_f32 = literal->token->literal_float;
				} break;

				case TOKEN_LITERAL_FLOAT64:
				{
					literal->type = &type_float64;
					literal->value_f64 = literal->token->literal_float;
				} break;

				case TOKEN_TRUE:  literal->type = &type_bool; literal->value_int = 1; break;
				case TOKEN_FALSE: literal->type = &type_bool; literal->value_int = 0; break;
				case TOKEN_NULL:  literal->type = GetPointer(&type_byte); literal->value_int = 0; break;

				case TOKEN_LITERAL_STRING:
				{
					literal->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;
					literal->type = GetFixedArray(&type_uint8, literal->token->literal_string.length);
				} break;

				default: Assert(); Unreachable();
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			tuple->recursive_count = tuple->elements.count;

			Type* types[tuple->elements.count];

			Ast_Expression_Flags element_flags =
				AST_EXPRESSION_FLAG_REFERENTIAL |
				AST_EXPRESSION_FLAG_PURE |
				AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

			Ast_Expression_Flags tuple_flags =
				AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL |
				AST_EXPRESSION_FLAG_PURE |
				AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

			for (uint32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				ScanExpression(element, scope, module);
				types[i] = element->type;

				if (element->kind == AST_EXPRESSION_TUPLE)
				{
					tuple->recursive_count += ((Ast_Expression_Tuple*)element)->recursive_count-1;
					tuple_flags &= element->flags;
				}
				else
				{
					element_flags &= element->flags;
				}

				if (element->type == &empty_tuple && (element->kind != AST_EXPRESSION_TUPLE || tuple->elements.count > 1))
				{
					Error(module, element->span, "Tuple elements aren't allowed to be of type %.\n", element->type);
				}
			}

			tuple->flags = element_flags & tuple_flags;

			if (element_flags & AST_EXPRESSION_FLAG_REFERENTIAL)
			{
				tuple->flags |= (tuple_flags & AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL);
			}

			tuple->type = GetTuple(Array(types, tuple->elements.count));
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);

			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!(unary->subexpression->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
			{
				Error(module, unary->span, "Cannot take address of a non-referential value.\n");
			}

			unary->type = GetPointer(unary->subexpression->type);
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);

			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
			unary->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;

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
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (IsEffectiveInteger(unary->subexpression->type))
			{
				unary->subexpression = ImplicitCast(unary->subexpression, GetEffectiveType(unary->subexpression->type), module);
				unary->type = unary->subexpression->type;
			}
			else if (IsPointer(unary->subexpression->type))
			{
				unary->type = unary->subexpression->type;
			}
			else
			{
				Error(module, unary->subexpression->span, "Type % is not an integer or pointer.\n", unary->subexpression->type);
			}
		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!IsEffectiveInteger(unary->subexpression->type) && !IsPointer(unary->subexpression->type) && !IsFloat(unary->subexpression->type))
			{
				Error(module, unary->subexpression->span, "Unary minus does not work on type '%'.\n", unary->subexpression->type);
			}

			unary->subexpression = ImplicitCast(unary->subexpression, GetEffectiveType(unary->subexpression->type), module);
			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

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
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

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
				String name = member_terminal->token->identifier_string;

				Ast_Enum_Member* member = FindEnumMember(ast_enum, name);

				if (!member)
				{
					Error(module, binary->right->span, "Enum % does not contain a member called \"%\".", ast_enum->name, name);
				}

				member_terminal->member = member;
				member_terminal->type = type;
				member_terminal->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

				binary->type = type;
				binary->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;
			}
			else if (binary->right->kind == AST_EXPRESSION_TERMINAL_NAME && ((Ast_Expression_Terminal*)binary->right)->token->kind == TOKEN_IDENTIFIER_CASUAL)
			{
				Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;
				String name = terminal->token->identifier_string;

				while (type->kind == TYPE_SPECIFIER_POINTER) type = type->subtype;

				if (type->kind == TYPE_BASETYPE_STRUCT)
				{
					binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE | AST_EXPRESSION_FLAG_REFERENTIAL);

					if (binary->left->type->kind == TYPE_SPECIFIER_POINTER)
					{
						binary->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;
					}

					Ast_Struct* ast_struct = type->structure;
					Ast_Struct_Member* member = FindStructMember(ast_struct, terminal->token->identifier_string);

					if (!member)
					{
						Error(module, binary->span, "Struct % does not have a member named %\n", ast_struct->name, terminal->token);
					}

					binary->type = member->type;
					terminal->kind = AST_EXPRESSION_TERMINAL_STRUCT_MEMBER;
					((Ast_Expression_Struct_Member*)terminal)->member = member;
					terminal->type = member->type;
				}
				else if (type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY || type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
				{
					bool fixed = type->kind == TYPE_SPECIFIER_FIXED_ARRAY;

					if (CompareString(name, "begin") || CompareString(name, "data"))
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_BEGIN;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

						if (!fixed)
						{
							binary->flags |= binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL;
						}

						binary->type = GetPointer(binary->left->type->subtype);
					}
					else if (CompareString(name, "end"))
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_END;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
						binary->type = GetPointer(binary->left->type->subtype);
					}
					else if (CompareString(name, "length") || CompareString(name, "count"))
					{
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_LENGTH;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

						if (!fixed)
						{
							binary->flags |= binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL;
						}

						binary->type = &type_uint64;
					}
					else
					{
						Error(module, binary->right->span, "'%' does not have a member named '%'.\n", type, name);
					}
				}
				else
				{
					Error(module, binary->span, "'%' does not have a member named '%'.\n", type, name);
				}
			}
			else
			{
				Error(module, binary->span, "Invalid dot expression.\n", binary->left->type);
			}
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);

			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (!dominant)
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
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			Type* dominant = GetDominantType(GetEffectiveType(binary->left->type), GetEffectiveType(binary->right->type));

			if (!dominant)
			{
				Error(module, binary->span, "Incompatible types % and %\n", binary->left->type, binary->right->type);
			}

			if (!IsInteger(dominant) && !IsFloat(dominant) && !IsPointer(dominant))
			{
				Error(module, binary->span, "Cannot compare types '%' and '%'.\n", binary->left->type, binary->right->type);
			}

			binary->left  = ImplicitCast(binary->left,  dominant, module);
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
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (IsPointer(binary->left->type))
			{
				// ptr + int = ptr
				// ptr - int = ptr
				// ptr - ptr = int

				if (binary->kind == AST_EXPRESSION_BINARY_ADD)
				{
					if (!CanImplicitCast(binary->right->type, &type_int64))
					{
						Error(module, binary->span, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
					}

					binary->right = ImplicitCast(binary->right, &type_int64, module);
					binary->type = binary->left->type;
				}
				else if (binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
				{
					if (IsPointer(binary->right->type))
					{
						Type* dominant = GetDominantType(binary->left->type, binary->right->type);

						if (!dominant)
						{
							Error(module, binary->span, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
						}

						binary->left = ImplicitCast(binary->left, dominant, module);
						binary->right = ImplicitCast(binary->right, dominant, module);

						binary->type = &type_int64;
					}
					else if (CanImplicitCast(binary->right->type, &type_int64))
					{
						binary->right = ImplicitCast(binary->right, &type_int64, module);
						binary->type = binary->left->type;
					}
					else
					{
						Error(module, binary->span, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
					}
				}
				else
				{
					Error(module, binary->span, "Binary expression '%' cannot be used on a pointer.\n", binary->op);
				}
			}
			else if (IsFloat(binary->left->type) || IsFloat(binary->right->type))
			{
				// float + float = float
				// float - float = float
				// float * float = float
				// float / float = float

				Type* dominant = GetDominantType(binary->left->type, binary->right->type);

				if (!dominant || !IsFloat(dominant) || binary->kind == AST_EXPRESSION_BINARY_MODULO)
				{
					Error(module, binary->span, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
				}

				binary->left  = ImplicitCast(binary->left,  dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = dominant;
			}
			else
			{
				// int + int
				// int - int
				// int * int
				// int / int
				// int % int

				Type* dominant = GetDominantType(GetEffectiveType(binary->left->type), GetEffectiveType(binary->right->type));

				if (!dominant || !IsInteger(dominant))
				{
					Error(module, binary->span, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
				}

				binary->left = ImplicitCast(binary->left, dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = dominant;
			}
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			// ptr OR  int = ptr
			// ptr XOR int = ptr
			// ptr AND int = int

			if (!IsEffectiveInteger(binary->left->type) && !IsPointer(binary->left->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);
			}

			if (!IsEffectiveInteger(binary->right->type) && !IsPointer(binary->right->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);
			}

			binary->type = binary->left->type;
		} break;

		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!IsEffectiveInteger(binary->left->type) && !IsPointer(binary->left->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);
			}

			if (!IsEffectiveInteger(binary->right->type))
			{
				Error(module, binary->span, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);
			}

			binary->left  = ImplicitCastToEffectiveType(binary->left,  module);
			binary->right = ImplicitCastToEffectiveType(binary->right, module);

			binary->type = binary->left->type;
		} break;


		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

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
			ternary->flags = (ternary->left->flags & ternary->middle->flags & ternary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			Type* dominant = GetDominantType(ternary->left->type, ternary->right->type);

			if (!dominant)
			{
				Error(module, ternary->left->span, "Types '%' and '%' are incompatible.\n", ternary->left->type, ternary->right->type);
			}

			if (!CanImplicitCast(ternary->middle->type, &type_bool))
			{
				Error(module, ternary->middle->span, "Type % not convertable to bool\n", ternary->middle->type);
			}

			ternary->middle = ImplicitCast(ternary->middle, &type_bool, module);
			ternary->left = ImplicitCast(ternary->left, dominant, module);
			ternary->right = ImplicitCast(ternary->right, dominant, module);

			if (ternary->left->type == ternary->right->type)
			{
				ternary->flags |= ternary->left->flags & ternary->right->flags & AST_EXPRESSION_FLAG_REFERENTIAL;
			}

			ternary->type = dominant;
		} break;

		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;

			Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)call->function;
			Ast_Expression_Binary* dot = (Ast_Expression_Binary*)call->function;

			ScanExpression(call->parameters, scope, module);

			if (dot->kind == AST_EXPRESSION_BINARY_DOT && dot->right->kind == AST_EXPRESSION_TERMINAL_NAME &&
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
				Ast_Function* function = FindFunction(scope, function_expression->token->identifier_string, full_param_type);

				if (!function)
				{
					Error(module, call->span, "No function exists called % with input type: %.\n", function_expression->token->identifier_string, full_param_type);
				}

				function_expression->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
				function_expression->function = function;
				function_expression->type = function->type;

				call->type = function->return_type;
				call->flags = 0;

				Assert(call->parameters->elements.count == function->parameters.count-1);

				dot->left = ImplicitCast(dot->left, function->parameters[0].type, module);

				for (uint32 i = 0; i < call->parameters->elements.count; i++)
				{
					call->parameters->elements[i] = ImplicitCast(call->parameters->elements[i], function->parameters[i+1].type, module);
				}
			}
			else if (call->function->kind == AST_EXPRESSION_TERMINAL_NAME && terminal->token->kind == TOKEN_IDENTIFIER_FORMAL)
			{
				if (Ast_Function* function = FindFunction(scope, terminal->token->identifier_string, call->parameters->type); function)
				{
					Ast_Expression_Function* function_expression = (Ast_Expression_Function*)call->function;
					call->function->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
					function_expression->function = function;
					function_expression->type = function->type;

					call->type = function->return_type;
					call->flags = 0;

					call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, function->type->input, module);
				}
				else if (IntrinsicID intrinsic = FindIntrinsic(terminal->token->identifier_string, call->parameters->type); intrinsic != INTRINSIC_INVALID)
				{
					Type* type = GetIntrinsicType(intrinsic);

					Ast_Expression_Intrinsic* intrinsic_expression = (Ast_Expression_Intrinsic*)call->function;
					intrinsic_expression->intrinsic = intrinsic;
					intrinsic_expression->type = type;

					call->function->kind = AST_EXPRESSION_TERMINAL_INTRINSIC;
					call->type = type->output;
					call->flags = 0;

					call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, type->input, module);
				}
				else
				{
					Error(module, terminal->token->location, "No function exists called % with input type: %.\n", terminal->token->identifier_string, call->parameters->type);
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
				call->flags = 0;
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

			bool fixed = subscript->array->type->kind == TYPE_SPECIFIER_FIXED_ARRAY;

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
			subscript->flags = subscript->array->flags & subscript->index->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
			subscript->flags |= subscript->array->flags & AST_EXPRESSION_FLAG_REFERENTIAL;

			if (!fixed)
			{
				subscript->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;
			}

			subscript->type = subscript->array->type->subtype;
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			ScanExpression(as->expression, scope, module);
			as->type = GetType(&as->ast_type, scope, module);

			as->flags = as->expression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!CanExplicitCast(as->expression->type, as->type))
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
	for (uint32 i = 0; i < tuple.count; i++)
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
		Type* type = member->type;
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
		Error(module, ast_struct->name_token->location, "The closure of struct '%' contains '%' (circularly dependent)\n", ast_struct->name, ast_struct->name);
	}
}

static void CalculateStructSize(Ast_Struct* s);
static void CalculateTupleSize(Type* tuple);

static void CalculateTupleSize(Type* tuple)
{
	if (tuple->size) return;

	uint64 size = 0;

	Assert(tuple->tuple.count);
	for (uint32 i = 0; i < tuple->tuple.count; i++)
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

	uint64 size = 0;

	for (Ast_Struct_Member* member = s->members; member < s->members.End(); member++)
	{
		Type* type = member->type;

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

static void ScanScope(Ast_Scope* scope, Ast_Module* module)
{
	for (uint32 i = 0; i < scope->variables.count; i++) // @RemoveMe?
	{
		Ast_Variable* var = scope->variables[i];
		var->ir = None();
	}

	// @Yuck: This function is *disgusting*, all of this should be implicit when we try to use a type for the first time or something... Also, what about multi-threading?
	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
	{
		ZeroMemory(&ast_struct->type);
		ast_struct->type.kind = TYPE_BASETYPE_STRUCT;
		ast_struct->type.structure = ast_struct;

		for (Ast_Struct* other = scope->structs; other < ast_struct; other++)
		{
			if (CompareString(ast_struct->name, other->name))
			{
				Error(module, ast_struct->name_token->location, "Duplicate struct called '%'\n", ast_struct->name);
			}
		}

		for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++)
		{
			if (CompareString(ast_struct->name, ast_enum->name))
			{
				if (ast_struct->name_token->location.line < ast_enum->name_token->location.line)
				{
					Error(module, ast_enum->name_token->location, "Duplicate type called '%'\n", ast_enum->name);
				}
				else
				{
					Error(module, ast_struct->name_token->location, "Duplicate type called '%'\n", ast_struct->name);
				}
			}
		}

		for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
		{
			for (Ast_Struct_Member* member_other = ast_struct->members; member_other < member; member_other++)
			{
				if (CompareString(member->name, member_other->name))
				{
					Error(module, member->name_token->location, "Duplicate member called '%' in struct %\n", member->name, ast_struct->name);
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
			if (CompareString(ast_enum->name, eo->name))
			{
				Error(module, ast_enum->name_token->location, "Duplicate enum called '%'\n", ast_enum->name);
			}
		}

		for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++)
		{
			for (Ast_Enum_Member* member_other = ast_enum->members; member_other < member; member_other++)
			{
				if (CompareString(member->name, member_other->name))
				{
					Error(module, member->name_token->location, "Duplicate member called '%' in enum %\n", member->name, ast_enum->name);
				}
			}
		}
	}

	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++)
	{
		for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++)
		{
			member->type = GetType(&member->ast_type, scope, module);

			if (!member->type)
			{
				Error(module, member->ast_type.basetype.token->location, "Unknown type '%'\n", member->ast_type.basetype.token);
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
			if (function->type == other->type && CompareString(function->name, other->name))
			{
				Error(module, function->name_token->location, "Function '%' with type % already exists.\n", function->name, function->type);
			}
		}
	}

	for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++)
	{
		ScanCode(&function->code, scope, function, module);

		if (function->return_type != &empty_tuple && !function->code.all_paths_return)
		{
			if (function->code.contains_return)
			{
				Error(module, function->name_token->location, "Not all paths return a value.\n");
			}
			else
			{
				Error(module, function->name_token->location, "Function does not return a value.\n");
			}
		}
	}
}

static bool IsAssignable(Ast_Expression* expression)
{
	return expression->flags & (AST_EXPRESSION_FLAG_REFERENTIAL | AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL);
}

static bool IsLoop(Ast_Branch* branch)
{
	switch (branch->kind)
	{
		case AST_BRANCH_NAKED:
		case AST_BRANCH_IF:
			return false;

		case AST_BRANCH_WHILE:
		case AST_BRANCH_FOR_RANGE:
		case AST_BRANCH_FOR_VERBOSE:
			return true;
	}
}

static bool DoesBranchAlwaysReturn(Ast_Branch* branch)
{
	if (!branch) return false;

	bool true_path_returns = branch->code.all_paths_return || DoesBranchAlwaysReturn(branch->then_branch);

	if (branch->kind == AST_BRANCH_NAKED) return true_path_returns;

	return true_path_returns && DoesBranchAlwaysReturn(branch->else_branch);
}

static void SemanticParseBranchBlock(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_BranchBlock* branch_block)
{
	for (uint32 i = 0; i < branch_block->branches.count; i++)
	{
		Ast_Branch* branch = &branch_block->branches[i];

		branch->code.scope.parent = &code->scope; // @Hack

		switch (branch->kind)
		{
			case AST_BRANCH_NAKED:
			{
				branch->code.is_inside_loop = code->is_inside_loop;
				code->contains_break = code->contains_break || branch->code.contains_break;
			} break;

			case AST_BRANCH_IF:
			{
				ScanExpression(branch->if_condition, &code->scope, module);

				if (!CanImplicitCast(branch->if_condition->type, &type_bool))
				{
					Error(module, branch->if_condition->span, "Cannot implicitly cast condition with type % to bool\n", branch->if_condition->type);
				}

				branch->if_condition = ImplicitCast(branch->if_condition, &type_bool, module);

				branch->code.is_inside_loop = code->is_inside_loop;
				code->contains_break = code->contains_break || branch->code.contains_break;
			} break;

			case AST_BRANCH_WHILE:
			{
				ScanExpression(branch->while_condition, &code->scope, module);

				if (!CanImplicitCast(branch->while_condition->type, &type_bool))
				{
					Error(module, branch->while_condition->span, "Cannot implicitly cast condition with type % to bool\n", branch->while_condition->type);
				}

				branch->while_condition = ImplicitCast(branch->while_condition, &type_bool, module);

				branch->code.is_inside_loop = true;
			} break;

			case AST_BRANCH_FOR_RANGE:
			{
				ScanExpression(branch->for_range.range, &code->scope, module);
				branch->code.scope.variables.Add(branch->for_range.iterator); // for i in i:

				if (!IsDynamicArray(branch->for_range.range->type) && !IsFixedArray(branch->for_range.range->type))
				{
					Error(module, branch->for_range.range->span, "For loop cannot range over type: %\n", branch->for_range.range->type);
				}

				if (branch->for_range.filter)
				{
					ScanExpression(branch->for_range.filter, &branch->code.scope, module);

					if (!CanImplicitCast(branch->for_range.filter->type, &type_bool))
					{
						Error(module, branch->for_range.filter->span, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->for_range.filter->type);
					}

					branch->for_range.filter = ImplicitCast(branch->for_range.filter, &type_bool, module);
				}

				if (branch->for_range.stride)
				{
					ScanExpression(branch->for_range.stride, &branch->code.scope, module);

					if (!CanImplicitCast(branch->for_range.stride->type, &type_int64))
					{
						Error(module, branch->for_range.filter->span, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->for_range.filter->type);
					}

					branch->for_range.stride = ImplicitCast(branch->for_range.stride, &type_int64, module);
				}

				branch->code.is_inside_loop = true;
			} break;

			case AST_BRANCH_FOR_VERBOSE:
			{
				if (branch->for_verbose.variable->assignment)
				{
					ScanExpression(branch->for_verbose.variable->assignment, &code->scope, module);
					branch->for_verbose.variable->type = branch->for_verbose.variable->assignment->type;
				}

				if (branch->for_verbose.variable->ast_type)
				{
					branch->for_verbose.variable->type = GetType(branch->for_verbose.variable->ast_type, &code->scope, module);
				}

				if (branch->for_verbose.variable->ast_type && branch->for_verbose.variable->assignment)
				{
					if (!CanImplicitCast(branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type))
					{
						Error(module, branch->for_verbose.variable->assignment->span, "Cannot assign expression with type % to variable with type %\n", branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type);
					}

					branch->for_verbose.variable->assignment = ImplicitCast(branch->for_verbose.variable->assignment, branch->for_verbose.variable->type, module);
				}

				Assert(branch->code.scope.parent);
				branch->code.scope.variables.Add(branch->for_verbose.variable);

				ScanExpression(branch->for_verbose.condition, &branch->code.scope, module);

				if (!CanImplicitCast(branch->for_verbose.condition->type, &type_bool))
				{
					Error(module, branch->for_verbose.condition->span, "For loop condition expression of type % cannot be implicitly casted to bool\n", branch->for_verbose.condition->type);
				}

				branch->for_verbose.condition = ImplicitCast(branch->for_verbose.condition, &type_bool, module);

				if (branch->for_verbose.stride)
				{
					ScanExpression(branch->for_verbose.stride, &branch->code.scope, module);

					Type* preferred_stride_type = branch->for_verbose.variable->type;

					if (IsPointer(branch->for_verbose.variable->type))
					{
						preferred_stride_type = &type_int64;
					}

					if (!IsFloat(branch->for_verbose.variable->type) && !IsInteger(branch->for_verbose.variable->type) && !IsPointer(branch->for_verbose.variable->type))
					{
						Error(module, branch->for_verbose.variable->assignment->span, "For loop variable type must be an int, float or pointer, not: %\n", branch->for_verbose.variable->type);
					}

					if (!CanImplicitCast(branch->for_verbose.stride->type, preferred_stride_type))
					{
						Error(module, branch->for_verbose.stride->span, "For loop stride expression of type % cannot be implicitly casted to %\n", branch->for_verbose.stride->type, preferred_stride_type);
					}

					branch->for_verbose.stride = ImplicitCast(branch->for_verbose.stride, preferred_stride_type, module);
				}

				branch->code.is_inside_loop = true;
			} break;
		}

		ScanCode(&branch->code, &code->scope, function, module);

		code->contains_return = code->contains_return || branch->code.contains_return;
		branch->code.has_defer_that_returns = code->has_defer_that_returns;
	}

	code->all_paths_return = DoesBranchAlwaysReturn(&branch_block->branches[0]);
}

static void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Ast_Module* module)
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
				SemanticParseBranchBlock(module, function, code, branch_block);
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

				if (!(inc->expression->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
				{
					Error(module, inc->expression->span, "Expression is not a referential value.\n");
				}
			} break;

			case AST_STATEMENT_RETURN:
			{
				code->contains_return = true;
				code->all_paths_return = !code->contains_break;
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
				code->contains_break = true;
				code->all_paths_break = !code->contains_return;

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
				Ast_Variable* variable = &statement->variable_declaration;

				for (Ast_Variable** other_variable = code->scope.variables.Begin(); other_variable < code->scope.variables.End(); other_variable++)
				{
					if (CompareString(variable->name, (*other_variable)->name))
					{
						Error(module, variable->name_token->location, "Variable with name '%' already declared in this scope.\n", variable->name);
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

				if (variable->ast_type)
				{
					variable->type = GetType(variable->ast_type, &code->scope, module);

					if (!variable->type)
					{
						Error(module, variable->name_token->location, "Unknown type %\n", variable->ast_type->basetype.token);
					}

					if (variable->assignment && !CanImplicitCast(variable->assignment->type, variable->type))
					{
						Error(module, variable->name_token->location, "Cannot assign expression with type % to variable with type %\n", variable->assignment->type, variable->type);
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
				Ast_Assignment* assignment = &statement->assignment;

				ScanExpression(assignment->right, &code->scope, module);
				ScanExpression(assignment->left,  &code->scope, module);

				if (!IsAssignable(assignment->left))
				{
					Error(module, assignment->left->span, "Expression is not assignable.\n");
				}

				if (!CanImplicitCast(assignment->right->type, assignment->left->type))
				{
					Error(module, assignment->token->location, "Cannot cast % to %.\n", assignment->right->type, assignment->left->type);
				}

				assignment->right = ImplicitCast(assignment->right, assignment->left->type, module);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL:
			{
				Ast_Assignment* assignment = &statement->assignment;

				ScanExpression(assignment->right, &code->scope, module);
				ScanExpression(assignment->left,  &code->scope, module);

				if (!(assignment->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
				{
					Error(module, assignment->left->span, "Expression is not assignable.\n");
				}

				if (!IsInteger(assignment->left->type) &&
					!IsFloat(assignment->left->type) &&
					!IsPointer(assignment->left->type))
				{
					Error(module, assignment->left->span, "Arithmetic assignment type must be to an integer, float or pointer, not: '%'\n", assignment->left->type);
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
						Error(module, assignment->left->span, "Arithmetic assignment to pointer only allows '+=' and '-='.\n");
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
						Error(module, assignment->right->span, "Cannot implicitly cast '%' to '%'.\n", assignment->right->type, assignment->left->type);
					}

					assignment->right = ImplicitCast(assignment->right, assignment->left->type, module);
				}
			} break;
		}
	}
}

static Type* GetTypeFromParams(Array<Ast_Variable> params, Ast_Module* module)
{
	Type* types[params.count];
	for (uint32 i = 0; i < params.count; i++)
	{
		types[i] = params[i].type;
	}

	return GetTuple(Array(types, params.count));
}

static void ScanFunction(Ast_Function* function, Ast_Scope* scope, Ast_Module* module)
{
	for (Ast_Variable* param = function->parameters; param < function->parameters.End(); param++)
	{
		param->type = GetType(param->ast_type, scope, module);
		function->code.scope.variables.Add(param);

		if (!param->type)
		{
			Error(module, param->ast_type->basetype.token->location, "Unknown type '%'\n", param->ast_type->basetype.token);
		}

		for (Ast_Variable* param_other = function->parameters; param_other < param; param_other++)
		{
			if (CompareString(param_other->name, param->name))
			{
				Error(module, param->name_token->location, "Duplicate parameter called '%'\n", param->name);
			}
		}
	}

	if (function->ast_return_type)
	{
		function->return_type = GetType(function->ast_return_type, scope, module);

		if (!function->return_type)
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

static void SemanticParse(Ast_Module* module)
{
	ScanScope(&module->scope, module);
}


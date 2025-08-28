#include "parser.h"
#include "memory.h"
#include "print.h"
#include "assert.h"
#include "general.h"
#include "error.h"

static TypeID GetType(Ast_Type* ast_type, Ast_Scope* scope, Ast_Module* module);

static TypeID FindUserType(String name, Ast_Scope* scope) {
	while (scope) {
		for (Ast_Struct* ast = scope->structs; ast < scope->structs.End(); ast++) {
			if (name == ast->name)
				return ast->type;
		}

		for (Ast_Enum* ast = scope->enums; ast < scope->enums.End(); ast++) {
			if (name == ast->name)
				return ast->type;
		}

		scope = scope->parent;
	}
	return TYPE_NULL;
}

static TypeID GetBaseType(Ast_BaseType basetype, Ast_Scope* scope, Ast_Module* module) {
	TypeID result = TYPE_NULL;

	switch (basetype.kind) {
		case AST_BASETYPE_PRIMITIVE: {
			switch (basetype.token->kind) {
				case TOKEN_BOOL:    result = TYPE_BOOL;    break;
				case TOKEN_BYTE:    result = TYPE_BYTE;    break;

				case TOKEN_INT:     result = type_int;     break;
				case TOKEN_INT8:    result = TYPE_INT8;    break;
				case TOKEN_INT16:   result = TYPE_INT16;   break;
				case TOKEN_INT32:   result = TYPE_INT32;   break;
				case TOKEN_INT64:   result = TYPE_INT64;   break;

				case TOKEN_UINT:    result = type_uint;    break;
				case TOKEN_UINT8:   result = TYPE_UINT8;   break;
				case TOKEN_UINT16:  result = TYPE_UINT16;  break;
				case TOKEN_UINT32:  result = TYPE_UINT32;  break;
				case TOKEN_UINT64:  result = TYPE_UINT64;  break;

				case TOKEN_FLOAT32: result = TYPE_FLOAT32; break;
				case TOKEN_FLOAT64: result = TYPE_FLOAT64; break;

				default: AssertUnreachable();
			}
		} break;

		case AST_BASETYPE_TUPLE: {
			u32 tuple_count = basetype.tuple.length;

			if (!tuple_count)
				Error(module, basetype.token->location, "Empty tuple is an invalid type.\n");

			TypeID types[tuple_count];

			for (u32 i = 0; i < tuple_count; i++) {
				types[i] = GetType(&basetype.tuple[i], scope, module);
			}

			result = GetTuple({types, tuple_count});
		} break;

		case AST_BASETYPE_FUNCTION: {
			TypeID input  = GetType(basetype.function.input, scope, module);
			TypeID output = GetType(basetype.function.output, scope, module);

			result = GetFunctionType(input, output);
		} break;

		case AST_BASETYPE_USERTYPE: {
			result = FindUserType(basetype.token->identifier_string, scope);
		} break;
	}

	return result;
}

static TypeID GetType(Ast_Type* ast_type, Ast_Scope* scope, Ast_Module* module) {
	TypeID result = TYPE_EMPTY_TUPLE;

	if (!ast_type)
		return result;

	result = GetBaseType(ast_type->basetype, scope, module);

	if (!result)
		return result;

	for (s32 i = ast_type->specifiers.length-1; i >= 0; i--) {
		Ast_Specifier* specifier = &ast_type->specifiers[i];

		switch (specifier->kind) {
			case AST_SPECIFIER_POINTER:     result = GetPointer(result);  break;
			case AST_SPECIFIER_OPTIONAL:    result = GetOptional(result); break;
			case AST_SPECIFIER_ARRAY:       result = GetArray(result);    break;

			case AST_SPECIFIER_FIXED_ARRAY: {
				ScanExpression(specifier->size_expression, scope, module);

				if (!(specifier->size_expression->flags & AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE))
					Error(module, specifier->size_expression, "Fixed array size must be a constant.\n");

				if (!CanCast(CAST_IMPLICIT, specifier->size_expression->type, TYPE_INT64))
					Error(module, specifier->size_expression, "Fixed array size must be an integer.\n");

				// @RemoveMe @Todo: Need to interpret size_expression
				Assert(specifier->size_expression->kind == AST_EXPRESSION_TERMINAL_LITERAL);

				Ast_Expression_Literal* literal = (Ast_Expression_Literal*)specifier->size_expression;
				u64 length = literal->token->literal_int;

				if (length <= 0) // Shouldn't we just enforce uint?
					Error(module, specifier->size_expression, "Fixed array size must be larger than 0.\n");

				result = GetFixedArray(result, length);
			} break;
		}
	}

	return result;
}

static void Intrinsic_SystemCall(s64* input, s64* output) {
	*output = SystemCall(input[0], input[1], input[2], input[3], input[4], input[5], input[6]);
}

typedef void (*InternalIntrinsicFunctionType)(void*,void*);

static InternalIntrinsicFunctionType intrinsic_function_implementation_lut[INTRINSIC_COUNT] = {
	[INTRINSIC_SYSTEM_CALL] = (InternalIntrinsicFunctionType)&Intrinsic_SystemCall,
};

static TypeID intrinsic_type_lut[INTRINSIC_COUNT];

static const String intrinsic_name_lut[INTRINSIC_COUNT] = {
	[INTRINSIC_SYSTEM_CALL] = "SystemCall",
};

static void InitIntrinsics() {
	intrinsic_type_lut[INTRINSIC_SYSTEM_CALL] = GetFunctionType(
		GetTuple((TypeID[]){
			TYPE_INT64, // rax
			TYPE_INT64, // rdi
			TYPE_INT64, // rsi
			TYPE_INT64, // rdx
			TYPE_INT64, // r10
			TYPE_INT64, // r8
			TYPE_INT64, // r9
		}, 7),
		TYPE_INT64
	);
}

static IntrinsicID FindIntrinsic(String name, TypeID input_type) {
	for (u64 id = 0; id < INTRINSIC_COUNT; id++) {
		TypeInfo* info = GetTypeInfo(intrinsic_type_lut[id]);

		if (name == intrinsic_name_lut[id] && CanCast(CAST_IMPLICIT, input_type, info->function_info.input))
			return (IntrinsicID)id;
	}

	return INTRINSIC_INVALID;
}

static Ast_Function* FindFunction(Ast_Scope* scope, String name, TypeID input_type) {
	while (scope) {
		for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++) {
			TypeInfo* info = GetTypeInfo(function->type);

			if (name == function->name && CanCast(CAST_IMPLICIT, input_type, info->function_info.input))
				return function;
		}

		scope = scope->parent;
	}

	return null;
}

static Ast_Variable* FindVariable(Ast_Scope* scope, String name) {
	while (scope) {
		for (u32 i = 0; i < scope->variables.count; i++) {
			Ast_Variable* variable = scope->variables[i];

			if (name == variable->name)
				return variable;
		}

		scope = scope->parent;
	}

	return null;
}

static Ast_Struct_Member* FindStructMember(Ast_Struct* ast_struct, String name) {
	for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
		if (name == member->name)
			return member;
	}

	return null;
}

static Ast_Enum_Member* FindEnumMember(Ast_Enum* ast_enum, String name) {
	for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++) {
		if (name == member->name)
			return member;
	}

	return null;
}

static Ast_Expression* ImplicitCast(Ast_Expression* expression, TypeID type, Ast_Module* module) {
	Assert(CanCast(CAST_IMPLICIT, expression->type, type));

	if (expression->type == type)
		return expression;

	Ast_Expression_Implicit_Cast* cast = module->stack.Allocate<Ast_Expression_Implicit_Cast>();
	cast->kind = AST_EXPRESSION_IMPLICIT_CAST;
	cast->subexpression = expression;
	cast->type  = type;
	cast->flags = expression->flags & (AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE | AST_EXPRESSION_FLAG_PURE);
	cast->begin = expression->begin;
	cast->end   = expression->end;

	return cast;
}

static void ScanExpressionTerminalName(Ast_Expression_Terminal* terminal, Ast_Scope* scope, Ast_Module* module) {
	if (terminal->token->kind == TOKEN_IDENTIFIER_CASUAL) {
		Ast_Variable* variable = FindVariable(scope, terminal->token->identifier_string);

		if (!variable)
			Error(module, terminal->token->location, "Variable with name '%' does not exist.\n", terminal->token);

		Ast_Expression_Variable* variable_expression = (Ast_Expression_Variable*)terminal;
		variable_expression->variable = variable;

		variable_expression->kind = AST_EXPRESSION_TERMINAL_VARIABLE;
		variable_expression->type = variable->type;

		variable_expression->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_REFERENTIAL;

		if (variable->flags & AST_VARIABLE_FLAG_CONSTANT) {
			variable_expression->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;
		}
		else if (variable->flags & AST_VARIABLE_FLAG_GLOBAL) {
			variable_expression->flags = AST_EXPRESSION_FLAG_REFERENTIAL;
		}
	}
	else if (terminal->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		TypeID type = FindUserType(terminal->token->identifier_string, scope);

		if (!type)
			Error(module, terminal->token->location, "User type with name '%' does not exist.\n", terminal->token);

		terminal->type = type;

		if (GetTypeKind(type) == TYPE_STRUCT) {
			terminal->kind = AST_EXPRESSION_TERMINAL_STRUCT; // @FixMe
			Ast_Expression_Struct* struct_terminal = (Ast_Expression_Struct*)terminal;
			struct_terminal->structure = GetTypeInfo(type)->struct_info.ast;
		}
		else if (GetTypeKind(type) == TYPE_ENUM) {
			terminal->kind = AST_EXPRESSION_TERMINAL_ENUM;
			Ast_Expression_Enum* enum_terminal = (Ast_Expression_Enum*)terminal;
			enum_terminal->enumeration = GetTypeInfo(type)->enum_info.ast;
		}
		else if (GetTypeKind(type) == TYPE_PRIMITIVE) {
			terminal->kind = AST_EXPRESSION_TERMINAL_PRIMITIVE; // @FixMe
		}
	}
	else {
		Assert();
		Error(module, terminal->token->location, "Unknown variable '%'\n", terminal->token);
	}
}

static void ScanExpressionFixedArray(Ast_Expression_Fixed_Array* fixed_array, Ast_Scope* scope, Ast_Module* module) {
	TypeID subtype = TYPE_NULL;

	Ast_Expression_Flags flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

	for (u32 i = 0; i < fixed_array->elements.length; i++) {
		Ast_Expression* element = fixed_array->elements[i];
		ScanExpression(element, scope, module);

		flags &= element->flags;

		if (i) {
			if (!CanCast(CAST_IMPLICIT, element->type, subtype))
				Error(module, element, "Cannot implicitly cast type '%' to '%'\n", subtype, element->type); // @FixMe

			fixed_array->elements[i] = ImplicitCast(element, subtype, module);
		}
		else {
			subtype = element->type;
			fixed_array->type = GetFixedArray(subtype, fixed_array->elements.length);
		}
	}

	fixed_array->flags = flags;
}

static void ScanExpressionArray(Ast_Expression_Array* array, Ast_Scope* scope, Ast_Module* module) {
	ScanExpression(array->left, scope, module);
	ScanExpression(array->right, scope, module);

	array->flags = (array->left->flags & array->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

	if (GetTypeKind(array->left->type) != TYPE_POINTER)
		Error(module, array, "Begin expression must be a pointer type, not: %\n", array->left->type);

	if (GetTypeKind(array->right->type) == TYPE_POINTER) {
		if (!CanCast(CAST_IMPLICIT, array->right->type, array->left->type))
			Error(module, array->right, "Array begin and end types are incompatible: [%..%]\n", array->left->type, array->right->type);

		array->right = ImplicitCast(array->right, array->left->type, module);
	}
	else if (CanCast(CAST_IMPLICIT, array->right->type, TYPE_UINT64)) {
		array->right = ImplicitCast(array->right, TYPE_UINT64, module);
	}
	else {
		Error(module, array->right, "Array end must be a % or an uint, not: %\n", array->left->type, array->right->type);
	}

	array->type = GetArray(GetSubType(array->left->type));
}

static void ScanExpressionLiteral(Ast_Expression_Literal* literal, Ast_Scope* scope, Ast_Module* module) {
	literal->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

	switch (literal->token->kind) {
		case TOKEN_LITERAL_INT:    literal->type = TYPE_INT64;  literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_INT8:   literal->type = TYPE_INT8;   literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_INT16:  literal->type = TYPE_INT16;  literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_INT32:  literal->type = TYPE_INT32;  literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_INT64:  literal->type = TYPE_INT64;  literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_UINT:   literal->type = TYPE_UINT64; literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_UINT8:  literal->type = TYPE_UINT8;  literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_UINT16: literal->type = TYPE_UINT16; literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_UINT32: literal->type = TYPE_UINT32; literal->value_int = literal->token->literal_int; break;
		case TOKEN_LITERAL_UINT64: literal->type = TYPE_UINT64; literal->value_int = literal->token->literal_int; break;

		case TOKEN_LITERAL_FLOAT: {
			literal->type = TYPE_FLOAT32; // @Fixme?
			literal->value_f32 = literal->token->literal_float;
		} break;

		case TOKEN_LITERAL_FLOAT32: {
			literal->type = TYPE_FLOAT32;
			literal->value_f32 = literal->token->literal_float;
		} break;

		case TOKEN_LITERAL_FLOAT64: {
			literal->type = TYPE_FLOAT64;
			literal->value_f64 = literal->token->literal_float;
		} break;

		case TOKEN_TRUE:  literal->type = TYPE_BOOL; literal->value_int = 1; break;
		case TOKEN_FALSE: literal->type = TYPE_BOOL; literal->value_int = 0; break;
		case TOKEN_NULL:  literal->type = GetPointer(TYPE_BYTE); literal->value_int = 0; break;

		case TOKEN_LITERAL_STRING: {
			literal->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;
			literal->type = GetFixedArray(TYPE_UINT8, literal->token->literal_string.length);
		} break;

		default: Assert(); Unreachable();
	}
}

static void ScanExpressionTuple(Ast_Expression_Tuple* tuple, Ast_Scope* scope, Ast_Module* module) {
	tuple->recursive_count = tuple->elements.length;

	TypeID types[tuple->elements.length];

	Ast_Expression_Flags element_flags =
		AST_EXPRESSION_FLAG_REFERENTIAL |
		AST_EXPRESSION_FLAG_PURE |
		AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

	Ast_Expression_Flags tuple_flags =
		AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL |
		AST_EXPRESSION_FLAG_PURE |
		AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

	for (u32 i = 0; i < tuple->elements.length; i++) {
		Ast_Expression* element = tuple->elements[i];
		ScanExpression(element, scope, module);
		types[i] = element->type;

		if (element->kind == AST_EXPRESSION_TUPLE) {
			tuple->recursive_count += ((Ast_Expression_Tuple*)element)->recursive_count-1;
			tuple_flags &= element->flags;
		}
		else {
			element_flags &= element->flags;
		}

		if (element->type == TYPE_EMPTY_TUPLE && (element->kind != AST_EXPRESSION_TUPLE || tuple->elements.length > 1))
			Error(module, element, "Tuple elements aren't allowed to be of type %.\n", element->type);
	}

	tuple->flags = element_flags & tuple_flags;

	if (element_flags & AST_EXPRESSION_FLAG_REFERENTIAL) {
		tuple->flags |= (tuple_flags & AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL);
	}

	tuple->type = GetTuple({ types, tuple->elements.length });
}

static void ScanExpressionCall(Ast_Expression_Call* call, Ast_Scope* scope, Ast_Module* module) {
	Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)call->function;
	Ast_Expression_Binary* dot = (Ast_Expression_Binary*)call->function;

	ScanExpression(call->parameters, scope, module);
	Print("Parameters: %\n", call->parameters->type);

	if (dot->kind == AST_EXPRESSION_BINARY_DOT && dot->right->kind == AST_EXPRESSION_TERMINAL_NAME && ((Ast_Expression_Terminal*)dot->right)->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		Ast_Expression_Dot_Call* dcall = (Ast_Expression_Dot_Call*)call;
		dcall->kind = AST_EXPRESSION_DOT_CALL;

		Ast_Expression_Function* function_expression = (Ast_Expression_Function*)dot->right;

		ScanExpression(dot->left, scope, module);

		if (dot->left->type == TYPE_EMPTY_TUPLE)
			Error(module, dot->left, "Cannot call into an empty tuple.\n");

		TypeID full_param_type = MergeTypeRight(dot->left->type, dcall->parameters->type);
		Ast_Function* function = FindFunction(scope, function_expression->token->identifier_string, full_param_type);

		if (!function)
			Error(module, dcall, "No function exists called % with input type: %.\n", function_expression->token->identifier_string, full_param_type);

		function_expression->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
		function_expression->function = function;
		function_expression->type = function->type;

		dcall->type = function->return_type;
		dcall->flags = 0;

		Assert(dcall->parameters->elements.length == function->parameters.length-1);

		dot->left = ImplicitCast(dot->left, function->parameters[0].type, module);

		for (u32 i = 0; i < dcall->parameters->elements.length; i++)
			call->parameters->elements[i] = ImplicitCast(call->parameters->elements[i], function->parameters[i+1].type, module);

		return;
	}

	if (call->function->kind == AST_EXPRESSION_TERMINAL_NAME && terminal->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		if (Ast_Function* function = FindFunction(scope, terminal->token->identifier_string, call->parameters->type); function) {
			Ast_Expression_Function* function_expression = (Ast_Expression_Function*)call->function;
			call->function->kind = AST_EXPRESSION_TERMINAL_FUNCTION;
			function_expression->function = function;
			function_expression->type = function->type;

			call->type = function->return_type;
			call->flags = 0;

			TypeInfo* info = GetTypeInfo(function->type);

			call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, info->function_info.input, module);
			return;
		}

		if (IntrinsicID intrinsic_id = FindIntrinsic(terminal->token->identifier_string, call->parameters->type); intrinsic_id != INTRINSIC_INVALID) {
			TypeID type = intrinsic_type_lut[intrinsic_id];
			TypeInfo* type_info = GetTypeInfo(type);

			Ast_Expression_Intrinsic* intrinsic_expression = (Ast_Expression_Intrinsic*)call->function;
			intrinsic_expression->intrinsic = intrinsic_id;
			intrinsic_expression->type = type;

			call->function->kind = AST_EXPRESSION_TERMINAL_INTRINSIC;
			call->type = type_info->function_info.output;
			call->flags = 0;

			call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, type_info->function_info.input, module);
			return;
		}

		Error(module, terminal->token->location, "No function exists called % with input type: %.\n", terminal->token->identifier_string, call->parameters->type);
		return;
	}

	ScanExpression(call->function, scope, module);

	if (!IsFunctionPointer(call->function->type))
		Error(module, call->function, "Expression of type % cannot be called like a function.\n", call->function->type);

	TypeID function_type = GetSubType(call->function->type);
	TypeInfo* function_type_info = GetTypeInfo(function_type);
	call->flags = 0;
	call->type = function_type_info->function_info.output;

	if (!CanCast(CAST_IMPLICIT, call->parameters->type, GetFunctionInputType(function_type)))
		Error(module, call->function, "Function of type % called with invalid arguments: %.\n", call->function->type, call->parameters->type);

	call->parameters = (Ast_Expression_Tuple*)ImplicitCast(call->parameters, function_type_info->function_info.input, module);
}

static void ScanExpressionSubscript(Ast_Expression_Subscript* subscript, Ast_Scope* scope, Ast_Module* module) {
	Ast_Expression* array = subscript->array;

	ScanExpression(subscript->array, scope, module);
	ScanExpression(subscript->index, scope, module);

	bool fixed = GetTypeKind(subscript->array->type) == TYPE_FIXED_ARRAY;

	if (GetTypeKind(subscript->array->type) != TYPE_FIXED_ARRAY &&
		GetTypeKind(subscript->array->type) != TYPE_ARRAY &&
		GetTypeKind(subscript->array->type) != TYPE_POINTER)
		Error(module, subscript->array, "Expression with type % is not a valid array.\n", subscript->array->type);

	if (!CanCast(CAST_IMPLICIT, subscript->index->type, TYPE_INT64))
		Error(module, subscript->index, "Subscript index must be an integer, not: %\n", subscript->index->type);

	subscript->index = ImplicitCast(subscript->index, TYPE_INT64, module);
	subscript->flags = subscript->array->flags & subscript->index->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
	subscript->flags |= subscript->array->flags & AST_EXPRESSION_FLAG_REFERENTIAL;

	if (!fixed)
		subscript->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;

	subscript->type = GetSubType(subscript->array->type);
}

static void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Ast_Module* module) {
	switch (expression->kind) {
		case AST_EXPRESSION_TERMINAL_NAME:    ScanExpressionTerminalName((Ast_Expression_Terminal*)expression,  scope, module); break;
		case AST_EXPRESSION_FIXED_ARRAY:      ScanExpressionFixedArray((Ast_Expression_Fixed_Array*)expression, scope, module); break;
		case AST_EXPRESSION_ARRAY:            ScanExpressionArray((Ast_Expression_Array*)expression,            scope, module); break;
		case AST_EXPRESSION_TERMINAL_LITERAL: ScanExpressionLiteral((Ast_Expression_Literal*)expression,        scope, module); break;
		case AST_EXPRESSION_TUPLE:            ScanExpressionTuple((Ast_Expression_Tuple*)expression,            scope, module); break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);

			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!(unary->subexpression->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
				Error(module, unary, "Cannot take address of a non-referential value.\n");

			unary->type = GetPointer(unary->subexpression->type);
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);

			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
			unary->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;

			if (GetTypeKind(unary->subexpression->type) != TYPE_POINTER)
				Error(module, unary, "Cannot take reference of type: %\n", unary->subexpression->type);

			unary->type = GetSubType(unary->subexpression->type);
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (IsInteger(GetArithmeticBackingType(unary->subexpression->type))) {
				unary->subexpression = ImplicitCast(unary->subexpression, GetArithmeticBackingType(unary->subexpression->type), module);
				unary->type = unary->subexpression->type;
			}
			else if (GetTypeKind(unary->subexpression->type) == TYPE_POINTER) {
				unary->type = unary->subexpression->type;
			}
			else Error(module, unary->subexpression, "Type % is not an integer or pointer.\n", unary->subexpression->type);
		} break;

		case AST_EXPRESSION_UNARY_MINUS: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!IsInteger(GetArithmeticBackingType(unary->subexpression->type)) && GetTypeKind(unary->subexpression->type) != TYPE_POINTER && !IsFloat(unary->subexpression->type))
				Error(module, unary->subexpression, "Unary minus does not work on type '%'.\n", unary->subexpression->type);

			unary->subexpression = ImplicitCast(unary->subexpression, GetArithmeticBackingType(unary->subexpression->type), module);
			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_PLUS: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!IsSignedInteger(unary->subexpression->type) && !IsFloat(unary->subexpression->type))
				Error(module, unary->subexpression, "Unary plus can only be applied to a signed integer or float.\n", unary->subexpression->type);

			unary->type = unary->subexpression->type;
		} break;

		case AST_EXPRESSION_UNARY_NOT: {
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			ScanExpression(unary->subexpression, scope, module);
			unary->flags = unary->subexpression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!CanCast(CAST_IMPLICIT, unary->subexpression->type, TYPE_BOOL))
				Error(module, unary->subexpression, "Type % cannot be casted to bool.\n", unary->subexpression->type);

			unary->subexpression = ImplicitCast(unary->subexpression, TYPE_BOOL, module);
			unary->type = TYPE_BOOL;
		} break;

		case AST_EXPRESSION_BINARY_DOT: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);

			TypeID type = binary->left->type;

			if (type == TYPE_EMPTY_TUPLE)
				Error(module, binary->left, "Cannot dot into empty tuple.\n");

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM) {
				Ast_Enum* ast_enum = ((Ast_Expression_Enum*)binary->left)->enumeration;

				if (binary->right->kind != AST_EXPRESSION_TERMINAL_NAME)
					Error(module, binary->right, "Expected enum member name.\n");

				binary->right->kind = AST_EXPRESSION_TERMINAL_ENUM_MEMBER;

				Ast_Expression_Enum_Member* member_terminal = (Ast_Expression_Enum_Member*)binary->right;
				String name = member_terminal->token->identifier_string;

				Ast_Enum_Member* member = FindEnumMember(ast_enum, name);

				if (!member)
					Error(module, binary->right, "Enum % does not contain a member called \"%\".", ast_enum->name, name);

				member_terminal->member = member;
				member_terminal->type = type;
				member_terminal->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;

				binary->type = type;
				binary->flags = AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE;
			}
			else if (binary->right->kind == AST_EXPRESSION_TERMINAL_NAME && ((Ast_Expression_Terminal*)binary->right)->token->kind == TOKEN_IDENTIFIER_CASUAL) {
				Ast_Expression_Terminal* terminal = (Ast_Expression_Terminal*)binary->right;
				String name = terminal->token->identifier_string;

				while (GetTypeKind(type) == TYPE_POINTER) type = GetSubType(type);

				if (GetTypeKind(type) == TYPE_STRUCT) {
					binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE | AST_EXPRESSION_FLAG_REFERENTIAL);

					if (GetTypeKind(binary->left->type) == TYPE_POINTER) {
						binary->flags |= AST_EXPRESSION_FLAG_REFERENTIAL;
					}

					Ast_Struct* ast_struct = GetTypeInfo(type)->struct_info.ast;
					Ast_Struct_Member* member = FindStructMember(ast_struct, terminal->token->identifier_string);

					if (!member)
						Error(module, binary, "Struct % does not have a member named %\n", ast_struct->name, terminal->token);

					binary->type = member->type;
					terminal->kind = AST_EXPRESSION_TERMINAL_STRUCT_MEMBER;
					((Ast_Expression_Struct_Member*)terminal)->member = member;
					terminal->type = member->type;
				}
				else if (GetTypeKind(type) == TYPE_ARRAY || GetTypeKind(type) == TYPE_FIXED_ARRAY) {
					bool fixed = GetTypeKind(type) == TYPE_FIXED_ARRAY;

					if (name == "begin" || name == "data") {
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_BEGIN;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

						if (!fixed) {
							binary->flags |= binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL;
						}

						binary->type = GetPointer(GetSubType(binary->left->type));
					}
					else if (name == "end") {
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_END;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);
						binary->type = GetPointer(GetSubType(binary->left->type));
					}
					else if (name == "length" || name == "count") {
						binary->right->kind = AST_EXPRESSION_TERMINAL_ARRAY_LENGTH;
						binary->flags = binary->left->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

						if (!fixed) {
							binary->flags |= binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL;
						}

						binary->type = TYPE_UINT64;
					}
					else Error(module, binary->right, "'%' does not have a member named '%'.\n", type, name);
				}
				else Error(module, binary, "'%' does not have a member named '%'.\n", type, name);
			}
			else Error(module, binary, "Invalid dot expression.\n", binary->left->type);
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);

			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

			if (!dominant)
				Error(module, binary, "% and % are incompatible types.\n", binary->left->type, binary->right->type);

			binary->left  = ImplicitCast(binary->left,  dominant, module);
			binary->right = ImplicitCast(binary->right, dominant, module);

			binary->type = TYPE_BOOL;
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			TypeID dominant = GetDominantType(GetArithmeticBackingType(binary->left->type), GetArithmeticBackingType(binary->right->type));

			if (!dominant)
				Error(module, binary, "Incompatible types % and %\n", binary->left->type, binary->right->type);

			if (!IsInteger(dominant) && !IsFloat(dominant) && GetTypeKind(dominant) != TYPE_POINTER)
				Error(module, binary, "Cannot compare types '%' and '%'.\n", binary->left->type, binary->right->type);

			binary->left  = ImplicitCast(binary->left,  dominant, module);
			binary->right = ImplicitCast(binary->right, dominant, module);

			binary->type = TYPE_BOOL;
		} break;

		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (GetTypeKind(binary->left->type) == TYPE_POINTER) {
				// ptr + int = ptr
				// ptr - int = ptr
				// ptr - ptr = int

				if (binary->kind == AST_EXPRESSION_BINARY_ADD) {
					if (!CanCast(CAST_IMPLICIT, binary->right->type, TYPE_INT64))
						Error(module, binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

					binary->right = ImplicitCast(binary->right, TYPE_INT64, module);
					binary->type = binary->left->type;
				}
				else if (binary->kind == AST_EXPRESSION_BINARY_SUBTRACT) {
					if (GetTypeKind(binary->right->type) == TYPE_POINTER) {
						TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

						if (!dominant)
							Error(module, binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

						binary->left = ImplicitCast(binary->left, dominant, module);
						binary->right = ImplicitCast(binary->right, dominant, module);

						binary->type = TYPE_INT64;
					}
					else if (CanCast(CAST_IMPLICIT, binary->right->type, TYPE_INT64)) {
						binary->right = ImplicitCast(binary->right, TYPE_INT64, module);
						binary->type = binary->left->type;
					}
					else
						Error(module, binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
				}
				else
					Error(module, binary, "Binary expression '%' cannot be used on a pointer.\n", binary->op);
			}
			else if (IsFloat(binary->left->type) || IsFloat(binary->right->type)) {
				// float + float = float
				// float - float = float
				// float * float = float
				// float / float = float

				TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

				if (!dominant || !IsFloat(dominant) || binary->kind == AST_EXPRESSION_BINARY_MODULO)
					Error(module, binary, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

				binary->left  = ImplicitCast(binary->left,  dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = dominant;
			}
			else {
				// int + int
				// int - int
				// int * int
				// int / int
				// int % int

				TypeID dominant = GetDominantType(GetArithmeticBackingType(binary->left->type), GetArithmeticBackingType(binary->right->type));

				if (!dominant || !IsInteger(dominant))
					Error(module, binary, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

				binary->left = ImplicitCast(binary->left, dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = dominant;
			}
		} break;

		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			// ptr OR  int = ptr
			// ptr XOR int = ptr
			// ptr AND int = int

			if (!IsInteger(GetArithmeticBackingType(binary->left->type)) && GetTypeKind(binary->left->type) != TYPE_POINTER)
				Error(module, binary, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);

			if (!IsInteger(GetArithmeticBackingType(binary->right->type)) && GetTypeKind(binary->right->type) != TYPE_POINTER)
				Error(module, binary, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);

			binary->type = binary->left->type;
		} break;

		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!IsInteger(GetArithmeticBackingType(binary->left->type)) && GetTypeKind(binary->left->type) != TYPE_POINTER)
				Error(module, binary, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);

			if (!IsInteger(GetArithmeticBackingType(binary->right->type)))
				Error(module, binary, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);

			binary->left  = ImplicitCast(binary->left,  GetArithmeticBackingType(binary->left->type),  module);
			binary->right = ImplicitCast(binary->right, GetArithmeticBackingType(binary->right->type), module);

			binary->type = binary->left->type;
		} break;


		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR: {
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			ScanExpression(binary->left,  scope, module);
			ScanExpression(binary->right, scope, module);
			binary->flags = (binary->left->flags & binary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!CanCast(CAST_IMPLICIT, binary->left->type, TYPE_BOOL))
				Error(module, binary, "% cannot be converted to bool.\n", binary->left->type);

			if (!CanCast(CAST_IMPLICIT, binary->right->type, TYPE_BOOL))
				Error(module, binary, "% cannot be converted to bool.\n", binary->right->type);

			binary->left = ImplicitCast(binary->left, TYPE_BOOL, module);
			binary->right = ImplicitCast(binary->right, TYPE_BOOL, module);

			binary->type = TYPE_BOOL;
		} break;

		case AST_EXPRESSION_IF_ELSE: {
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;
			ScanExpression(ternary->left,   scope, module);
			ScanExpression(ternary->middle, scope, module);
			ScanExpression(ternary->right,  scope, module);
			ternary->flags = (ternary->left->flags & ternary->middle->flags & ternary->right->flags) & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			TypeID dominant = GetDominantType(ternary->left->type, ternary->right->type);

			if (!dominant)
				Error(module, ternary->left, "Types '%' and '%' are incompatible.\n", ternary->left->type, ternary->right->type);

			if (!CanCast(CAST_IMPLICIT, ternary->middle->type, TYPE_BOOL))
				Error(module, ternary->middle, "Type % not convertable to bool\n", ternary->middle->type);

			ternary->middle = ImplicitCast(ternary->middle, TYPE_BOOL, module);
			ternary->left   = ImplicitCast(ternary->left, dominant, module);
			ternary->right  = ImplicitCast(ternary->right, dominant, module);

			if (ternary->left->type == ternary->right->type)
				ternary->flags |= ternary->left->flags & ternary->right->flags & AST_EXPRESSION_FLAG_REFERENTIAL;

			ternary->type = dominant;
		} break;

		case AST_EXPRESSION_CALL: ScanExpressionCall((Ast_Expression_Call*)expression, scope, module); break;
		
		case AST_EXPRESSION_LAMBDA: {
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT: ScanExpressionSubscript((Ast_Expression_Subscript*)expression, scope, module); break;

		case AST_EXPRESSION_AS: {
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			ScanExpression(as->expression, scope, module);
			as->type = GetType(&as->ast_type, scope, module);

			as->flags = as->expression->flags & (AST_EXPRESSION_FLAG_PURE | AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE);

			if (!CanCast(CAST_EXPLICIT, as->expression->type, as->type))
				Error(module, as, "Type % is not convertable to %\n", as->expression->type, as->type);
		} break;

		default:
			AssertUnreachable();
	}

	Assert(expression);
}

static void GenerateClosure(Ast_Struct* target, Ast_Struct* ast_struct);
static void GenerateClosure(Ast_Struct* target, Array<TypeID> tuple);

static void GenerateClosure(Ast_Struct* target, Array<TypeID> tuple) {
	for (u32 i = 0; i < tuple.length; i++) {
		TypeID type = tuple[i];
		TypeInfo* type_info = GetTypeInfo(type);

		if (GetTypeKind(type) == TYPE_STRUCT) {
			if (target->closure.AddIfUnique(GetTypeInfo(type)->struct_info.ast)) {
				GenerateClosure(target, GetTypeInfo(type)->struct_info.ast);
			}
		}
		else if (GetTypeKind(type) == TYPE_TUPLE) {
			GenerateClosure(target, type_info->tuple_info.elements);
		}
	}
}

static void GenerateClosure(Ast_Struct* target, Ast_Struct* ast_struct) {
	for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
		TypeID type = member->type;
		TypeInfo* type_info = GetTypeInfo(type);

		if (GetTypeKind(type) == TYPE_STRUCT) {
			if (target->closure.AddIfUnique(GetTypeInfo(type)->struct_info.ast)) {
				GenerateClosure(target, GetTypeInfo(type)->struct_info.ast);
			}
		}
		else if (GetTypeKind(type) == TYPE_TUPLE) {
			GenerateClosure(target, type_info->tuple_info.elements);
		}
	}
}

static void CheckForCircularDependencies(Ast_Struct* ast_struct, Ast_Module* module) {
	GenerateClosure(ast_struct, ast_struct);

	if (ast_struct->closure.Contains(ast_struct))
		Error(module, ast_struct->name_token->location, "The closure of struct '%' contains '%' (circularly dependent)\n", ast_struct->name, ast_struct->name);
}

static void CalculateStructSize(Ast_Struct* ast_struct);
static void CalculateTupleSize(TypeID tuple);

static void CalculateTupleSize(TypeID tuple) {
	TypeInfo* info = GetTypeInfo(tuple);

	if (info->size) return;

	u64 size = 0;

	Assert(info->tuple_info.elements.length);
	for (TypeID element_type : info->tuple_info.elements) {

		if (GetTypeKind(element_type) == TYPE_STRUCT) {
			CalculateStructSize(GetTypeInfo(element_type)->struct_info.ast);
		}
		else if (GetTypeKind(element_type) == TYPE_TUPLE) {
			CalculateTupleSize(element_type);
		}

		size += GetTypeSize(element_type);
	}

	info->size = size;
}

static void CalculateStructSize(Ast_Struct* ast_struct) {
	if (GetTypeSize(ast_struct->type)) return;
	TypeInfo* info = GetTypeInfo(ast_struct->type);

	u64 size = 0;

	for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
		TypeID type = member->type;

		if (GetTypeKind(type) == TYPE_STRUCT) {
			CalculateStructSize(GetTypeInfo(type)->struct_info.ast);
		}
		else if (GetTypeKind(type) == TYPE_TUPLE) {
			CalculateTupleSize(type);
		}

		member->offset = size;
		size += GetTypeSize(type);
	}

	info->size = size;
}

static void CheckForEnumMemberDiplicates(Ast_Module* module, Ast_Enum* ast) {
	for (Ast_Enum_Member* m0 = ast->members; m0 < ast->members.End(); m0++) {
		for (Ast_Enum_Member* m1 = ast->members; m1 < m0; m1++) {
			if (m0->name == m1->name)
				Error(module, m0->name_token->location, "Duplicate member called '%' in enum %\n", m0->name, ast->name);
		}
	}
}

static void CheckForStructMemberDuplicates(Ast_Module* module, Ast_Struct* ast) {
	for (Ast_Struct_Member* m0 = ast->members; m0 < ast->members.End(); m0++) {
		for (Ast_Struct_Member* m1 = ast->members; m1 < m0; m1++) {
			if (m0->name == m1->name)
				Error(module, m0->name_token->location, "Duplicate member called '%' in struct %\n", m0->name, ast->name);
		}
	}
}

// @Yuck: This function is *disgusting*, all of this should be implicit when we try to use a type for the first time or something... Also, what about multi-threading?
static void ScanScope(Ast_Scope* scope, Ast_Module* module) {
	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		ast_struct->type = CreateStructType(ast_struct, 0);
		TypeInfo* type_info = GetTypeInfo(ast_struct->type);

		for (Ast_Struct* other = scope->structs; other < ast_struct; other++) {
			if (ast_struct->name == other->name)
				Error(module, ast_struct->name_token->location, "Duplicate struct called '%'\n", ast_struct->name);
		}

		for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
			if (ast_struct->name == ast_enum->name) {
				Token* name_token = ast_struct->name_token;

				if (ast_struct->name_token->location.line < ast_enum->name_token->location.line)
					name_token = ast_enum->name_token;

				Error(module, name_token->location, "Duplicate type called '%'\n", name_token);
			}
		}

		CheckForStructMemberDuplicates(module, ast_struct);
	}

	for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
		ZeroMemory(&ast_enum->type);
		ast_enum->underlying_type = TYPE_INT64;
		ast_enum->type = CreateEnumType(ast_enum, ast_enum->underlying_type);

		for (Ast_Enum* eo = scope->enums; eo < ast_enum; eo++) {
			if (ast_enum->name == eo->name)
				Error(module, ast_enum->name_token->location, "Duplicate enum called '%'\n", ast_enum->name);
		}

		CheckForEnumMemberDiplicates(module, ast_enum);
	}

	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		for (Ast_Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
			member->type = GetType(&member->ast_type, scope, module);

			if (!member->type)
				Error(module, member->ast_type.basetype.token->location, "Unknown type '%'\n", member->ast_type.basetype.token);
		}
	}

	for (Ast_Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		CheckForCircularDependencies(ast_struct, module);
		CalculateStructSize(ast_struct);
		// Print("Struct % has size = %\n", ast_struct->name, ast_struct->type.size);
	}

	for (Ast_Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
		for (Ast_Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++) {
			ScanExpression(member->expression, scope, module);
		}
	}

	for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++) {
		ScanFunction(function, scope, module);

		for (Ast_Function* other = scope->functions; other < function; other++) {
			if (function->type == other->type && function->name == other->name)
				Error(module, function->name_token->location, "Function '%' with type % already exists.\n", function->name, function->type);
		}
	}

	for (Ast_Function* function = scope->functions; function < scope->functions.End(); function++) {
		ScanCode(&function->code, scope, function, module);

		if (function->return_type != TYPE_EMPTY_TUPLE && !function->code.all_paths_return) {
			if (function->code.contains_return)
				Error(module, function->name_token->location, "Not all paths return a value.\n");
			else
				Error(module, function->name_token->location, "Function does not return a value.\n");
		}
	}
}

static bool IsAssignable(Ast_Expression* expression) {
	return expression->flags & (AST_EXPRESSION_FLAG_REFERENTIAL | AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL);
}

static bool DoesBranchAlwaysReturn(Ast_Branch* branch) {
	if (!branch)
		return false;

	bool true_path_returns = branch->code.all_paths_return || DoesBranchAlwaysReturn(branch->then_branch);

	if (branch->kind == AST_BRANCH_NAKED)
		return true_path_returns;

	return true_path_returns && DoesBranchAlwaysReturn(branch->else_branch);
}

static void ScanIf(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_Branch* branch) {
	ScanExpression(branch->if_condition, &code->scope, module);

	if (!CanCast(CAST_IMPLICIT, branch->if_condition->type, TYPE_BOOL))
		Error(module, branch->if_condition, "Cannot implicitly cast condition with type % to bool\n", branch->if_condition->type);

	branch->if_condition = ImplicitCast(branch->if_condition, TYPE_BOOL, module);

	branch->code.is_inside_loop = code->is_inside_loop;
	code->contains_break = code->contains_break || branch->code.contains_break;
}

static void ScanWhile(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_Branch* branch) {
	ScanExpression(branch->while_condition, &code->scope, module);

	if (!CanCast(CAST_IMPLICIT, branch->while_condition->type, TYPE_BOOL))
		Error(module, branch->while_condition, "Cannot implicitly cast condition with type % to bool\n", branch->while_condition->type);

	branch->while_condition = ImplicitCast(branch->while_condition, TYPE_BOOL, module);

	branch->code.is_inside_loop = true;
}


static void ScanRangeFor(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_Branch* branch) {
	ScanExpression(branch->for_range.range, &code->scope, module);
	branch->code.scope.variables.Add(branch->for_range.iterator); // for i in i:

	if (!IsArray(branch->for_range.range->type) && !IsFixedArray(branch->for_range.range->type))
		Error(module, branch->for_range.range, "For loop cannot range over type: %\n", branch->for_range.range->type);

	if (branch->for_range.filter) {
		ScanExpression(branch->for_range.filter, &branch->code.scope, module);

		if (!CanCast(CAST_IMPLICIT, branch->for_range.filter->type, TYPE_BOOL))
			Error(module, branch->for_range.filter, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->for_range.filter->type);

		branch->for_range.filter = ImplicitCast(branch->for_range.filter, TYPE_BOOL, module);
	}

	branch->code.is_inside_loop = true;
}

static void ScanVerboseFor(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_Branch* branch) {
	if (branch->for_verbose.variable->assignment) {
		ScanExpression(branch->for_verbose.variable->assignment, &code->scope, module);
		branch->for_verbose.variable->type = branch->for_verbose.variable->assignment->type;
	}

	if (branch->for_verbose.variable->ast_type) {
		branch->for_verbose.variable->type = GetType(branch->for_verbose.variable->ast_type, &code->scope, module);
	}

	if (branch->for_verbose.variable->ast_type && branch->for_verbose.variable->assignment) {
		if (!CanCast(CAST_IMPLICIT, branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type))
			Error(module, branch->for_verbose.variable->assignment, "Cannot assign expression with type % to variable with type %\n", branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type);

		branch->for_verbose.variable->assignment = ImplicitCast(branch->for_verbose.variable->assignment, branch->for_verbose.variable->type, module);
	}

	Assert(branch->code.scope.parent);
	branch->code.scope.variables.Add(branch->for_verbose.variable);

	ScanExpression(branch->for_verbose.condition, &branch->code.scope, module);

	if (!CanCast(CAST_IMPLICIT, branch->for_verbose.condition->type, TYPE_BOOL))
		Error(module, branch->for_verbose.condition, "For loop condition expression of type % cannot be implicitly casted to bool\n", branch->for_verbose.condition->type);

	branch->for_verbose.condition = ImplicitCast(branch->for_verbose.condition, TYPE_BOOL, module);

	if (branch->for_verbose.next) {
		ScanExpression(branch->for_verbose.next, &branch->code.scope, module);

		if (!CanCast(CAST_EXPLICIT, branch->for_verbose.next->type, branch->for_verbose.variable->type))
			Error(module, branch->for_verbose.next,
				"For loop stride expression of type % cannot be explicitly casted to type %\n", 
				branch->for_verbose.next->type, branch->for_verbose.variable->type
			);

		branch->for_verbose.next = ImplicitCast(branch->for_verbose.next, branch->for_verbose.variable->type, module);
	}

	branch->code.is_inside_loop = true;
}

static void ScanBranchBlock(Ast_Module* module, Ast_Function* function, Ast_Code* code, Ast_BranchBlock* branch_block) {
	for (u32 i = 0; i < branch_block->branches.length; i++) {
		Ast_Branch* branch = &branch_block->branches[i];

		branch->code.scope.parent = &code->scope; // @Hack

		switch (branch->kind) {
			case AST_BRANCH_NAKED: {
				branch->code.is_inside_loop = code->is_inside_loop;
				code->contains_break = code->contains_break || branch->code.contains_break;
			} break;

			case AST_BRANCH_IF:          ScanIf(module,         function, code, branch); break;
			case AST_BRANCH_WHILE:       ScanWhile(module,      function, code, branch); break;
			case AST_BRANCH_FOR_RANGE:   ScanRangeFor(module,   function, code, branch); break;
			case AST_BRANCH_FOR_VERBOSE: ScanVerboseFor(module, function, code, branch); break;
		}

		ScanCode(&branch->code, &code->scope, function, module);

		code->contains_return = code->contains_return || branch->code.contains_return;
		branch->code.has_defer_that_returns = code->has_defer_that_returns;
	}

	code->all_paths_return = DoesBranchAlwaysReturn(&branch_block->branches[0]);
}

static void ScanDefer(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	code->defers.Add(&statement->defer);
	statement->defer.code.is_inside_loop = code->is_inside_loop;

	ScanCode(&statement->defer.code, &code->scope, function, module);

	if (statement->defer.code.all_paths_return) {
		code->has_defer_that_returns = true;
		code->all_paths_return = true;
	}
}

static void ScanClaim(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	Ast_Claim* claim = &statement->claim;

	ScanExpression(claim->expression, &code->scope, module);

	if (!CanCast(CAST_IMPLICIT, claim->expression->type, TYPE_BOOL))
		Error(module, claim->expression, "Claim expression type must be implicitly castable to bool\n");

	claim->expression = ImplicitCast(claim->expression, TYPE_BOOL, module);
}

static void ScanIncDec(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	Ast_Increment* inc = &statement->increment;
	ScanExpression(inc->expression, &code->scope, module);

	bool direction = statement->kind == AST_STATEMENT_INCREMENT;

	if (!IsInteger(inc->expression->type) && !IsFloat(inc->expression->type) && GetTypeKind(inc->expression->type) != TYPE_POINTER)
		Error(module, inc->expression, "Cannot % type %, expression must be either an integer, float or pointer.\n", (direction ? "increment" : "decrement"), inc->expression->type);

	if (!(inc->expression->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
		Error(module, inc->expression, "Expression is not a referential value.\n");
}

static void ScanReturn(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	code->contains_return = true;
	code->all_paths_return = !code->contains_break;
	function->returns.Add(&statement->ret);

	if (code->has_defer_that_returns)
		Error(module, statement->ret.token->location, "A defer in this scope already has a return statement. This isn't allowed.\n");

	if (statement->ret.expression) {
		ScanExpression(statement->ret.expression, &code->scope, module);

		if (!function->return_type)
			Error(module, statement->ret.token->location, "Unexpected return value for function that doesn't return anything.\n");

		if (!CanCast(CAST_IMPLICIT, function->return_type, statement->ret.expression->type))
			Error(module, statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
	}
	else if (function->ast_return_type)
		Error(module, statement->ret.token->location, "Expected return value with type: %\n", function->return_type);
}

static void ScanBreak(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	code->contains_break = true;
	code->all_paths_break = !code->contains_return;

	if (!code->is_inside_loop)
		Error(module, statement->brk.token->location, "break statement must be inside of a loop.\n");
}

static void ScanVarDecl(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	Ast_Variable* var = &statement->variable_declaration;

	for (Ast_Variable** other_variable = code->scope.variables.Begin(); other_variable < code->scope.variables.End(); other_variable++) {
		if (var->name == (*other_variable)->name)
			Error(module, var->name_token->location, "Variable with name '%' already declared in this scope.\n", var->name);
	}

	if (var->assignment) {
		ScanExpression(var->assignment, &code->scope, module);

		if (!var->assignment->type)
			Error(module, var->assignment, "Expression does not have a type.\n");

		var->type = var->assignment->type;
	}

	if (var->ast_type) {
		var->type = GetType(var->ast_type, &code->scope, module);

		if (!var->type)
			Error(module, var->name_token->location, "Unknown type %\n", var->ast_type->basetype.token);

		if (var->assignment && !CanCast(CAST_IMPLICIT, var->assignment->type, var->type))
			Error(module, var->name_token->location, "Variable '%' with type % cannot be assign to type %\n", var->name, var->type, var->assignment->type);
	}

	Assert(var->type);

	if (var->type == TYPE_EMPTY_TUPLE)
		Error(module, var->assignment, "Cannot declare variable with type %\n", var->type);

	Assert(var->type != TYPE_EMPTY_TUPLE);

	code->scope.variables.Add(var);
}

static void ScanAssignment(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	Ast_Assignment* assignment = &statement->assignment;

	ScanExpression(assignment->right, &code->scope, module);
	ScanExpression(assignment->left,  &code->scope, module);

	if (!IsAssignable(assignment->left))
		Error(module, assignment->left, "Expression is not assignable.\n");

	if (!CanCast(CAST_IMPLICIT, assignment->right->type, assignment->left->type))
		Error(module, assignment->token->location, "Cannot cast % to %.\n", assignment->right->type, assignment->left->type);

	assignment->right = ImplicitCast(assignment->right, assignment->left->type, module);
}

static void ScanBinAssignment(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	Ast_Assignment* assignment = &statement->assignment;

	ScanExpression(assignment->right, &code->scope, module);
	ScanExpression(assignment->left,  &code->scope, module);

	if (!(assignment->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
		Error(module, assignment->left, "Expression is not assignable.\n");

	if (!IsInteger(assignment->left->type) &&
		!IsFloat(assignment->left->type) &&
		GetTypeKind(assignment->left->type) != TYPE_POINTER)
		Error(module, assignment->left, "Arithmetic assignment type must be to an integer, float or pointer, not: '%'\n", assignment->left->type);

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

	if (IsPointer(assignment->left->type)) {
		if (statement->kind != AST_STATEMENT_ASSIGNMENT_ADD && statement->kind != AST_STATEMENT_ASSIGNMENT_SUBTRACT)
			Error(module, assignment->left, "Arithmetic assignment to pointer only allows '+=' and '-='.\n");

		if (!CanCast(CAST_IMPLICIT, assignment->right->type, TYPE_INT64))
			Error(module, assignment->right, "Expression type must be an integer.\n");

		assignment->right = ImplicitCast(assignment->right, TYPE_INT64, module);
	}
	else {
		if (!CanCast(CAST_IMPLICIT, assignment->right->type, assignment->left->type))
			Error(module, assignment->right, "Cannot implicitly cast '%' to '%'.\n", assignment->right->type, assignment->left->type);

		assignment->right = ImplicitCast(assignment->right, assignment->left->type, module);
	}
}

static void ScanStatement(Ast_Statement* statement, Ast_Code* code, Ast_Function* function, Ast_Module* module) {
	switch (statement->kind) {
		case AST_STATEMENT_DEFER:                ScanDefer(statement,   code, function, module); break;
		case AST_STATEMENT_CLAIM:                ScanClaim(statement,   code, function, module); break;
		case AST_STATEMENT_INCREMENT:            ScanIncDec(statement,  code, function, module); break;
		case AST_STATEMENT_DECREMENT:            ScanIncDec(statement,  code, function, module); break;
		case AST_STATEMENT_RETURN:               ScanReturn(statement,  code, function, module); break;
		case AST_STATEMENT_BREAK:                ScanBreak(statement,   code, function, module); break;
		case AST_STATEMENT_VARIABLE_DECLARATION: ScanVarDecl(statement, code, function, module); break;

		case AST_STATEMENT_EXPRESSION:   ScanExpression(statement->expression, &code->scope, module); break;
		case AST_STATEMENT_BRANCH_BLOCK: ScanBranchBlock(module, function, code, &statement->branch_block); break;
		case AST_STATEMENT_ASSIGNMENT:   ScanAssignment(statement, code, function, module); break;

		case AST_STATEMENT_ASSIGNMENT_ADD:
		case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
		case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
		case AST_STATEMENT_ASSIGNMENT_DIVIDE:
		case AST_STATEMENT_ASSIGNMENT_XOR: ScanBinAssignment(statement, code, function, module); break;
	}
}

static void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Ast_Module* module) {
	code->scope.parent = scope;
	ScanScope(&code->scope, module);

	for (Ast_Statement* statement = code->statements.Begin(); statement < code->statements.End(); statement++) {
		ScanStatement(statement, code, function, module);
	}
}

static TypeID GetTypeFromParams(Array<Ast_Variable> params, Ast_Module* module) {
	TypeID types[params.length];
	for (u32 i = 0; i < params.length; i++) {
		types[i] = params[i].type;
	}

	return GetTuple(types, params.length);
}

static void ScanFunction(Ast_Function* function, Ast_Scope* scope, Ast_Module* module) {
	for (Ast_Variable* param = function->parameters; param < function->parameters.End(); param++) {
		param->type = GetType(param->ast_type, scope, module);
		function->code.scope.variables.Add(param);

		if (!param->type)
			Error(module, param->ast_type->basetype.token->location, "Unknown type '%'\n", param->ast_type->basetype.token);

		for (Ast_Variable* param_other = function->parameters; param_other < param; param_other++) {
			if (param_other->name == param->name)
				Error(module, param->name_token->location, "Duplicate parameter called '%'\n", param->name);
		}
	}

	if (function->ast_return_type) {
		function->return_type = GetType(function->ast_return_type, scope, module);

		if (!function->return_type)
			Error(module, function->ast_return_type->basetype.token->location, "Unknown type: %\n", function->ast_return_type);
	}
	else {
		function->return_type = TYPE_EMPTY_TUPLE;
	}

	TypeID param_type = GetTypeFromParams(function->parameters, module);
	function->type = GetFunctionType(param_type, function->return_type);
}

static void SemanticParse(Ast_Module* module) {
	ScanScope(&module->scope, module);
}


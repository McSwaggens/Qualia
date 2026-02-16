#include "semantic.h"
#include "alloc.h"
#include "print.h"
#include "assert.h"
#include "general.h"
#include "error.h"
#include "type_system.h"

static TypeID FindUserType(String name, Ast::Scope* scope) {
	while (scope) {
		for (Ast::Struct* ast = scope->structs; ast < scope->structs.End(); ast++) {
			if (name == ast->name)
				return ast->type;
		}

		for (Ast::Enum* ast = scope->enums; ast < scope->enums.End(); ast++) {
			if (name == ast->name)
				return ast->type;
		}

		scope = scope->parent;
	}
	return TYPE_NULL;
}

static Ast::Function* FindFunction(Ast::Scope* scope, String name, TypeID input_type) {
	while (scope) {
		for (Ast::Function* function = scope->functions; function < scope->functions.End(); function++) {
			TypeInfo* info = GetTypeInfo(function->type);

			if (name == function->name && CanCast(CAST_IMPLICIT, input_type, info->function_info.input))
				return function;
		}

		scope = scope->parent;
	}

	return null;
}

static Ast::Variable* FindVariable(Ast::Scope* scope, String name) {
	while (scope) {
		for (u32 i = 0; i < scope->variables.count; i++) {
			Ast::Variable* variable = scope->variables[i];

			if (name == variable->name)
				return variable;
		}

		scope = scope->parent;
	}

	return null;
}

static Ast::Struct_Member* FindStructMember(Ast::Struct* ast_struct, String name) {
	for (Ast::Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
		if (name == member->name)
			return member;
	}

	return null;
}

static Ast::Enum_Member* FindEnumMember(Ast::Enum* ast_enum, String name) {
	for (Ast::Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++) {
		if (name == member->name)
			return member;
	}

	return null;
}

static Ast::Expression* ImplicitCast(Ast::Expression* expression, TypeID type, Ast::Module* module) {
	Assert(CanCast(CAST_IMPLICIT, expression->type, type));

	if (expression->type == type)
		return expression;

	Ast::Expression_Implicit_Cast* cast = Alloc<Ast::Expression_Implicit_Cast>(); // @fixme using general allocator and not the stack. Proper Ast stack is in the parser. Which we don't have access to here.
	new (cast) Ast::Expression_Implicit_Cast(expression, type);

	return cast;
}

static bool IsAssignable(Ast::Expression* expression) {
	return IsReference(expression->type);
}

static bool DoesBranchAlwaysReturn(Ast::Branch* branch) {
	if (!branch)
		return false;

	bool true_path_returns = branch->code.all_paths_return || DoesBranchAlwaysReturn(branch->then_branch);

	if (branch->kind == Ast::BRANCH_NAKED)
		return true_path_returns;

	return true_path_returns && DoesBranchAlwaysReturn(branch->else_branch);
}

static void GenerateClosure(Ast::Struct* target, Ast::Struct* ast_struct);
static void GenerateClosure(Ast::Struct* target, Array<TypeID> tuple);

static void GenerateClosure(Ast::Struct* target, Array<TypeID> tuple) {
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

static void GenerateClosure(Ast::Struct* target, Ast::Struct* ast_struct) {
	for (Ast::Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
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

static void CheckForCircularDependencies(Ast::Struct* ast_struct, Ast::Module* module) {
	GenerateClosure(ast_struct, ast_struct);

	if (ast_struct->closure.Contains(ast_struct))
		Error(module, ast_struct->name_token->location, "The closure of struct '%' contains '%' (circularly dependent)\n", ast_struct->name, ast_struct->name);
}

static void CalculateStructSize(Ast::Struct* ast_struct);
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

static void CalculateStructSize(Ast::Struct* ast_struct) {
	if (GetTypeSize(ast_struct->type)) return;
	TypeInfo* info = GetTypeInfo(ast_struct->type);

	u64 size = 0;

	for (Ast::Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
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

static void CheckForEnumMemberDuplicates(Ast::Module* module, Ast::Enum* ast) {
	for (Ast::Enum_Member* m0 = ast->members; m0 < ast->members.End(); m0++) {
		for (Ast::Enum_Member* m1 = ast->members; m1 < m0; m1++) {
			if (m0->name == m1->name)
				Error(module, m0->name_token->location, "Duplicate member called '%' in enum %\n", m0->name, ast->name);
		}
	}
}

static void CheckForStructMemberDuplicates(Ast::Module* module, Ast::Struct* ast) {
	for (Ast::Struct_Member* m0 = ast->members; m0 < ast->members.End(); m0++) {
		for (Ast::Struct_Member* m1 = ast->members; m1 < m0; m1++) {
			if (m0->name == m1->name)
				Error(module, m0->name_token->location, "Duplicate member called '%' in struct %\n", m0->name, ast->name);
		}
	}
}

static Intrinsic intrinsics[INTRINSIC_COUNT];

static void InitIntrinsics() {
	intrinsics[INTRINSIC_SYSTEM_CALL] = {
		"SystemCall",
		GetFunctionType(
			GetTuple({ TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64, TYPE_INT64 }),
			TYPE_INT64
		),
		[](void* input, void* output) { *(s64*)output = 0; },
	};
}

static IntrinsicID FindIntrinsic(String name, TypeID input_type) {
	for (u64 id = 0; id < INTRINSIC_COUNT; id++) {
		TypeInfo* info = GetTypeInfo(intrinsics[id].type);

		if (name == intrinsics[id].name && CanCast(CAST_IMPLICIT, input_type, info->function_info.input))
			return (IntrinsicID)id;
	}

	return INTRINSIC_INVALID;
}

TypeID Scanner::GetBaseType(Ast::BaseType basetype, Ast::Scope* scope) {
	TypeID result = TYPE_NULL;

	switch (basetype.kind) {
		case Ast::BASETYPE_PRIMITIVE: {
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

		case Ast::BASETYPE_TUPLE: {
			u32 tuple_count = basetype.tuple.length;

			if (!tuple_count)
				Error(basetype.token->location, "Empty tuple is an invalid type.\n");

			TypeID types[tuple_count];

			for (u32 i = 0; i < tuple_count; i++) {
				types[i] = GetType(&basetype.tuple[i], scope);
			}

			result = GetTuple({types, tuple_count});
		} break;

		case Ast::BASETYPE_FUNCTION: {
			TypeID input  = GetType(basetype.function.input, scope);
			TypeID output = GetType(basetype.function.output, scope);

			result = GetFunctionType(input, output);
		} break;

		case Ast::BASETYPE_USERTYPE: {
			result = FindUserType(basetype.token->identifier_string, scope);
		} break;
	}

	return result;
}

TypeID Scanner::GetType(Ast::Type* ast_type, Ast::Scope* scope) {
	TypeID result = TYPE_EMPTY_TUPLE;

	if (!ast_type)
		return result;

	result = GetBaseType(ast_type->basetype, scope);

	if (!result)
		return result;

	for (s32 i = ast_type->specifiers.length-1; i >= 0; i--) {
		Ast::Specifier* specifier = &ast_type->specifiers[i];

		switch (specifier->kind) {
			case Ast::SPECIFIER_POINTER:     result = GetPointer(result);  break;
			case Ast::SPECIFIER_OPTIONAL:    result = GetOptional(result); break;
			case Ast::SPECIFIER_ARRAY:       result = GetArray(result);    break;

			case Ast::SPECIFIER_FIXED_ARRAY: {
				ScanExpression(specifier->size_expression, scope);

				if (!CanCast(CAST_IMPLICIT, specifier->size_expression->type, TYPE_INT64))
					Error(specifier->size_expression, "Fixed array size must be an integer.\n");

				// @RemoveMe @Todo: Need to interpret size_expression
				Assert(specifier->size_expression->kind == Ast::Expression::TERMINAL_LITERAL);

				Ast::Expression_Literal* literal = (Ast::Expression_Literal*)specifier->size_expression;
				u64 length = literal->token->literal_int;

				if (length <= 0) // Shouldn't we just enforce uint?
					Error(specifier->size_expression, "Fixed array size must be larger than 0.\n");

				result = GetFixedArray(result, length);
			} break;
		}
	}

	return result;
}

void Scanner::ScanExpressionTerminalName(Ast::Expression_Terminal* terminal, Ast::Scope* scope) {
	if (terminal->token->kind == TOKEN_IDENTIFIER_CASUAL) {
		Ast::Variable* variable = FindVariable(scope, terminal->token->identifier_string);

		if (!variable)
			Error(terminal->token->location, "Variable with name '%' does not exist.\n", terminal->token);

		Ast::Expression_Variable* variable_expression = (Ast::Expression_Variable*)terminal;
		variable_expression->variable = variable;

		variable_expression->kind = Ast::Expression::TERMINAL_VARIABLE;
		variable_expression->type = variable->type;

		if (variable->flags & Ast::VARIABLE_FLAG_CONSTANT) {
			// Constants are not lvalues - type stays as-is
		}
		else {
			// Variables are lvalues - wrap type in reference
			variable_expression->type = GetReference(variable_expression->type);
		}
	}
	else if (terminal->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		TypeID type = FindUserType(terminal->token->identifier_string, scope);

		if (!type)
			Error(terminal->token->location, "User type with name '%' does not exist.\n", terminal->token);

		terminal->type = type;

		if (GetTypeKind(type) == TYPE_STRUCT) {
			terminal->kind = Ast::Expression::TERMINAL_STRUCT; // @FixMe
			Ast::Expression_Struct* struct_terminal = (Ast::Expression_Struct*)terminal;
			struct_terminal->structure = GetTypeInfo(type)->struct_info.ast;
		}
		else if (GetTypeKind(type) == TYPE_ENUM) {
			terminal->kind = Ast::Expression::TERMINAL_ENUM;
			Ast::Expression_Enum* enum_terminal = (Ast::Expression_Enum*)terminal;
			enum_terminal->enumeration = GetTypeInfo(type)->enum_info.ast;
		}
		else if (GetTypeKind(type) == TYPE_PRIMITIVE) {
			terminal->kind = Ast::Expression::TERMINAL_PRIMITIVE; // @FixMe
		}
	}
	else {
		Assert();
		Error(terminal->token->location, "Unknown variable '%'\n", terminal->token);
	}
}

void Scanner::ScanExpressionFixedArray(Ast::Expression_Fixed_Array* fixed_array, Ast::Scope* scope) {
	TypeID subtype = TYPE_NULL;

	for (u32 i = 0; i < fixed_array->elements.length; i++) {
		Ast::Expression* element = fixed_array->elements[i];
		ScanExpression(element, scope);

		if (i) {
			if (!CanCast(CAST_IMPLICIT, element->type, subtype))
				Error(element, "Cannot implicitly cast type '%' to '%'\n", element->type, subtype);

			fixed_array->elements[i] = ImplicitCast(element, subtype, module);
		}
		else {
			subtype = element->type;
			fixed_array->type = GetFixedArray(subtype, fixed_array->elements.length);
		}
	}

}

void Scanner::ScanExpressionArray(Ast::Expression_Array* array, Ast::Scope* scope) {
	ScanExpression(array->left, scope);
	ScanExpression(array->right, scope);

	TypeID left_type = RemoveReference(array->left->type);
	TypeID right_type = RemoveReference(array->right->type);

	if (GetTypeKind(left_type) != TYPE_POINTER)
		Error(array, "Begin expression must be a pointer type, not: %\n", left_type);

	if (GetTypeKind(right_type) == TYPE_POINTER) {
		if (!CanCast(CAST_IMPLICIT, right_type, left_type))
			Error(array->right, "Array begin and end types are incompatible: [%..%]\n", left_type, right_type);

		array->right = ImplicitCast(array->right, left_type, module);
	}
	else if (CanCast(CAST_IMPLICIT, right_type, TYPE_UINT64)) {
		array->right = ImplicitCast(array->right, TYPE_UINT64, module);
	}
	else {
		Error(array->right, "Array end must be a % or an uint, not: %\n", left_type, right_type);
	}

	array->type = GetArray(GetSubType(left_type));
}

void Scanner::ScanExpressionLiteral(Ast::Expression_Literal* literal, Ast::Scope* scope) {
}

void Scanner::ScanExpressionTuple(Ast::Expression_Tuple* tuple, Ast::Scope* scope) {
	tuple->recursive_count = tuple->elements.length;

	TypeID types[tuple->elements.length];

	for (u32 i = 0; i < tuple->elements.length; i++) {
		Ast::Expression* element = tuple->elements[i];
		ScanExpression(element, scope);
		types[i] = element->type;

		if (element->kind == Ast::Expression::TUPLE) {
			tuple->recursive_count += ((Ast::Expression_Tuple*)element)->recursive_count-1;
		}

		if (element->type == TYPE_EMPTY_TUPLE && (element->kind != Ast::Expression::TUPLE || tuple->elements.length > 1))
			Error(element, "Tuple elements aren't allowed to be of type %.\n", element->type);
	}

	tuple->type = GetTuple({ types, tuple->elements.length });
}

void Scanner::ScanExpressionCall(Ast::Expression_Call* call, Ast::Scope* scope) {
	Ast::Expression_Terminal* terminal = (Ast::Expression_Terminal*)call->function;
	Ast::Expression_Binary* dot = (Ast::Expression_Binary*)call->function;

	ScanExpression(call->parameters, scope);

	if (dot->kind == Ast::Expression::BINARY_DOT && dot->right->kind == Ast::Expression::TERMINAL_NAME && ((Ast::Expression_Terminal*)dot->right)->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		Ast::Expression_Dot_Call* dcall = (Ast::Expression_Dot_Call*)call;
		dcall->kind = Ast::Expression::DOT_CALL;

		Ast::Expression_Function* function_expression = (Ast::Expression_Function*)dot->right;

		ScanExpression(dot->left, scope);

		if (dot->left->type == TYPE_EMPTY_TUPLE)
			Error(dot->left, "Cannot call into an empty tuple.\n");

		TypeID full_param_type = MergeTypeRight(dot->left->type, dcall->parameters->type);
		Ast::Function* function = FindFunction(scope, function_expression->token->identifier_string, full_param_type);

		if (!function)
			Error(dcall, "No function exists called % with input type: %.\n", function_expression->token->identifier_string, full_param_type);

		function_expression->kind = Ast::Expression::TERMINAL_FUNCTION;
		function_expression->function = function;
		function_expression->type = function->type;

		dcall->type = function->return_type;

		Assert(dcall->parameters->elements.length == function->parameters.length-1);

		dot->left = ImplicitCast(dot->left, function->parameters[0].type, module);

		for (u32 i = 0; i < dcall->parameters->elements.length; i++)
			call->parameters->elements[i] = ImplicitCast(call->parameters->elements[i], function->parameters[i+1].type, module);

		return;
	}

	if (call->function->kind == Ast::Expression::TERMINAL_NAME && terminal->token->kind == TOKEN_IDENTIFIER_FORMAL) {
		if (Ast::Function* function = FindFunction(scope, terminal->token->identifier_string, call->parameters->type); function) {
			Ast::Expression_Function* function_expression = (Ast::Expression_Function*)call->function;
			call->function->kind = Ast::Expression::TERMINAL_FUNCTION;
			function_expression->function = function;
			function_expression->type = function->type;

			call->type = function->return_type;

			TypeInfo* info = GetTypeInfo(function->type);

			call->parameters = (Ast::Expression_Tuple*)ImplicitCast(call->parameters, info->function_info.input, module);
			return;
		}

		if (IntrinsicID intrinsic_id = FindIntrinsic(terminal->token->identifier_string, call->parameters->type); intrinsic_id != INTRINSIC_INVALID) {
			TypeID type = intrinsics[intrinsic_id].type;
			TypeInfo* type_info = GetTypeInfo(type);

			Ast::Expression_Intrinsic* intrinsic_expression = (Ast::Expression_Intrinsic*)call->function;
			intrinsic_expression->intrinsic = intrinsic_id;
			intrinsic_expression->type = type;

			call->function->kind = Ast::Expression::TERMINAL_INTRINSIC;
			call->type = type_info->function_info.output;

			call->parameters = (Ast::Expression_Tuple*)ImplicitCast(call->parameters, type_info->function_info.input, module);
			return;
		}

		Error(terminal->token->location, "No function exists called % with input type: %.\n", terminal->token->identifier_string, call->parameters->type);
		return;
	}

	ScanExpression(call->function, scope);

	if (!IsFunctionPointer(call->function->type))
		Error(call->function, "Expression of type % cannot be called like a function.\n", call->function->type);

	TypeID function_type = GetSubType(call->function->type);
	TypeInfo* function_type_info = GetTypeInfo(function_type);
	call->type = function_type_info->function_info.output;

	if (!CanCast(CAST_IMPLICIT, call->parameters->type, GetFunctionInputType(function_type)))
		Error(call->function, "Function of type % called with invalid arguments: %.\n", call->function->type, call->parameters->type);

	call->parameters = (Ast::Expression_Tuple*)ImplicitCast(call->parameters, function_type_info->function_info.input, module);
}

void Scanner::ScanExpressionSubscript(Ast::Expression_Subscript* subscript, Ast::Scope* scope) {
	ScanExpression(subscript->array, scope);
	ScanExpression(subscript->index, scope);

	TypeID array_type = subscript->array->type;
	bool is_lvalue = IsReference(array_type);

	// Unwrap reference if present
	if (is_lvalue)
		array_type = GetSubType(array_type);

	bool fixed = GetTypeKind(array_type) == TYPE_FIXED_ARRAY;

	if (GetTypeKind(array_type) != TYPE_FIXED_ARRAY &&
		GetTypeKind(array_type) != TYPE_ARRAY &&
		GetTypeKind(array_type) != TYPE_POINTER)
		Error(subscript->array, "Expression with type % is not a valid array.\n", subscript->array->type);

	if (!CanCast(CAST_IMPLICIT, subscript->index->type, TYPE_INT64))
		Error(subscript->index, "Subscript index must be an integer, not: %\n", subscript->index->type);

	subscript->index = ImplicitCast(subscript->index, TYPE_INT64, module);

	TypeID element_type = GetSubType(array_type);

	if (!fixed || is_lvalue) {
		// Subscripting an array or lvalue gives lvalue element
		subscript->type = GetReference(element_type);
	} else {
		// Fixed array rvalue gives rvalue element
		subscript->type = element_type;
	}
}

void Scanner::ScanExpressionUnaryAddressOf(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	if (!IsReference(unary->subexpression->type))
		Error(unary, "Cannot take address of non-lvalue (type: %).\n", unary->subexpression->type);

	// Take address of ref T -> get *T (pointer to T)
	TypeID subtype = GetSubType(unary->subexpression->type);
	unary->type = GetPointer(subtype);
}

void Scanner::ScanExpressionUnaryReferenceOf(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	TypeID ptr_type = RemoveReference(unary->subexpression->type);

	if (GetTypeKind(ptr_type) != TYPE_POINTER)
		Error(unary, "Cannot dereference non-pointer type: %\n", ptr_type);

	TypeID subtype = GetSubType(ptr_type);
	unary->type = GetReference(subtype);  // Dereferencing gives lvalue
}

void Scanner::ScanExpressionUnaryBitwiseNot(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	if (IsInteger(GetArithmeticBackingType(unary->subexpression->type))) {
		unary->subexpression = ImplicitCast(unary->subexpression, GetArithmeticBackingType(unary->subexpression->type), module);
		unary->type = unary->subexpression->type;
	}
	else if (GetTypeKind(unary->subexpression->type) == TYPE_POINTER) {
		unary->type = unary->subexpression->type;
	}
	else Error(unary->subexpression, "Type % is not an integer or pointer.\n", unary->subexpression->type);
}

void Scanner::ScanExpressionUnaryMinus(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	if (!IsInteger(GetArithmeticBackingType(unary->subexpression->type)) && GetTypeKind(unary->subexpression->type) != TYPE_POINTER && !IsFloat(unary->subexpression->type))
		Error(unary->subexpression, "Unary minus does not work on type '%'.\n", unary->subexpression->type);

	unary->subexpression = ImplicitCast(unary->subexpression, GetArithmeticBackingType(unary->subexpression->type), module);
	unary->type = unary->subexpression->type;
}

void Scanner::ScanExpressionUnaryPlus(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	if (!IsSignedInteger(unary->subexpression->type) && !IsFloat(unary->subexpression->type))
		Error(unary->subexpression, "Unary plus can only be applied to a signed integer or float.\n", unary->subexpression->type);

	unary->type = unary->subexpression->type;
}

void Scanner::ScanExpressionUnaryNot(Ast::Expression_Unary* unary, Ast::Scope* scope) {
	ScanExpression(unary->subexpression, scope);

	if (!CanCast(CAST_IMPLICIT, unary->subexpression->type, TYPE_BOOL))
		Error(unary->subexpression, "Type % cannot be casted to bool.\n", unary->subexpression->type);

	unary->subexpression = ImplicitCast(unary->subexpression, TYPE_BOOL, module);
	unary->type = TYPE_BOOL;
}

void Scanner::ScanExpressionBinaryDot(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);

	TypeID type = binary->left->type;

	if (type == TYPE_EMPTY_TUPLE)
		Error(binary->left, "Cannot dot into empty tuple.\n");

	if (binary->left->kind == Ast::Expression::TERMINAL_ENUM) {
		Ast::Enum* ast_enum = ((Ast::Expression_Enum*)binary->left)->enumeration;

		if (binary->right->kind != Ast::Expression::TERMINAL_NAME)
			Error(binary->right, "Expected enum member name.\n");

		binary->right->kind = Ast::Expression::TERMINAL_ENUM_MEMBER;

		Ast::Expression_Enum_Member* member_terminal = (Ast::Expression_Enum_Member*)binary->right;
		String name = member_terminal->token->identifier_string;

		Ast::Enum_Member* member = FindEnumMember(ast_enum, name);

		if (!member)
			Error(binary->right, "Enum % does not contain a member called \"%\".", ast_enum->name, name);

		member_terminal->member = member;
		member_terminal->type = type;

		binary->type = type;
	}
	else if (binary->right->kind == Ast::Expression::TERMINAL_NAME && ((Ast::Expression_Terminal*)binary->right)->token->kind == TOKEN_IDENTIFIER_CASUAL) {
		Ast::Expression_Terminal* terminal = (Ast::Expression_Terminal*)binary->right;
		String name = terminal->token->identifier_string;

		// Unwrap references, then pointers
		type = RemoveReference(type);
		while (GetTypeKind(type) == TYPE_POINTER)
			type = GetSubType(type);

		if (GetTypeKind(type) == TYPE_STRUCT) {

			Ast::Struct* ast_struct = GetTypeInfo(type)->struct_info.ast;
			Ast::Struct_Member* member = FindStructMember(ast_struct, terminal->token->identifier_string);

			if (!member)
				Error(binary, "Struct % does not have a member named %\n", ast_struct->name, terminal->token);

			TypeID member_type = member->type;

			if (GetTypeKind(binary->left->type) == TYPE_POINTER) {
				// Pointer member access gives lvalue
				binary->type = GetReference(member_type);
			} else if (IsReference(binary->left->type)) {
				// Reference member access gives lvalue member
				binary->type = GetReference(member_type);
			} else {
				// Value member access - preserve type
				binary->type = member_type;
			}

			terminal->kind = Ast::Expression::TERMINAL_STRUCT_MEMBER;
			((Ast::Expression_Struct_Member*)terminal)->member = member;
			terminal->type = member->type;
		}
		else if (GetTypeKind(type) == TYPE_ARRAY || GetTypeKind(type) == TYPE_FIXED_ARRAY) {
			bool fixed = GetTypeKind(type) == TYPE_FIXED_ARRAY;

			if (name == "begin" || name == "data") {
				binary->right->kind = Ast::Expression::TERMINAL_ARRAY_BEGIN;

				if (fixed) {
					// Fixed array .data/.begin gives lvalue to first element
					binary->type = GetReference(GetSubType(binary->left->type));
				} else {
					// Dynamic array .data/.begin is a pointer (not ref)
					binary->type = GetPointer(GetSubType(binary->left->type));
				}
			}
			else if (name == "end") {
				binary->right->kind = Ast::Expression::TERMINAL_ARRAY_END;
				binary->type = GetPointer(GetSubType(binary->left->type));
			}
			else if (name == "length" || name == "count") {
				binary->right->kind = Ast::Expression::TERMINAL_ARRAY_LENGTH;
				binary->type = TYPE_UINT64;
				// Length is rvalue, not ref
			}
			else Error(binary->right, "'%' does not have a member named '%'.\n", type, name);
		}
		else Error(binary, "'%' does not have a member named '%'.\n", type, name);
	}
	else Error(binary, "Invalid dot expression.\n", binary->left->type);
}

void Scanner::ScanExpressionBinaryCompareEquality(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

	if (!dominant)
		Error(binary, "% and % are incompatible types.\n", binary->left->type, binary->right->type);

	binary->left  = ImplicitCast(binary->left,  dominant, module);
	binary->right = ImplicitCast(binary->right, dominant, module);

	binary->type = TYPE_BOOL;
}

void Scanner::ScanExpressionBinaryCompareOrdered(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	TypeID dominant = GetDominantType(GetArithmeticBackingType(binary->left->type), GetArithmeticBackingType(binary->right->type));

	if (!dominant)
		Error(binary, "Incompatible types % and %\n", binary->left->type, binary->right->type);

	if (!IsInteger(dominant) && !IsFloat(dominant) && GetTypeKind(dominant) != TYPE_POINTER)
		Error(binary, "Cannot compare types '%' and '%'.\n", binary->left->type, binary->right->type);

	binary->left  = ImplicitCast(binary->left,  dominant, module);
	binary->right = ImplicitCast(binary->right, dominant, module);

	binary->type = TYPE_BOOL;
}

void Scanner::ScanExpressionBinaryArithmetic(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	if (GetTypeKind(binary->left->type) == TYPE_POINTER) {
		// ptr + int = ptr
		// ptr - int = ptr
		// ptr - ptr = int

		if (binary->kind == Ast::Expression::BINARY_ADD) {
			if (!CanCast(CAST_IMPLICIT, binary->right->type, TYPE_INT64))
				Error(binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

			binary->right = ImplicitCast(binary->right, TYPE_INT64, module);
			binary->type = binary->left->type;
		}
		else if (binary->kind == Ast::Expression::BINARY_SUBTRACT) {
			if (GetTypeKind(binary->right->type) == TYPE_POINTER) {
				TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

				if (!dominant)
					Error(binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

				binary->left = ImplicitCast(binary->left, dominant, module);
				binary->right = ImplicitCast(binary->right, dominant, module);

				binary->type = TYPE_INT64;
			}
			else if (CanCast(CAST_IMPLICIT, binary->right->type, TYPE_INT64)) {
				binary->right = ImplicitCast(binary->right, TYPE_INT64, module);
				binary->type = binary->left->type;
			}
			else
				Error(binary, "Pointer expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);
		}
		else
			Error(binary, "Binary expression '%' cannot be used on a pointer.\n", binary->op);
	}
	else if (IsFloat(binary->left->type) || IsFloat(binary->right->type)) {
		// float + float = float
		// float - float = float
		// float * float = float
		// float / float = float

		TypeID dominant = GetDominantType(binary->left->type, binary->right->type);

		if (!dominant || !IsFloat(dominant) || binary->kind == Ast::Expression::BINARY_MODULO)
			Error(binary, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

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
			Error(binary, "Binary expression % % % is invalid.\n", binary->left->type, binary->op, binary->right->type);

		binary->left = ImplicitCast(binary->left, dominant, module);
		binary->right = ImplicitCast(binary->right, dominant, module);

		binary->type = dominant;
	}
}

void Scanner::ScanExpressionBinaryBitwise(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	// ptr OR  int = ptr
	// ptr XOR int = ptr
	// ptr AND int = int

	if (!IsInteger(GetArithmeticBackingType(binary->left->type)) && GetTypeKind(binary->left->type) != TYPE_POINTER)
		Error(binary, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);

	if (!IsInteger(GetArithmeticBackingType(binary->right->type)) && GetTypeKind(binary->right->type) != TYPE_POINTER)
		Error(binary, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);

	binary->type = binary->left->type;
}

void Scanner::ScanExpressionBinaryShift(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	if (!IsInteger(GetArithmeticBackingType(binary->left->type)) && GetTypeKind(binary->left->type) != TYPE_POINTER)
		Error(binary, "Cannot use bitwise % with type: %\n", binary->op, binary->left->type);

	if (!IsInteger(GetArithmeticBackingType(binary->right->type)))
		Error(binary, "Cannot use bitwise % with type: %\n", binary->op, binary->right->type);

	binary->left  = ImplicitCast(binary->left,  GetArithmeticBackingType(binary->left->type),  module);
	binary->right = ImplicitCast(binary->right, GetArithmeticBackingType(binary->right->type), module);

	binary->type = binary->left->type;
}

void Scanner::ScanExpressionBinaryLogical(Ast::Expression_Binary* binary, Ast::Scope* scope) {
	ScanExpression(binary->left, scope);
	ScanExpression(binary->right, scope);

	if (!CanCast(CAST_IMPLICIT, binary->left->type, TYPE_BOOL))
		Error(binary, "% cannot be converted to bool.\n", binary->left->type);

	if (!CanCast(CAST_IMPLICIT, binary->right->type, TYPE_BOOL))
		Error(binary, "% cannot be converted to bool.\n", binary->right->type);

	binary->left = ImplicitCast(binary->left, TYPE_BOOL, module);
	binary->right = ImplicitCast(binary->right, TYPE_BOOL, module);

	binary->type = TYPE_BOOL;
}

void Scanner::ScanExpressionIfElse(Ast::Expression_Ternary* ternary, Ast::Scope* scope) {
	ScanExpression(ternary->left, scope);
	ScanExpression(ternary->middle, scope);
	ScanExpression(ternary->right, scope);

	TypeID dominant = GetDominantType(ternary->left->type, ternary->right->type);

	if (!dominant)
		Error(ternary->left, "Types '%' and '%' are incompatible.\n", ternary->left->type, ternary->right->type);

	if (!CanCast(CAST_IMPLICIT, ternary->middle->type, TYPE_BOOL))
		Error(ternary->middle, "Type % not convertable to bool\n", ternary->middle->type);

	ternary->middle = ImplicitCast(ternary->middle, TYPE_BOOL, module);
	ternary->left   = ImplicitCast(ternary->left, dominant, module);
	ternary->right  = ImplicitCast(ternary->right, dominant, module);

	// If both branches produce references of the same type, result is also a reference
	if (ternary->left->type == ternary->right->type && IsReference(ternary->left->type)) {
		ternary->type = ternary->left->type;  // Preserve reference type
	} else {
		ternary->type = dominant;
	}
}

void Scanner::ScanExpressionLambda(Ast::Expression* expression, Ast::Scope* scope) {
	Assert();
}

void Scanner::ScanExpressionAs(Ast::Expression_As* as, Ast::Scope* scope) {
	ScanExpression(as->expression, scope);
	as->type = GetType(&as->ast_type, scope);

	if (!CanCast(CAST_EXPLICIT, as->expression->type, as->type))
		Error(as, "Type % is not convertable to %\n", as->expression->type, as->type);
}

void Scanner::ScanExpression(Ast::Expression* expression, Ast::Scope* scope) {
	switch (expression->kind) {
		case Ast::Expression::TERMINAL_NAME:    ScanExpressionTerminalName((Ast::Expression_Terminal*)expression, scope);   break;
		case Ast::Expression::FIXED_ARRAY:      ScanExpressionFixedArray((Ast::Expression_Fixed_Array*)expression, scope);  break;
		case Ast::Expression::ARRAY:            ScanExpressionArray((Ast::Expression_Array*)expression, scope);             break;
		case Ast::Expression::TERMINAL_LITERAL: ScanExpressionLiteral((Ast::Expression_Literal*)expression, scope);         break;
		case Ast::Expression::TUPLE:            ScanExpressionTuple((Ast::Expression_Tuple*)expression, scope);             break;
		case Ast::Expression::CALL:             ScanExpressionCall((Ast::Expression_Call*)expression, scope);               break;
		case Ast::Expression::SUBSCRIPT:        ScanExpressionSubscript((Ast::Expression_Subscript*)expression, scope);     break;

		case Ast::Expression::UNARY_ADDRESS_OF:   ScanExpressionUnaryAddressOf((Ast::Expression_Unary*)expression, scope);   break;
		case Ast::Expression::UNARY_REFERENCE_OF: ScanExpressionUnaryReferenceOf((Ast::Expression_Unary*)expression, scope); break;
		case Ast::Expression::UNARY_BITWISE_NOT:  ScanExpressionUnaryBitwiseNot((Ast::Expression_Unary*)expression, scope);  break;
		case Ast::Expression::UNARY_MINUS:        ScanExpressionUnaryMinus((Ast::Expression_Unary*)expression, scope);       break;
		case Ast::Expression::UNARY_PLUS:         ScanExpressionUnaryPlus((Ast::Expression_Unary*)expression, scope);        break;
		case Ast::Expression::UNARY_NOT:          ScanExpressionUnaryNot((Ast::Expression_Unary*)expression, scope);         break;

		case Ast::Expression::BINARY_DOT: ScanExpressionBinaryDot((Ast::Expression_Binary*)expression, scope); break;

		case Ast::Expression::BINARY_COMPARE_EQUAL:
		case Ast::Expression::BINARY_COMPARE_NOT_EQUAL:
			ScanExpressionBinaryCompareEquality((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::BINARY_COMPARE_LESS:
		case Ast::Expression::BINARY_COMPARE_LESS_OR_EQUAL:
		case Ast::Expression::BINARY_COMPARE_GREATER:
		case Ast::Expression::BINARY_COMPARE_GREATER_OR_EQUAL:
			ScanExpressionBinaryCompareOrdered((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::BINARY_ADD:
		case Ast::Expression::BINARY_SUBTRACT:
		case Ast::Expression::BINARY_MULTIPLY:
		case Ast::Expression::BINARY_DIVIDE:
		case Ast::Expression::BINARY_MODULO:
			ScanExpressionBinaryArithmetic((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::BINARY_BITWISE_OR:
		case Ast::Expression::BINARY_BITWISE_XOR:
		case Ast::Expression::BINARY_BITWISE_AND:
			ScanExpressionBinaryBitwise((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::BINARY_LEFT_SHIFT:
		case Ast::Expression::BINARY_RIGHT_SHIFT:
			ScanExpressionBinaryShift((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::BINARY_AND:
		case Ast::Expression::BINARY_OR:
			ScanExpressionBinaryLogical((Ast::Expression_Binary*)expression, scope);
			break;

		case Ast::Expression::IF_ELSE: ScanExpressionIfElse((Ast::Expression_Ternary*)expression, scope); break;
		case Ast::Expression::LAMBDA:  ScanExpressionLambda(expression, scope); break;
		case Ast::Expression::AS:      ScanExpressionAs((Ast::Expression_As*)expression, scope); break;

		default:
			AssertUnreachable();
	}
}

void Scanner::ScanScope(Ast::Scope* scope) {
	for (Ast::Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		ast_struct->type = CreateStructType(ast_struct, 0);

		for (Ast::Struct* other = scope->structs; other < ast_struct; other++) {
			if (ast_struct->name == other->name)
				Error(ast_struct->name_token->location, "Duplicate struct called '%'\n", ast_struct->name);
		}

		for (Ast::Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
			if (ast_struct->name == ast_enum->name) {
				Token* name_token = ast_struct->name_token;

				if (ast_struct->name_token->location.line < ast_enum->name_token->location.line)
					name_token = ast_enum->name_token;

				Error(name_token->location, "Duplicate type called '%'\n", name_token);
			}
		}

		CheckForStructMemberDuplicates(module, ast_struct);
	}

	for (Ast::Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
		Zero(&ast_enum->type);
		ast_enum->underlying_type = TYPE_INT64;
		ast_enum->type = CreateEnumType(ast_enum, ast_enum->underlying_type);

		for (Ast::Enum* eo = scope->enums; eo < ast_enum; eo++) {
			if (ast_enum->name == eo->name)
				Error(ast_enum->name_token->location, "Duplicate enum called '%'\n", ast_enum->name);
		}

		CheckForEnumMemberDuplicates(module, ast_enum);
	}

	for (Ast::Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		for (Ast::Struct_Member* member = ast_struct->members; member < ast_struct->members.End(); member++) {
			member->type = GetType(&member->ast_type, scope);

			if (!member->type)
				Error(member->ast_type.basetype.token->location, "Unknown type '%'\n", member->ast_type.basetype.token);

			if (GetTypeKind(member->type) == TYPE_FUNCTION)
				Error(member->ast_type.basetype.token->location, "Bare function type not allowed as struct member\n");
		}
	}

	for (Ast::Struct* ast_struct = scope->structs; ast_struct < scope->structs.End(); ast_struct++) {
		CheckForCircularDependencies(ast_struct, module);
		CalculateStructSize(ast_struct);
		// Print("Struct % has size = %\n", ast_struct->name, ast_struct->type.size);
	}

	for (Ast::Enum* ast_enum = scope->enums; ast_enum < scope->enums.End(); ast_enum++) {
		for (Ast::Enum_Member* member = ast_enum->members; member < ast_enum->members.End(); member++) {
			ScanExpression(member->expression, scope);
		}
	}

	for (Ast::Function* function = scope->functions; function < scope->functions.End(); function++) {
		ScanFunction(function, scope);

		for (Ast::Function* other = scope->functions; other < function; other++) {
			if (function->type == other->type && function->name == other->name)
				Error(function->name_token->location, "Function '%' with type % already exists.\n", function->name, function->type);
		}
	}

	for (Ast::Function* function = scope->functions; function < scope->functions.End(); function++) {
		ScanCode(&function->code, scope, function);

		if (function->return_type != TYPE_EMPTY_TUPLE && !function->code.all_paths_return) {
			if (function->code.contains_return)
				Error(function->name_token->location, "Not all paths return a value.\n");
			else
				Error(function->name_token->location, "Function does not return a value.\n");
		}
	}
}

void Scanner::ScanIf(Ast::Function* function, Ast::Code* code, Ast::Branch* branch) {
	ScanExpression(branch->if_condition, &code->scope);

	if (!CanCast(CAST_IMPLICIT, branch->if_condition->type, TYPE_BOOL))
		Error(branch->if_condition, "Cannot implicitly cast condition with type % to bool\n", branch->if_condition->type);

	branch->if_condition = ImplicitCast(branch->if_condition, TYPE_BOOL, module);

	branch->code.is_inside_loop = code->is_inside_loop;
	code->contains_break = code->contains_break || branch->code.contains_break;
}

void Scanner::ScanWhile(Ast::Function* function, Ast::Code* code, Ast::Branch* branch) {
	ScanExpression(branch->while_condition, &code->scope);

	if (!CanCast(CAST_IMPLICIT, branch->while_condition->type, TYPE_BOOL))
		Error(branch->while_condition, "Cannot implicitly cast condition with type % to bool\n", branch->while_condition->type);

	branch->while_condition = ImplicitCast(branch->while_condition, TYPE_BOOL, module);

	branch->code.is_inside_loop = true;
}

void Scanner::ScanRangeFor(Ast::Function* function, Ast::Code* code, Ast::Branch* branch) {
	ScanExpression(branch->for_range.range, &code->scope);

	if (!IsArray(branch->for_range.range->type) && !IsFixedArray(branch->for_range.range->type))
		Error(branch->for_range.range, "For loop cannot range over type: %\n", branch->for_range.range->type);

	TypeID range_type = RemoveReference(branch->for_range.range->type);

	branch->for_range.iterator->type = GetSubType(range_type);
	branch->code.scope.variables.Add(branch->for_range.iterator);

	if (branch->for_range.filter) {
		ScanExpression(branch->for_range.filter, &branch->code.scope);

		if (!CanCast(CAST_IMPLICIT, branch->for_range.filter->type, TYPE_BOOL))
			Error(branch->for_range.filter, "For loop filter expression of type % cannot be implicitly casted to bool\n", branch->for_range.filter->type);

		branch->for_range.filter = ImplicitCast(branch->for_range.filter, TYPE_BOOL, module);
	}

	branch->code.is_inside_loop = true;
}

void Scanner::ScanVerboseFor(Ast::Function* function, Ast::Code* code, Ast::Branch* branch) {
	if (branch->for_verbose.variable->assignment) {
		ScanExpression(branch->for_verbose.variable->assignment, &code->scope);
		// Variables store the value type, not the reference type
		branch->for_verbose.variable->type = RemoveReference(branch->for_verbose.variable->assignment->type);
	}

	if (branch->for_verbose.variable->ast_type)
		branch->for_verbose.variable->type = GetType(branch->for_verbose.variable->ast_type, &code->scope);

	if (branch->for_verbose.variable->ast_type && branch->for_verbose.variable->assignment) {
		if (!CanCast(CAST_IMPLICIT, branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type))
			Error(branch->for_verbose.variable->assignment, "Cannot assign expression with type % to variable with type %\n", branch->for_verbose.variable->assignment->type, branch->for_verbose.variable->type);

		branch->for_verbose.variable->assignment = ImplicitCast(branch->for_verbose.variable->assignment, branch->for_verbose.variable->type, module);
	}

	Assert(branch->code.scope.parent);
	branch->code.scope.variables.Add(branch->for_verbose.variable);

	ScanExpression(branch->for_verbose.condition, &branch->code.scope);

	if (!CanCast(CAST_IMPLICIT, branch->for_verbose.condition->type, TYPE_BOOL))
		Error(branch->for_verbose.condition, "For loop condition expression of type % cannot be implicitly casted to bool\n", branch->for_verbose.condition->type);

	branch->for_verbose.condition = ImplicitCast(branch->for_verbose.condition, TYPE_BOOL, module);

	if (branch->for_verbose.next) {
		ScanExpression(branch->for_verbose.next, &branch->code.scope);

		if (!CanCast(CAST_EXPLICIT, branch->for_verbose.next->type, branch->for_verbose.variable->type))
			Error(branch->for_verbose.next,
				"For loop stride expression of type % cannot be explicitly casted to type %\n",
				branch->for_verbose.next->type, branch->for_verbose.variable->type
			);

		branch->for_verbose.next = ImplicitCast(branch->for_verbose.next, branch->for_verbose.variable->type, module);
	}

	branch->code.is_inside_loop = true;
}

void Scanner::ScanBranchBlock(Ast::Function* function, Ast::Code* code, Ast::BranchBlock* branch_block) {
	for (u32 i = 0; i < branch_block->branches.length; i++) {
		Ast::Branch* branch = &branch_block->branches[i];

		branch->code.scope.parent = &code->scope; // @Hack

		switch (branch->kind) {
			case Ast::BRANCH_NAKED: {
				branch->code.is_inside_loop = code->is_inside_loop;
				code->contains_break = code->contains_break || branch->code.contains_break;
			} break;

			case Ast::BRANCH_IF:          ScanIf(function, code, branch);         break;
			case Ast::BRANCH_WHILE:       ScanWhile(function, code, branch);      break;
			case Ast::BRANCH_FOR_RANGE:   ScanRangeFor(function, code, branch);   break;
			case Ast::BRANCH_FOR_VERBOSE: ScanVerboseFor(function, code, branch); break;
		}

		ScanCode(&branch->code, &code->scope, function);

		code->contains_return = code->contains_return || branch->code.contains_return;
		branch->code.has_defer_that_returns = code->has_defer_that_returns;
	}

	code->all_paths_return = DoesBranchAlwaysReturn(&branch_block->branches[0]);
}

void Scanner::ScanDefer(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	code->defers.Add(&statement->defer);
	statement->defer.code.is_inside_loop = code->is_inside_loop;

	ScanCode(&statement->defer.code, &code->scope, function);

	if (statement->defer.code.all_paths_return) {
		code->has_defer_that_returns = true;
		code->all_paths_return = true;
	}
}

void Scanner::ScanClaim(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	Ast::Claim* claim = &statement->claim;

	ScanExpression(claim->expression, &code->scope);

	if (!CanCast(CAST_IMPLICIT, claim->expression->type, TYPE_BOOL))
		Error(claim->expression, "Claim expression type must be implicitly castable to bool\n");

	claim->expression = ImplicitCast(claim->expression, TYPE_BOOL, module);
}

void Scanner::ScanIncDec(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	Ast::Increment* inc = &statement->increment;
	ScanExpression(inc->expression, &code->scope);

	bool direction = statement->kind == Ast::STATEMENT_INCREMENT;

	if (!IsReference(inc->expression->type))
		Error(inc->expression, "Cannot % non-lvalue (type: %).\n", (direction ? "increment" : "decrement"), inc->expression->type);

	TypeID value_type = GetSubType(inc->expression->type);

	if (!IsInteger(value_type) && !IsFloat(value_type) && GetTypeKind(value_type) != TYPE_POINTER)
		Error(inc->expression, "Cannot % type %, expression must be either an integer, float or pointer.\n", (direction ? "increment" : "decrement"), value_type);
}

void Scanner::ScanReturn(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	code->contains_return = true;
	code->all_paths_return = !code->contains_break;
	function->returns.Add(&statement->ret);

	if (code->has_defer_that_returns)
		Error(statement->ret.token->location, "A defer in this scope already has a return statement. This isn't allowed.\n");

	if (statement->ret.expression) {
		ScanExpression(statement->ret.expression, &code->scope);

		if (!function->return_type)
			Error(statement->ret.token->location, "Unexpected return value for function that doesn't return anything.\n");

		if (!CanCast(CAST_IMPLICIT, statement->ret.expression->type, function->return_type))
			Error(statement->ret.token->location, "Invalid return type: %, expected type: %\n", statement->ret.expression->type, function->return_type);
	}
	else if (function->ast_return_type)
		Error(statement->ret.token->location, "Expected return value with type: %\n", function->return_type);
}

void Scanner::ScanBreak(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	code->contains_break = true;
	code->all_paths_break = !code->contains_return;

	if (!code->is_inside_loop)
		Error(statement->brk.token->location, "break statement must be inside of a loop.\n");
}

void Scanner::ScanVarDecl(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	Ast::Variable* var = &statement->variable_declaration;

	for (Ast::Variable** other_variable = code->scope.variables.Begin(); other_variable < code->scope.variables.End(); other_variable++) {
		if (var->name == (*other_variable)->name)
			Error(var->name_token->location, "Variable with name '%' already declared in this scope.\n", var->name);
	}

	if (var->assignment) {
		ScanExpression(var->assignment, &code->scope);

		if (!var->assignment->type)
			Error(var->assignment, "Expression does not have a type.\n");

		// Variables store the value type, not the reference type
		var->type = RemoveReference(var->assignment->type);
	}

	if (var->ast_type) {
		var->type = GetType(var->ast_type, &code->scope);

		if (!var->type)
			Error(var->name_token->location, "Unknown type %\n", var->ast_type->basetype.token);

		if (var->assignment && !CanCast(CAST_IMPLICIT, var->assignment->type, var->type))
			Error(var->name_token->location, "Variable '%' with type % cannot be assign to type %\n", var->name, var->type, var->assignment->type);
	}

	Assert(var->type);

	if (var->type == TYPE_EMPTY_TUPLE)
		Error(var->assignment, "Cannot declare variable with type %\n", var->type);

	Assert(var->type != TYPE_EMPTY_TUPLE);

	code->scope.variables.Add(var);
}

void Scanner::ScanAssignment(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	Ast::Assignment* assignment = &statement->assignment;

	ScanExpression(assignment->right, &code->scope);
	ScanExpression(assignment->left, &code->scope);

	if (!IsAssignable(assignment->left))
		Error(assignment->left, "Cannot assign to non-lvalue (type: %).\n", assignment->left->type);

	// Left side is ref T, we want to assign to T
	TypeID target_type = GetSubType(assignment->left->type);

	if (!CanCast(CAST_COERCIVE, assignment->right->type, target_type))
		Error(assignment->token->location, "Cannot cast % to %.\n", assignment->right->type, target_type);

	assignment->right = ImplicitCast(assignment->right, target_type, module);
}

void Scanner::ScanBinaryAssignment(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	Ast::Assignment* assignment = &statement->assignment;

	ScanExpression(assignment->right, &code->scope);
	ScanExpression(assignment->left, &code->scope);

	if (!IsAssignable(assignment->left))
		Error(assignment->left, "Cannot assign to non-lvalue (type: %).\n", assignment->left->type);

	TypeID target_type = GetSubType(assignment->left->type);

	if (!IsInteger(target_type) &&
		!IsFloat(target_type) &&
		GetTypeKind(target_type) != TYPE_POINTER)
		Error(assignment->left, "Arithmetic assignment type must be to an integer, float or pointer, not: '%'\n", target_type);

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

	if (IsPointer(target_type)) {
		if (statement->kind != Ast::STATEMENT_ASSIGNMENT_ADD && statement->kind != Ast::STATEMENT_ASSIGNMENT_SUBTRACT)
			Error(assignment->left, "Arithmetic assignment to pointer only allows '+=' and '-='.\n");

		if (!CanCast(CAST_IMPLICIT, assignment->right->type, TYPE_INT64))
			Error(assignment->right, "Expression type must be an integer.\n");

		assignment->right = ImplicitCast(assignment->right, TYPE_INT64, module);
	}
	else {
		if (!CanCast(CAST_IMPLICIT, assignment->right->type, target_type))
			Error(assignment->right, "Cannot implicitly cast '%' to '%'.\n", assignment->right->type, target_type);

		assignment->right = ImplicitCast(assignment->right, target_type, module);
	}
}

void Scanner::ScanStatement(Ast::Statement* statement, Ast::Code* code, Ast::Function* function) {
	switch (statement->kind) {
		case Ast::STATEMENT_DEFER:                ScanDefer(statement, code, function);   break;
		case Ast::STATEMENT_CLAIM:                ScanClaim(statement, code, function);   break;
		case Ast::STATEMENT_INCREMENT:            ScanIncDec(statement, code, function);  break;
		case Ast::STATEMENT_DECREMENT:            ScanIncDec(statement, code, function);  break;
		case Ast::STATEMENT_RETURN:               ScanReturn(statement, code, function);  break;
		case Ast::STATEMENT_BREAK:                ScanBreak(statement, code, function);   break;
		case Ast::STATEMENT_VARIABLE_DECLARATION: ScanVarDecl(statement, code, function); break;

		case Ast::STATEMENT_EXPRESSION:   ScanExpression(statement->expression, &code->scope); break;
		case Ast::STATEMENT_BRANCH_BLOCK: ScanBranchBlock(function, code, &statement->branch_block); break;
		case Ast::STATEMENT_ASSIGNMENT:   ScanAssignment(statement, code, function); break;

		case Ast::STATEMENT_ASSIGNMENT_ADD:
		case Ast::STATEMENT_ASSIGNMENT_SUBTRACT:
		case Ast::STATEMENT_ASSIGNMENT_MULTIPLY:
		case Ast::STATEMENT_ASSIGNMENT_DIVIDE:
		case Ast::STATEMENT_ASSIGNMENT_XOR: ScanBinaryAssignment(statement, code, function); break;
	}
}

void Scanner::ScanCode(Ast::Code* code, Ast::Scope* scope, Ast::Function* function) {
	code->scope.parent = scope;
	ScanScope(&code->scope);

	for (Ast::Statement* statement = code->statements.Begin(); statement < code->statements.End(); statement++) {
		ScanStatement(statement, code, function);
	}
}

TypeID Scanner::GetTypeFromParams(Array<Ast::Variable> params) {
	TypeID types[params.length];
	for (u32 i = 0; i < params.length; i++)
		types[i] = params[i].type;

	return GetTuple(types, params.length);
}

void Scanner::ScanFunction(Ast::Function* function, Ast::Scope* scope) {
	for (Ast::Variable* param = function->parameters; param < function->parameters.End(); param++) {
		param->type = GetType(param->ast_type, scope);
		function->code.scope.variables.Add(param);

		if (!param->type)
			Error(param->ast_type->basetype.token->location, "Unknown type '%'\n", param->ast_type->basetype.token);

		for (Ast::Variable* param_other = function->parameters; param_other < param; param_other++) {
			if (param_other->name == param->name)
				Error(param->name_token->location, "Duplicate parameter called '%'\n", param->name);
		}
	}

	if (function->ast_return_type) {
		function->return_type = GetType(function->ast_return_type, scope);

		if (!function->return_type)
			Error(function->ast_return_type->basetype.token->location, "Unknown type: %\n", function->ast_return_type);
	}
	else {
		function->return_type = TYPE_EMPTY_TUPLE;
	}

	TypeID param_type = GetTypeFromParams(function->parameters);
	function->type = GetFunctionType(param_type, function->return_type);
}

void Scanner::SemanticParse() {
	ScanScope(&module->scope);
}

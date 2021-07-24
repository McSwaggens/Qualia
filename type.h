#pragma once

#include "list.h"
#include "token.h"
#include "assert.h"

struct Ast_Struct;
struct Ast_Enum;

enum Type_Kind
{
	TYPE_BASETYPE_BOOL,
	TYPE_BASETYPE_UINT8,
	TYPE_BASETYPE_UINT16,
	TYPE_BASETYPE_UINT32,
	TYPE_BASETYPE_UINT64,
	TYPE_BASETYPE_INT8,
	TYPE_BASETYPE_INT16,
	TYPE_BASETYPE_INT32,
	TYPE_BASETYPE_INT64,
	TYPE_BASETYPE_FLOAT16,
	TYPE_BASETYPE_FLOAT32,
	TYPE_BASETYPE_FLOAT64,
	TYPE_BASETYPE_STRUCT,
	TYPE_BASETYPE_ENUM,
	TYPE_BASETYPE_TUPLE,
	TYPE_BASETYPE_FUNCTION,
	TYPE_SPECIFIER_POINTER,
	TYPE_SPECIFIER_OPTIONAL,
	TYPE_SPECIFIER_DYNAMIC_ARRAY,
	TYPE_SPECIFIER_FIXED_ARRAY
};

struct Type
{
	Type_Kind kind;

	union
	{
		Ast_Struct* structure;
		Ast_Enum*   enumeration;

		struct
		{
			Type* input;
			Type* output;
		} function;

		Array<Type*> tuple;
		Type* subtype;
	};

	Type* specifiers;
	u64 length; // Fixed array size
	u64 size;   // Size of the type in bytes

	// @Optimization: Combine these into single List?
	List<Type*> fixed_arrays;
	List<Type*> tuple_extensions;
	List<Type*> function_extensions;
};

extern Type empty_tuple;
extern Type type_bool;
extern Type type_int8;
extern Type type_int16;
extern Type type_int32;
extern Type type_int64;
extern Type type_uint8;
extern Type type_uint16;
extern Type type_uint32;
extern Type type_uint64;
extern Type type_float16;
extern Type type_float32;
extern Type type_float64;

static bool IsPrimitive(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_BOOL:
		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
		case TYPE_BASETYPE_INT8:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
			return true;
		default:
			return false;
	}
}

static bool IsInteger(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
		case TYPE_BASETYPE_INT8:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
			return true;
		default:
			return false;
	}
}

static bool IsSignedInteger(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_INT8:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
			return true;
		default:
			return false;
	}
}

static bool IsUnsignedInteger(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
			return true;
		default:
			return false;
	}
}

static bool IsFloat(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
			return true;
		default:
			return false;
	}
}

static bool IsNumerical(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_BOOL:
		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
		case TYPE_BASETYPE_INT8:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
		case TYPE_SPECIFIER_POINTER:
		case TYPE_BASETYPE_ENUM:
			return true;
		default:
			return false;
	}
}

static bool IsEnum(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_ENUM:
			return true;
		default:
			return false;
	}
}

static bool IsStruct(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_STRUCT:
			return true;
		default:
			return false;
	}
}

static bool IsPointer(Type* type)
{
	switch (type->kind)
	{
		case TYPE_SPECIFIER_POINTER:
			return true;
		default:
			return false;
	}
}

static bool IsOptional(Type* type)
{
	switch (type->kind)
	{
		case TYPE_SPECIFIER_OPTIONAL:
			return true;
		default:
			return false;
	}
}

static u32 GetTypePrecedence(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_TUPLE:
		case TYPE_BASETYPE_FUNCTION:
		case TYPE_BASETYPE_STRUCT:
		case TYPE_SPECIFIER_OPTIONAL:
		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
		case TYPE_SPECIFIER_FIXED_ARRAY:
			return 0;

		case TYPE_BASETYPE_BOOL:    return 1;

		case TYPE_BASETYPE_UINT8:   return 2;
		case TYPE_BASETYPE_UINT16:  return 3;
		case TYPE_BASETYPE_UINT32:  return 4;
		case TYPE_BASETYPE_UINT64:  return 5;

		case TYPE_BASETYPE_INT8:    return 6;
		case TYPE_BASETYPE_INT16:   return 7;
		case TYPE_BASETYPE_INT32:   return 8;
		case TYPE_BASETYPE_INT64:   return 9;

		case TYPE_BASETYPE_FLOAT16: return 10;
		case TYPE_BASETYPE_FLOAT32: return 11;
		case TYPE_BASETYPE_FLOAT64: return 12;

		case TYPE_BASETYPE_ENUM:
		case TYPE_SPECIFIER_POINTER:
			return 13;
	}
}

static constexpr Type* GetPrimitiveTypeFromTokenKind(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_BOOL:    return &type_bool;
		case TOKEN_INT:     return &type_int64;
		case TOKEN_INT8:    return &type_int8;
		case TOKEN_INT16:   return &type_int16;
		case TOKEN_INT32:   return &type_int32;
		case TOKEN_INT64:   return &type_int64;
		case TOKEN_UINT:    return &type_uint64;
		case TOKEN_UINT8:   return &type_uint8;
		case TOKEN_UINT16:  return &type_uint16;
		case TOKEN_UINT32:  return &type_uint32;
		case TOKEN_UINT64:  return &type_uint64;
		case TOKEN_FLOAT16: return &type_float16;
		case TOKEN_FLOAT32: return &type_float32;
		case TOKEN_FLOAT64: return &type_float64;

		default:
			Assert();
			Unreachable();
	}
}

static constexpr Type* GetPrimitiveTypeFromKind(Type_Kind kind)
{
	switch (kind)
	{
		case TYPE_BASETYPE_BOOL:    return &type_bool;
		case TYPE_BASETYPE_UINT8:   return &type_uint8;
		case TYPE_BASETYPE_UINT16:  return &type_uint16;
		case TYPE_BASETYPE_UINT32:  return &type_uint32;
		case TYPE_BASETYPE_UINT64:  return &type_uint64;
		case TYPE_BASETYPE_INT8:    return &type_int8;
		case TYPE_BASETYPE_INT16:   return &type_int16;
		case TYPE_BASETYPE_INT32:   return &type_int32;
		case TYPE_BASETYPE_INT64:   return &type_int64;
		case TYPE_BASETYPE_FLOAT16: return &type_float16;
		case TYPE_BASETYPE_FLOAT32: return &type_float32;
		case TYPE_BASETYPE_FLOAT64: return &type_float64;

		default:
			Assert();
			Unreachable();
	}
}

static constexpr Type_Kind GetUnsignedVersionOf(Type_Kind kind)
{
	switch (kind)
	{
		case TYPE_BASETYPE_INT8:  return TYPE_BASETYPE_UINT8;
		case TYPE_BASETYPE_INT16: return TYPE_BASETYPE_UINT16;
		case TYPE_BASETYPE_INT32: return TYPE_BASETYPE_UINT32;
		case TYPE_BASETYPE_INT64: return TYPE_BASETYPE_UINT64;

		default:
			Assert();
			Unreachable();
	}
}

static constexpr Type_Kind GetSignedVersionOf(Type_Kind kind)
{
	switch (kind)
	{
		case TYPE_BASETYPE_UINT8:  return TYPE_BASETYPE_INT8;
		case TYPE_BASETYPE_UINT16: return TYPE_BASETYPE_INT16;
		case TYPE_BASETYPE_UINT32: return TYPE_BASETYPE_INT32;
		case TYPE_BASETYPE_UINT64: return TYPE_BASETYPE_INT64;

		default:
			Assert();
			Unreachable();
	}
}

// @RemoveMe:
//  The idea of a "dominant" type needs to be replaced with something else.
//  int8 + uint64 = int64
//  *int + int = *int
//  *int - int =  int
static Type* GetDominantType(Type* a, Type* b)
{
	return GetTypePrecedence(a) >= GetTypePrecedence(b) ? a : b;
}

void InitTypeSystem();
Type* GetPointer(Type* type);
Type* GetOptional(Type* type);
Type* GetDynamicArray(Type* type);
Type* GetFixedArray(Type* type, u64 length);

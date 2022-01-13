#pragma once

#include "list.h"
#include "token.h"
#include "assert.h"

struct Ast_Struct;
struct Ast_Enum;

enum Type_Kind : u8
{
	TYPE_BASETYPE_BYTE,
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

struct Type;

struct Type_Extension
{
	Type* type;
	Type_Kind kind;

	union
	{
		Type* output;
		u64 length;
	};
};

struct Type
{
	Type_Kind kind;

	union
	{
		Type* subtype;

		Ast_Struct* structure;
		Ast_Enum*   enumeration;

		struct
		{
			Type* input;
			Type* output;
		};

		struct
		{
			u16 recursive_count;
			Array<Type*> tuple;
		};
	};

	Type* specifiers;
	u64 length; // Fixed array size
	u64 size;   // Size of the type in bytes

	List<Type_Extension> extensions;
};

extern Type empty_tuple;
extern Type type_byte;
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
		case TYPE_BASETYPE_BYTE:
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

static bool IsArithmetic(Type* type)
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
		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
		case TYPE_SPECIFIER_POINTER:
			return true;
		default:
			return false;
	}
}

static u8 GetTypePrecedence(Type* type)
{
	switch (type->kind)
	{
		case TYPE_BASETYPE_TUPLE:
		case TYPE_BASETYPE_FUNCTION:
		case TYPE_BASETYPE_STRUCT:
		case TYPE_SPECIFIER_OPTIONAL:
		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
		case TYPE_SPECIFIER_FIXED_ARRAY:
			return 10;

		case TYPE_BASETYPE_BYTE:     return 20;
		case TYPE_BASETYPE_ENUM:     return 21;

		case TYPE_BASETYPE_UINT8:    return 30;
		case TYPE_BASETYPE_UINT16:   return 31;
		case TYPE_BASETYPE_UINT32:   return 32;
		case TYPE_BASETYPE_UINT64:   return 33;

		case TYPE_BASETYPE_INT8:     return 60;
		case TYPE_BASETYPE_INT16:    return 61;
		case TYPE_BASETYPE_INT32:    return 62;
		case TYPE_BASETYPE_INT64:    return 63;

		case TYPE_BASETYPE_FLOAT16:  return 70;
		case TYPE_BASETYPE_FLOAT32:  return 71;
		case TYPE_BASETYPE_FLOAT64:  return 72;

		case TYPE_SPECIFIER_POINTER: return 80;

		case TYPE_BASETYPE_BOOL:     return 255;
	}
}

static constexpr Type* GetPrimitiveTypeFromTokenKind(Token_Kind kind)
{
	switch (kind)
	{
		case TOKEN_BYTE:    return &type_byte;
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
		case TYPE_BASETYPE_BYTE:    return &type_byte;
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

static bool IsSpecifier(Type* type)
{
	return type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_SPECIFIER_OPTIONAL
		|| type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY
		|| type->kind == TYPE_SPECIFIER_FIXED_ARRAY;
}

static bool IsBaseType(Type* type)
{
	return !IsSpecifier(type);
}

static Type* FindBaseType(Type* type)
{
	while (IsSpecifier(type)) type = type->subtype;
	return type;
}

static Type* GetOptimalInteger(Type* a, Type* b)
{
	if (a == b) return a;

	if (IsSignedInteger(a) != IsSignedInteger(b))
	{
		if (IsSignedInteger(a))
		{
			b = GetPrimitiveTypeFromKind(GetSignedVersionOf(b->kind));
		}
		else
		{
			a = GetPrimitiveTypeFromKind(GetSignedVersionOf(a->kind));
		}
	}

	return a->size >= b->size ? a : b;
}

static Type* GetDominantType(Type* a, Type* b)
{
	if (a == b) return a;

	if (IsInteger(a) && IsInteger(b))
	{
		return GetOptimalInteger(a, b);
	}

	return GetTypePrecedence(a) >= GetTypePrecedence(b) ? a : b;
}

static bool IsConvertableToArithmeticType(Type* type)
{
	return IsArithmetic(type) || type->kind == TYPE_BASETYPE_BOOL || type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsEnum(Type* type)
{
	return type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsStruct(Type* type)
{
	return type->kind == TYPE_BASETYPE_STRUCT;
}

static bool IsPointer(Type* type)
{
	return type->kind == TYPE_SPECIFIER_POINTER;
}

static bool IsOptional(Type* type)
{
	return type->kind == TYPE_SPECIFIER_OPTIONAL;
}

static bool IsFunctionPointer(Type* type)
{
	return type->kind == TYPE_SPECIFIER_POINTER
		&& type->subtype->kind == TYPE_BASETYPE_FUNCTION;
}

static bool IsBytePointer(Type* type)
{
	return type->kind == TYPE_SPECIFIER_POINTER
		&& type->subtype->kind == TYPE_BASETYPE_BYTE;
}

static bool IsDynamicByteArray(Type* type)
{
	return type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY
		&& type->subtype->kind == TYPE_BASETYPE_BYTE;
}

static bool IsFixedByteArray(Type* type)
{
	return type->kind == TYPE_SPECIFIER_FIXED_ARRAY
		&& type->subtype->kind == TYPE_BASETYPE_BYTE;
}

static bool IsDynamicArray(Type* type)
{
	return type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY;
}

static bool IsFixedArray(Type* type)
{
	return type->kind == TYPE_SPECIFIER_FIXED_ARRAY;
}

static void InitTypeSystem();
static Type* GetPointer(Type* type);
static Type* GetOptional(Type* type);
static Type* GetDynamicArray(Type* type);
static Type* GetFixedArray(Type* type, u64 length);
static Type* GetTuple(Array<Type*> types);
static Type* GetFunctionType(Type* input, Type* output);
static Type* MergeTypeRight(Type* a, Type* b);
static bool CanImplicitCast(Type* from, Type* to);


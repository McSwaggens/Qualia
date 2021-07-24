#include "type.h"
#include "memory.h"
#include "assert.h"

static consteval Type NewPrimitiveType(Type_Kind kind, u64 size)
{
	Type type;
	type.kind = kind;
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

Type empty_tuple  = NewEmptyTupleType();
Type type_bool    = NewPrimitiveType(TYPE_BASETYPE_BOOL,    1);
Type type_int8    = NewPrimitiveType(TYPE_BASETYPE_INT8,    1);
Type type_int16   = NewPrimitiveType(TYPE_BASETYPE_INT16,   2);
Type type_int32   = NewPrimitiveType(TYPE_BASETYPE_INT32,   4);
Type type_int64   = NewPrimitiveType(TYPE_BASETYPE_INT64,   8);
Type type_uint8   = NewPrimitiveType(TYPE_BASETYPE_UINT8,   1);
Type type_uint16  = NewPrimitiveType(TYPE_BASETYPE_UINT16,  2);
Type type_uint32  = NewPrimitiveType(TYPE_BASETYPE_UINT32,  4);
Type type_uint64  = NewPrimitiveType(TYPE_BASETYPE_UINT64,  8);
Type type_float16 = NewPrimitiveType(TYPE_BASETYPE_FLOAT16, 2);
Type type_float32 = NewPrimitiveType(TYPE_BASETYPE_FLOAT32, 4);
Type type_float64 = NewPrimitiveType(TYPE_BASETYPE_FLOAT64, 8);

Stack_Allocator type_allocator;

void InitTypeSystem()
{
	type_allocator = NewStackAllocator();
}

void InitSpecifiers(Type* type)
{
	if (type->specifiers) return;

	type->specifiers = type_allocator.Allocate<Type>(3);
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

Type* GetPointer(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 0;
}

Type* GetOptional(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 1;
}

Type* GetDynamicArray(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 2;
}

Type* GetFixedArray(Type* type, u64 length)
{
	for (u32 i = 0; i < type->fixed_arrays.count; i++)
	{
		if (type->fixed_arrays[i]->length == length)
		{
			return type->fixed_arrays[i];
		}
	}

	Type* new_type = type_allocator.Allocate<Type>();
	ZeroMemory(new_type);
	new_type->kind = TYPE_SPECIFIER_FIXED_ARRAY;
	new_type->subtype = type;
	new_type->length = length;
	new_type->size = type->size * length;
	type->fixed_arrays.Add(new_type);

	return new_type;
}


#include "type.h"
#include "parser.h"
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

static consteval Type NewSpecifier(Type_Kind kind, u64 size, Type* subtype)
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

Type empty_tuple  = NewEmptyTupleType();
Type type_byte    = NewPrimitiveType(TYPE_BASETYPE_BYTE,    1);
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

// Type type_byte_pointer       = NewSpecifier(TYPE_SPECIFIER_POINTER,        8, &type_byte);
// Type type_byte_optional      = NewSpecifier(TYPE_SPECIFIER_OPTIONAL,       2, &type_byte);
// Type type_byte_dynamic_array = NewSpecifier(TYPE_SPECIFIER_DYNAMIC_ARRAY, 16, &type_byte);

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

Type* GetTuple(Array<Type*> types)
{
	if (types.count == 0)
	{
		return &empty_tuple;
	}

	if (types.count == 1)
	{
		return types[0];
	}

	Type* first = types[0];

	for (u32 i = 0; i < first->tuple_extensions.count; i++)
	{
		Type* type = first->tuple_extensions[i];
		bool fail = false;

		if (type->tuple.count != types.count) continue;

		for (u32 j = 0; j < type->tuple.count; j++)
		{
			if (type->tuple[j] != types[j])
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

	Type* type = type_allocator.Allocate<Type>();
	Type** tuple_members = type_allocator.Allocate<Type*>(types.count);
	ZeroMemory(type);
	type->kind = TYPE_BASETYPE_TUPLE;

	u32 recursive_count = types.count;

	for (u32 i = 0; i < types.count; i++)
	{
		tuple_members[i] = types[i];
		type->size += types[i]->size;

		if (types[i]->kind == TYPE_BASETYPE_TUPLE)
		{
			recursive_count += types[i]->tuple.count - 1;
		}
	}

	type->tuple = Array(tuple_members, types.count);
	type->recursive_count = recursive_count;
	first->tuple_extensions.Add(type);
	return type;
}

Type* GetFunctionType(Type* input, Type* output)
{
	for (u32 i = 0; i < input->function_extensions.count; i++)
	{
		Type* func = input->function_extensions[i];
		if (func->function.output == output)
		{
			return func;
		}
	}

	Type* new_func = type_allocator.Allocate<Type>();
	ZeroMemory(new_func);
	new_func->kind = TYPE_BASETYPE_FUNCTION;
	new_func->function.input = input;
	new_func->function.output = output;
	new_func->size = 0;
	input->function_extensions.Add(new_func);

	return new_func;
}

bool IsConvertableTo(Type* from, Type* to)
{
	if (from == to) return true;

	if (IsFixedByteArray(from) ||
		IsFixedByteArray(to))
		return from->size == to->size;

	switch (from->kind)
	{
		case TYPE_BASETYPE_BYTE:
			return to->size == 1;

		case TYPE_BASETYPE_BOOL:
			return IsInteger(to)
				|| IsFloat(to)
				|| to->kind == TYPE_BASETYPE_BYTE
				|| to->kind == TYPE_BASETYPE_ENUM;

		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_INT8:
			if (to->kind == TYPE_BASETYPE_BYTE) return true;

		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
			return IsInteger(to)
				|| IsFloat(to)
				|| to->kind == TYPE_BASETYPE_BOOL
				|| (to->kind == TYPE_BASETYPE_ENUM && IsConvertableTo(from, to->enumeration->underlying_type));

		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
			return IsFloat(to)
				|| IsInteger(to)
				|| to->kind == TYPE_BASETYPE_BOOL;

		case TYPE_BASETYPE_STRUCT:
			return false;

		case TYPE_BASETYPE_ENUM:
			return IsConvertableTo(from->enumeration->underlying_type, to);

		case TYPE_BASETYPE_TUPLE:
			if (to->kind == TYPE_BASETYPE_TUPLE && to->tuple.count != from->tuple.count)
			{
				for (u32 i = 0; i < to->tuple.count; i++)
				{
					if (!IsConvertableTo(from->tuple[i], to->tuple[i]))
					{
						return false;
					}
				}

				return true;
			}

			return false;

		case TYPE_BASETYPE_FUNCTION:
			return false;

		case TYPE_SPECIFIER_POINTER:
			return IsBytePointer(from) || IsBytePointer(to);

		case TYPE_SPECIFIER_OPTIONAL:
			return to->kind == TYPE_BASETYPE_BOOL
				|| IsConvertableTo(from, to->subtype);

		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
			return false;

		case TYPE_SPECIFIER_FIXED_ARRAY:
			return to->kind == TYPE_SPECIFIER_FIXED_ARRAY
				&& from->length == to->length
				&& IsConvertableTo(from->subtype, to->subtype);
	}

	return false;
}


#include "type.h"
#include "memory.h"

// @Fixme @Threading: The entire type system is written as implied single thread, needs to be fixed.

static consteval Type NewPrimitiveType(Type_Kind kind, uint64 size)
{
	Type type;
	type.kind = kind;
	type.size = size;
	type.length = 0;
	type.specifiers = null;
	type.extensions = null;
	return type;
}

Type empty_tuple  = NewPrimitiveType(TYPE_BASETYPE_TUPLE,   0);
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

static Stack type_stack;

static void InitTypeSystem()
{
	type_stack = CreateStack(1 << 21);
}

static void InitSpecifiers(Type* type)
{
	if (type->specifiers) return;

	type->specifiers = StackAllocate<Type>(&type_stack, 3);
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

static Type* GetPointer(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 0;
}

static Type* GetOptional(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 1;
}

static Type* GetDynamicArray(Type* type)
{
	InitSpecifiers(type);
	return type->specifiers + 2;
}

static Type* GetFunctionType(Type* input, Type* output)
{
	for (uint32 i = 0; i < input->extensions.count; i++)
	{
		Type_Extension extension = input->extensions[i];
		if (extension.output == output && extension.kind == TYPE_BASETYPE_FUNCTION) // @Optimization: Bundle the output type with the pointer, this is cache-miss-city.
		{
			return extension.type;
		}
	}

	Type* type = StackAllocate<Type>(&type_stack);
	ZeroMemory(type);

	type->kind = TYPE_BASETYPE_FUNCTION;
	type->input = input;
	type->output = output;
	type->size = 0; // @Note: (X)->Y does not have a size, *(X)->Y does.

	Type_Extension extension;
	ZeroMemory(&extension);
	extension.kind = TYPE_BASETYPE_FUNCTION;
	extension.output = type;
	input->extensions.Add(extension);

	return type;
}

static Type* GetTuple(Array<Type*> elements)
{
	if (elements.count == 0)
	{
		return &empty_tuple;
	}

	if (elements.count == 1)
	{
		return elements[0];
	}

	Type* first = elements[0];

	for (uint32 i = 0; i < first->extensions.count; i++)
	{
		Type_Extension extension = first->extensions[i];

		if (extension.kind == TYPE_BASETYPE_TUPLE && extension.length == elements.count)
		{
			bool fail = false;
			for (uint32 j = 1; j < extension.type->tuple.count; j++)
			{
				if (extension.type->tuple[j] != elements[j])
				{
					fail = true;
					break;
				}
			}

			if (!fail)
			{
				return extension.type;
			}
		}
	}

	Type* type = StackAllocate<Type>(&type_stack);
	Type** tuple_members = StackAllocate<Type*>(&type_stack, elements.count);
	ZeroMemory(type);
	type->kind = TYPE_BASETYPE_TUPLE;

	uint32 recursive_count = elements.count;

	for (uint32 i = 0; i < elements.count; i++)
	{
		tuple_members[i] = elements[i];
		type->size += elements[i]->size;

		if (elements[i]->kind == TYPE_BASETYPE_TUPLE)
		{
			recursive_count += elements[i]->recursive_count - 1;
		}
	}

	type->tuple = Array(tuple_members, elements.count);
	type->recursive_count = recursive_count;

	// Print("Recursive count of % is %\n", type, type->recursive_count);

	Type_Extension extension;
	ZeroMemory(&extension);
	extension.kind = TYPE_BASETYPE_TUPLE;
	extension.type = type;
	extension.length = elements.count;
	first->extensions.Add(extension);

	return type;
}

static Type* GetFixedArray(Type* base, uint64 length)
{
	for (uint32 i = 0; i < base->extensions.count; i++)
	{
		if (base->extensions[i].kind == TYPE_SPECIFIER_FIXED_ARRAY && base->extensions[i].length == length)
		{
			return base->extensions[i].type;
		}
	}

	Type* new_type = StackAllocate<Type>(&type_stack);
	ZeroMemory(new_type);
	new_type->kind = TYPE_SPECIFIER_FIXED_ARRAY;
	new_type->subtype = base;
	new_type->length = length;
	new_type->size = base->size * length;

	Type_Extension extension;
	ZeroMemory(&extension);
	extension.kind = TYPE_SPECIFIER_FIXED_ARRAY;
	extension.length = length;
	extension.type = new_type;
	base->extensions.Add(extension);

	return new_type;
}

// () + A = A
// A + () = A
// A + B = (A, B)
// (A, B) + C = ((A, B), C)
// A + (B, C) = (A, B, C)
static Type* MergeTypeRight(Type* a, Type* b)
{
	if (a == &empty_tuple) COLD
		return b;

	if (b == &empty_tuple) COLD
		return a;

	if (b->kind != TYPE_BASETYPE_TUPLE)
	{
		Type* types[2];
		types[0] = a;
		types[1] = b;
		return GetTuple(Array(types, 2));
	}

	uint32 count = 1 + b->tuple.count;
	Type* types[count];

	types[0] = a;
	CopyMemory(types+1, b->tuple.elements, b->tuple.count);

	return GetTuple(Array(types, count));
}

static bool CanImplicitCast(Type* from, Type* to)
{
	if (from == to)
		return true;

	if (IsFixedByteArray(from) || IsFixedByteArray(to) || to->kind == TYPE_BASETYPE_BYTE)
		return from->size == to->size;

	switch (from->kind)
	{
		case TYPE_BASETYPE_BYTE:
			return to->size == 1;

		case TYPE_BASETYPE_BOOL:
		case TYPE_BASETYPE_UINT8:
		case TYPE_BASETYPE_INT8:
		case TYPE_BASETYPE_UINT16:
		case TYPE_BASETYPE_UINT32:
		case TYPE_BASETYPE_UINT64:
		case TYPE_BASETYPE_INT16:
		case TYPE_BASETYPE_INT32:
		case TYPE_BASETYPE_INT64:
			return IsInteger(to)
				|| IsFloat(to)
				|| to->kind == TYPE_BASETYPE_BOOL
				|| (to->kind == TYPE_BASETYPE_ENUM && CanImplicitCast(from, to->enumeration->underlying_type));

		case TYPE_BASETYPE_FLOAT16:
		case TYPE_BASETYPE_FLOAT32:
		case TYPE_BASETYPE_FLOAT64:
			return IsFloat(to)
				|| IsInteger(to)
				|| to->kind == TYPE_BASETYPE_BOOL;

		case TYPE_BASETYPE_STRUCT:
			return false;

		case TYPE_BASETYPE_ENUM:
			return CanImplicitCast(from->enumeration->underlying_type, to);

		case TYPE_BASETYPE_TUPLE:
			if (to->kind == TYPE_BASETYPE_TUPLE && to->tuple.count == from->tuple.count)
			{
				for (uint32 i = 0; i < to->tuple.count; i++)
				{
					if (!CanImplicitCast(from->tuple[i], to->tuple[i]))
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
			return to->kind == TYPE_BASETYPE_BOOL
				|| IsBytePointer(to)
				|| (IsBytePointer(from) && IsPointer(to));

		case TYPE_SPECIFIER_OPTIONAL:
			return to->kind == TYPE_BASETYPE_BOOL
				|| CanImplicitCast(from, to->subtype);

		case TYPE_SPECIFIER_DYNAMIC_ARRAY:
			return false;

		case TYPE_SPECIFIER_FIXED_ARRAY:
			return (to->kind == TYPE_SPECIFIER_FIXED_ARRAY
				&& from->length == to->length
				&& CanImplicitCast(from->subtype, to->subtype));
				// || (from->length == 1 && CanImplicitCast(from->subtype, to->subtype));
	}

	return false;
}

static bool CanExplicitCast(Type* from, Type* to)
{
	return CanImplicitCast(from, to);
}

static Type* GetDominantType(Type* a, Type* b)
{
	if (a == b) return a;

	if (IsInteger(a) && IsInteger(b))
	{
		return GetOptimalInteger(a, b);
	}

	uint32 precedence_a = GetTypePrecedence(a);
	uint32 precedence_b = GetTypePrecedence(b);

	bool ab = CanImplicitCast(a, b);
	bool ba = CanImplicitCast(b, a);

	if (ab && ba) return precedence_a >= precedence_b ? a : b;

	if (ab) return a;
	if (ba) return b;

	return null;
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

static inline bool IsDataType(Type* type)
{
	return type != &empty_tuple && type->kind != TYPE_BASETYPE_FUNCTION;
}

static bool CanForceCast(Type* from, Type* to)
{
	return from == to
		|| from->size == to->size && IsDataType(from) && IsDataType(to);
}


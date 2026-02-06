#include "type_system.h"
#include "memory.h"
#include "general.h"
#include "assert.h"

static TypeID GetArithmeticBackingType(TypeID id) {
	if (id == TYPE_BOOL)
		return TYPE_INT8;

	TypeKind kind = GetTypeKind(id);
	TypeInfo* info = GetTypeInfo(id);

	if (kind == TYPE_ENUM)
		return info->enum_info.backing_type;

	return id;
}

static TypeID GetIntegerWithSign(TypeID type, bool sign) {
	if (sign) switch (type) {
		case TYPE_UINT8:  return TYPE_INT8;
		case TYPE_UINT16: return TYPE_INT16;
		case TYPE_UINT32: return TYPE_INT32;
		case TYPE_UINT64: return TYPE_INT64;
		default: return type;
	}
	else switch (type) {
		case TYPE_INT8:  return TYPE_UINT8;
		case TYPE_INT16: return TYPE_UINT16;
		case TYPE_INT32: return TYPE_UINT32;
		case TYPE_INT64: return TYPE_UINT64;
		default: return type;
	}
}

static TypeID GetDominantType(TypeID a, TypeID b) {
	if (a == b)
		return a;

	if (a < b)
		Swap(a, b);

	if (IsInteger(a) && IsInteger(b)) {
		bool sign = IsSignedInteger(a) && IsSignedInteger(b);
		return GetIntegerWithSign(a, sign);
	}

	if (GetTypeKind(a) == TYPE_PRIMITIVE && GetTypeKind(b) == TYPE_PRIMITIVE) {
	}

	Assert("Invalid dominator combo");
	return TYPE_NULL;
}

static bool CanCast(CastKind cast, TypeID from, TypeID to) {
	TypeKind from_kind = GetTypeKind(from);
	TypeKind to_kind   = GetTypeKind(to);

	TypeInfo* from_info = GetTypeInfo(from);
	TypeInfo* to_info   = GetTypeInfo(to);

	if (from == to)
		return true;

	if (from == TYPE_BYTE)
		return GetTypeSize(to) == 1;

	if (to == TYPE_BYTE)
		return GetTypeSize(from) == 1;

	switch (from_kind) {
		case TYPE_PRIMITIVE:
		{
			if (to_kind == TYPE_PRIMITIVE)
				return cast >= CAST_IMPLICIT;

			if (to_kind == TYPE_ENUM)
				return cast >= CAST_EXPLICIT;

			return false;
		}

		case TYPE_TUPLE:
		{
			if (to_kind == TYPE_TUPLE && cast >= CAST_COERCIVE && from_info->tuple_info.elements.length == to_info->tuple_info.elements.length) {
				for (u64 i = 0; i < from_info->tuple_info.elements.length; i++) {
					if (!CanCast(cast, from_info->tuple_info.elements[i], to_info->tuple_info.elements[i]))
						return false;
				}

				return true;
			}
		} return false;

		case TYPE_FUNCTION: return false;
		case TYPE_STRUCT:   return false;

		case TYPE_ENUM:
		{
			TypeID backing_type = from_info->enum_info.backing_type;

			switch (to) {
				case TYPE_BOOL:
					return cast >= CAST_COERCIVE;

				case TYPE_UINT8:
				case TYPE_UINT16:
				case TYPE_UINT32:
				case TYPE_UINT64:
				case TYPE_INT8:
				case TYPE_INT16:
				case TYPE_INT32:
				case TYPE_INT64:
					return cast >= CAST_EXPLICIT;

				default:
					return false;
			}

			return false;
		}

		case TYPE_POINTER:
		{
			if (to_kind == TYPE_POINTER) {
				if (from == TYPE_POINTER_BYTE || to == TYPE_POINTER_BYTE)
					return cast >= CAST_IMPLICIT;

				return cast >= CAST_EXPLICIT;
			}

			switch (to) {
				case TYPE_BOOL:
					return cast >= CAST_COERCIVE;

				case TYPE_UINT8:
				case TYPE_UINT16:
				case TYPE_UINT32:
				case TYPE_UINT64:
				case TYPE_INT8:
				case TYPE_INT16:
				case TYPE_INT32:
				case TYPE_INT64:
					return cast >= CAST_EXPLICIT;

				default:
					return false;
			}

			return false;
		}

		case TYPE_OPTIONAL:
		{
			if (to == TYPE_BOOL)
				return cast >= CAST_COERCIVE;

			TypeID subtype = GetSubType(from);

			if (to == subtype)
				return cast >= CAST_EXPLICIT;

			return false;
		}

		case TYPE_ARRAY:
			return false;

		case TYPE_FIXED_ARRAY:
		{
			if (from_info->fixed_info.length == 1 && to == GetSubType(from))
				return true;

			if (to_kind == TYPE_FIXED_ARRAY && from_info->fixed_info.length == to_info->fixed_info.length)
				return CanCast(cast, GetSubType(from), GetSubType(to));

			return false;
		}
	}

	return false;
}

static void AddExtensionEntry(ExtensionTable* table, ExtensionEntry entry) {
	if (!table->count || IsPow2(table->count & -16)) {
		table->entries = (ExtensionEntry*)ReAllocMemory(
			table->entries,
			sizeof(ExtensionEntry) * table->count,
			sizeof(ExtensionEntry) * RaisePow2(table->count|16)
		);
	}

	table->entries[table->count++] = entry;
}

static void InitTypeSystem(void) {
	ZeroMemory(&type_system);
	type_system.info_count = CORE_TYPES_COUNT;
	type_system.info_capacity = 1<<20;
	type_system.infos = (TypeInfo*)AllocMemory(type_system.info_capacity*sizeof(TypeInfo));

	for (s32 prim = PRIMITIVE_BEGIN; prim < PRIMITIVE_END; prim++) {
		TypeInfo* info = &type_system.infos[prim];
		ZeroMemory(info);

		info->size = PRIMITIVE_SIZE_LUT[prim];

		info->pointer  = CreateTypeID(TYPE_POINTER,  TYPE_POINTER_TO_PRIMITIVE_OFFSET  + prim);
		info->optional = CreateTypeID(TYPE_OPTIONAL, TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + prim);
		info->array    = CreateTypeID(TYPE_ARRAY,    TYPE_ARRAY_TO_PRIMITIVE_OFFSET    + prim);

		TypeInfo* pointer  = GetTypeInfo(info->pointer);
		TypeInfo* optional = GetTypeInfo(info->optional);
		TypeInfo* array    = GetTypeInfo(info->array);

		ZeroMemory(pointer);
		ZeroMemory(optional);
		ZeroMemory(array);

		pointer->size  = 8;
		optional->size = 1 + info->size;
		array->size    = 16;

		pointer->pointer_info.subtype   = (TypeID)prim;
		optional->optional_info.subtype = (TypeID)prim;
		array->array_info.subtype       = (TypeID)prim;
	}

	*GetTypeInfo(TYPE_BARE_FUNCTION) = {
		.size = 0,
		.function_info = {
			.input  = TYPE_EMPTY_TUPLE,
			.output = TYPE_EMPTY_TUPLE,
		}
	};

	TypeInfo* empty = GetTypeInfo(TYPE_EMPTY_TUPLE);
	*empty = {
		.size = 0,
		.tuple_info = {
			.elements = { }
		}
	};

	AddExtensionEntry(
		&empty->extensions,
		(ExtensionEntry){
			.type = TYPE_BARE_FUNCTION,
			.output = TYPE_EMPTY_TUPLE,
		}
	);
}

static TypeID CreateType(TypeKind kind, TypeInfo info) {
	if (type_system.info_count == type_system.info_capacity) COLD
	{
		type_system.info_capacity <<= 4;
		type_system.infos = (TypeInfo*)ReAllocMemory(
			type_system.infos,
			sizeof(TypeInfo) * type_system.info_count,
			sizeof(TypeInfo) * (type_system.info_capacity)
		);
	}

	TypeID result = CreateTypeID(kind, type_system.info_count);
	type_system.infos[type_system.info_count++] = info;

	return result;
}

static TypeID GetPointer(TypeID subtype) {
	Assert(subtype);

	TypeInfo* info = GetTypeInfo(subtype);

	if (info->pointer) HOT
		return info->pointer;

	TypeID ptr = CreateType(TYPE_POINTER, {
		.size = 8,
		.pointer_info.subtype = subtype,
	});

	GetTypeInfo(subtype)->pointer = ptr;

	return ptr;
}

static TypeID GetOptional(TypeID subtype) {
	Assert(subtype);

	TypeInfo* info = GetTypeInfo(subtype);

	if (info->optional)
		return info->optional;

	TypeID result = CreateType(TYPE_OPTIONAL, {
		.size = GetTypeSize(subtype) + 1,
		.optional_info.subtype = subtype,
	});

	GetTypeInfo(subtype)->optional = result;

	return result;
}

static TypeID GetArray(TypeID subtype) {
	Assert(subtype);

	TypeInfo* info = GetTypeInfo(subtype);

	if (info->array)
		return info->array;

	TypeID result = CreateType(TYPE_ARRAY, {
		.size = 16,
		.array_info.subtype = subtype,
	});

	GetTypeInfo(subtype)->array = result;

	return result;
}

static TypeID GetFixedArray(TypeID subtype, u64 length) {
	Assert(subtype);

	TypeInfo* info = GetTypeInfo(subtype);

	for (u32 i = 0; i < info->extensions.count; i++) {
		ExtensionEntry* entry = &info->extensions.entries[i];

		if (entry->length == length && GetTypeKind(entry->type) == TYPE_FIXED_ARRAY)
			return entry->type;
	}

	TypeID result = CreateType(TYPE_FIXED_ARRAY, {
		.size = length * GetTypeSize(subtype),
		.fixed_info = {
			.length = length,
		}
	});

	AddExtensionEntry(
		&info->extensions,
		(ExtensionEntry){
			.type = result,
			.length = length,
		}
	);

	return result;
}

static TypeID GetFunctionType(TypeID input, TypeID output) {
	Assert(input);
	Assert(output);

	TypeInfo* input_info = GetTypeInfo(input);

	for (u32 i = 0; i < input_info->extensions.count; i++) {
		ExtensionEntry* entry = &input_info->extensions.entries[i];

		if (entry->output == output && GetTypeKind(entry->type) == TYPE_FUNCTION)
			return entry->type;
	}

	TypeID result = CreateType(TYPE_FUNCTION, {
		.size = 0,
		.function_info = {
			.input  = input,
			.output = output,
		}
	});

	AddExtensionEntry(
		&input_info->extensions,
		(ExtensionEntry){
			.type = result,
			.output = output,
		}
	);

	return result;
}

static TypeID GetTuple(Array<TypeID> elements) {
	if (!elements.length)
		return TYPE_EMPTY_TUPLE;

	if (elements.length == 1)
		return elements[0];

	TypeInfo* header_info = GetTypeInfo(elements[0]);
	ExtensionTable* table = &header_info->extensions;

	for (s32 i = 0; i < table->count; i++) {
		ExtensionEntry* entry = &table->entries[i];

		if (entry->length != elements.length)
			continue;

		if (GetTypeKind(entry->type) != TYPE_TUPLE)
			continue;

		TypeInfo* ext_info = GetTypeInfo(entry->type);
		if (elements == ext_info->tuple_info.elements)
			return entry->type;
	}

	u64 size = 0;
	for (s32 i = 0; i < elements.length; i++)
		size += GetTypeSize(elements[i]);

	TypeID* new_elements = CopyAlloc(elements.data, elements.length);

	TypeID result = CreateType(TYPE_TUPLE, {
		.size = size,
		.tuple_info.elements = { new_elements, elements.length }
	});

	AddExtensionEntry(table, {
		.type = result,
		.length = elements.length,
	});

	return result;
}

static TypeID GetTuple(TypeID* elements, u64 count) {
	return GetTuple({ elements, count });
}

static TypeID CreateStructType(Ast::Struct* ast, u64 size) {
	TypeID result = CreateType(TYPE_STRUCT, {
		.size = size,
		.struct_info.ast = ast,
	});

	return result;
}

static TypeID CreateEnumType(Ast::Enum* ast, TypeID backing_type) {
	TypeID result = CreateType(TYPE_ENUM, {
		.size = GetTypeSize(backing_type),
		.enum_info.ast = ast,
		.enum_info.backing_type = backing_type,
	});

	return result;
}

// () + () = ()
// () + A = A
// A + () = A
// A + B = (A, B)
// (A, B) + C = ((A, B), C)
// A + (B, C) = (A, B, C)
static TypeID MergeTypeRight(TypeID a, TypeID b) {
	if (a == TYPE_EMPTY_TUPLE)
		return b;

	if (b == TYPE_EMPTY_TUPLE)
		return a;

	if (GetTypeKind(b) != TYPE_TUPLE) {
		TypeID types[2] = { a, b };
		return GetTuple(Array<TypeID>(types, 2));
	}

	TypeInfo* b_info = GetTypeInfo(b);
	u64 count = b_info->tuple_info.elements.length+1;

	TypeID elements[count];
	elements[0] = a;
	Copy(elements+1, b_info->tuple_info.elements.Begin(), count-1);

	return GetTuple(Array<TypeID>(elements, count));
}


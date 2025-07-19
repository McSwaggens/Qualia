#pragma once

#include "int.h"

enum TypeKind : u32 {
	TYPE_PRIMITIVE   = 0,

	TYPE_TUPLE       = 1,
	TYPE_FUNCTION    = 2,

	TYPE_STRUCT      = 3,
	TYPE_ENUM        = 4,

	TYPE_POINTER     = 5,
	TYPE_OPTIONAL    = 6,
	TYPE_ARRAY       = 7,

	TYPE_FIXED_ARRAY = 8,
};

static const u32 TYPE_BITCOUNT       = sizeof(u32)*8;
static const u32 TYPE_KIND_BITCOUNT  = 4;
static const u32 TYPE_INDEX_BITCOUNT = TYPE_BITCOUNT - TYPE_KIND_BITCOUNT;

static const u32 PRIMITIVE_COUNT = 12;
static const u32 PRIMITIVE_BEGIN = 1;
static const u32 PRIMITIVE_END   = PRIMITIVE_BEGIN + PRIMITIVE_COUNT;
static const u32 TYPE_POINTER_TO_PRIMITIVE_OFFSET  = PRIMITIVE_COUNT*1;
static const u32 TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET = PRIMITIVE_COUNT*2;
static const u32 TYPE_ARRAY_TO_PRIMITIVE_OFFSET    = PRIMITIVE_COUNT*3;
static const u32 TYPE_EXTRA_OFFSET                 = PRIMITIVE_COUNT*4 + 1;
static const u32 CORE_TYPES_COUNT                  = TYPE_EXTRA_OFFSET + 2 + 1;

enum TypeID : u32 {

	TYPE_NULL    = 0,
	TYPE_BYTE    = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 1,
	TYPE_BOOL    = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 2,
	TYPE_UINT8   = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 3,
	TYPE_UINT16  = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 4,
	TYPE_UINT32  = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 5,
	TYPE_UINT64  = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 6,
	TYPE_INT8    = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 7,
	TYPE_INT16   = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 8,
	TYPE_INT32   = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 9,
	TYPE_INT64   = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 10,
	TYPE_FLOAT32 = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 11,
	TYPE_FLOAT64 = (TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 12,

	TYPE_POINTER_BYTE     = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_BYTE,
	TYPE_POINTER_BOOL     = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_BOOL,
	TYPE_POINTER_UINT8    = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_UINT8,
	TYPE_POINTER_UINT32   = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_UINT32,
	TYPE_POINTER_UINT64   = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_UINT64,
	TYPE_POINTER_INT8     = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_INT8,
	TYPE_POINTER_INT16    = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_INT16,
	TYPE_POINTER_INT32    = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_INT32,
	TYPE_POINTER_INT64    = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_INT64,
	TYPE_POINTER_FLOAT32  = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_FLOAT32,
	TYPE_POINTER_FLOAT64  = (TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + TYPE_FLOAT64,

	TYPE_OPTIONAL_BYTE    = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_BYTE,
	TYPE_OPTIONAL_BOOL    = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_BOOL,
	TYPE_OPTIONAL_UINT8   = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_UINT8,
	TYPE_OPTIONAL_UINT16  = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_UINT16,
	TYPE_OPTIONAL_UINT32  = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_UINT32,
	TYPE_OPTIONAL_UINT64  = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_UINT64,
	TYPE_OPTIONAL_INT8    = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_INT8,
	TYPE_OPTIONAL_INT16   = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_INT16,
	TYPE_OPTIONAL_INT32   = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_INT32,
	TYPE_OPTIONAL_INT64   = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_INT64,
	TYPE_OPTIONAL_FLOAT32 = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_FLOAT32,
	TYPE_OPTIONAL_FLOAT64 = (TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + TYPE_FLOAT64,

	TYPE_ARRAY_BYTE       = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_BYTE,
	TYPE_ARRAY_BOOL       = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_BOOL,
	TYPE_ARRAY_UINT8      = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_UINT8,
	TYPE_ARRAY_UINT16     = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_UINT16,
	TYPE_ARRAY_UINT32     = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_UINT32,
	TYPE_ARRAY_UINT64     = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_UINT64,
	TYPE_ARRAY_INT8       = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_INT8,
	TYPE_ARRAY_INT16      = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_INT16,
	TYPE_ARRAY_INT32      = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_INT32,
	TYPE_ARRAY_INT64      = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_INT64,
	TYPE_ARRAY_FLOAT32    = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_FLOAT32,
	TYPE_ARRAY_FLOAT64    = (TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + TYPE_FLOAT64,

	TYPE_BARE_FUNCTION    = (TYPE_FUNCTION << TYPE_INDEX_BITCOUNT) | TYPE_EXTRA_OFFSET + 0,
	TYPE_EMPTY_TUPLE      = (TYPE_TUPLE    << TYPE_INDEX_BITCOUNT) | TYPE_EXTRA_OFFSET + 1,
};

static TypeID type_int = TYPE_INT64;
static TypeID type_uint = TYPE_INT64;

struct PrimitiveTypeInfo {
};

struct PointerTypeInfo {
	TypeID subtype;
};

struct OptionalTypeInfo {
	TypeID subtype;
};

struct ArrayTypeInfo {
	TypeID subtype;
};

struct FixedArrayTypeInfo {
	TypeID subtype;
	u64 length;
};

struct FunctionTypeInfo {
	TypeID input;
	TypeID output;
};

struct TupleTypeInfo {
	TypeID* elements;
	u64  count;
};

struct EnumTypeInfo {
	struct Ast_Enum* ast;
	TypeID backing_type;
};

struct StructTypeInfo {
	struct Ast_Struct* ast;
};

struct ExtensionEntry {
	TypeID type;
	union { TypeID output; u64 length; };
};

struct ExtensionTable {
	u32 count;
	ExtensionEntry* entries;
};

struct TypeInfo {
	TypeID pointer;
	TypeID optional;
	TypeID array;
	ExtensionTable extensions;
	u64 size;

	union {
		PrimitiveTypeInfo  primitive_info;
		PointerTypeInfo    pointer_info;
		OptionalTypeInfo   optional_info;
		ArrayTypeInfo      array_info;
		FixedArrayTypeInfo fixed_info;
		FunctionTypeInfo   function_info;
		TupleTypeInfo      tuple_info;
		EnumTypeInfo       enum_info;
		StructTypeInfo     struct_info;
	};
};

struct TypeSystem {
	TypeInfo* infos;
	u64 info_count;
	u64 info_capacity;
} static type_system;

static inline TypeID CreateTypeID(TypeKind kind, u32 index) {
	Assume(index < (1<<TYPE_INDEX_BITCOUNT));
	return (TypeID)((kind << TYPE_INDEX_BITCOUNT) | index);
}

static inline TypeKind GetTypeKind(TypeID id) {
	return (TypeKind)((s32)id >> TYPE_INDEX_BITCOUNT);
}

static inline s32 GetTypeIndex(TypeID id) {
	return (u32)id & (-1u >> TYPE_KIND_BITCOUNT);
}

static inline TypeInfo* GetTypeInfo(TypeID type) {
	Assert(GetTypeIndex(type) < type_system.info_count);
	return &type_system.infos[GetTypeIndex(type)];
}

static TypeID GetSubType(TypeID type) {
	TypeInfo* info = GetTypeInfo(type);
	switch (GetTypeKind(type)) {
		case TYPE_POINTER:     return info->pointer_info.subtype;
		case TYPE_OPTIONAL:    return info->optional_info.subtype;
		case TYPE_ARRAY:       return info->array_info.subtype;
		case TYPE_FIXED_ARRAY: return info->fixed_info.subtype;
		default: return type;
	}
}

static const u8 PRIMITIVE_SIZE_LUT[PRIMITIVE_COUNT+1] = {
	[TYPE_BYTE]    = 1,
	[TYPE_BOOL]    = 1,
	[TYPE_UINT8]   = 1,
	[TYPE_UINT16]  = 2,
	[TYPE_UINT32]  = 4,
	[TYPE_UINT64]  = 8,
	[TYPE_INT8]    = 1,
	[TYPE_INT16]   = 2,
	[TYPE_INT32]   = 4,
	[TYPE_INT64]   = 8,
	[TYPE_FLOAT32] = 4,
	[TYPE_FLOAT64] = 8,
};

static inline u64 GetTypeSize(TypeID type) {
	switch (GetTypeKind(type)) {
		case TYPE_PRIMITIVE:   return PRIMITIVE_SIZE_LUT[type];
		case TYPE_TUPLE:       return GetTypeInfo(type)->size;
		case TYPE_FUNCTION:    return 0;
		case TYPE_STRUCT:      return GetTypeInfo(type)->size;
		case TYPE_ENUM:        return GetTypeInfo(type)->size;
		case TYPE_POINTER:     return 8;
		case TYPE_OPTIONAL:    return GetTypeInfo(type)->size;
		case TYPE_ARRAY:       return 16;
		case TYPE_FIXED_ARRAY: return GetTypeInfo(type)->size;
	}
}

enum CastKind : s32
{
	CAST_IMPLICIT,
	CAST_COERCIVE,
	CAST_EXPLICIT,
};

static bool IsInteger(TypeID type) {
	switch (type) {
		case TYPE_UINT8:
		case TYPE_UINT16:
		case TYPE_UINT32:
		case TYPE_UINT64:
		case TYPE_INT8:
		case TYPE_INT16:
		case TYPE_INT32:
		case TYPE_INT64:
			return true;

		default:
			return false;
	}
}

static bool IsSignedInteger(TypeID type) {
	switch (type) {
		case TYPE_INT8:
		case TYPE_INT16:
		case TYPE_INT32:
		case TYPE_INT64:
			return true;

		default:
			return false;
	}
}

static bool IsUnsignedInteger(TypeID type) {
	switch (type) {
		case TYPE_UINT8:
		case TYPE_UINT16:
		case TYPE_UINT32:
		case TYPE_UINT64:
			return true;

		default:
			return false;
	}
}

static bool IsFloat(TypeID type) {
	switch (type) {
		case TYPE_FLOAT32:
		case TYPE_FLOAT64:
			return true;

		default:
			return false;
	}
}

static bool IsPointer(TypeID type) {
	return GetTypeKind(type) == TYPE_POINTER;
}

static bool IsFunctionPointer(TypeID type) {
	return GetTypeKind(type) == TYPE_POINTER && GetTypeKind(GetSubType(type)) == TYPE_FUNCTION;
}

static inline bool IsArray(TypeID type) {
	return GetTypeKind(type) == TYPE_ARRAY;
}

static inline bool IsFixedArray(TypeID type) {
	return GetTypeKind(type) == TYPE_FIXED_ARRAY;
}

static TypeID GetFunctionInputType(TypeID type) {
	return GetTypeInfo(type)->function_info.input;
}

static TypeID GetFunctionOutputType(TypeID type) {
	return GetTypeInfo(type)->function_info.output;
}

// static bool IsXXX(TypeID type)
// {
// 	switch (type)
// 	{
// 		case TYPE_BYTE:
// 		case TYPE_BOOL:
// 		case TYPE_UINT8:
// 		case TYPE_UINT16:
// 		case TYPE_UINT32:
// 		case TYPE_UINT64:
// 		case TYPE_INT8:
// 		case TYPE_INT16:
// 		case TYPE_INT32:
// 		case TYPE_INT64:
// 		case TYPE_FLOAT16:
// 		case TYPE_FLOAT32:
// 		case TYPE_FLOAT64:
// 			return true;

// 		default:
// 			return false;
// 	}
// }

static bool CanCast(CastKind cast, TypeID from, TypeID to);
static void InitTypeSystem(void);
static TypeID MergeTypeRight(TypeID a, TypeID b);
static TypeID CreateStructType(Ast_Struct* ast, u64 size);
static TypeID CreateEnumType(Ast_Enum* ast, TypeID backing_type);
static TypeID GetArithmeticBackingType(TypeID id);
static TypeID GetSubType(TypeID type);
static TypeID GetPointer(TypeID subtype);
static TypeID GetOptional(TypeID subtype);
static TypeID GetArray(TypeID subtype);
static TypeID GetTuple(TypeID* types, u64 count);
static TypeID GetFixedArray(TypeID subtype, u64 length);
static TypeID GetFunctionType(TypeID input, TypeID output);
static TypeID GetDominantType(TypeID a, TypeID b);


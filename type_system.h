#pragma once

#include "int.h"
#include "general.h"
#include "assert.h"
#include "array.h"
#include "initlist.h"
#include "fixed_buffer.h"
#include "list.h"

namespace Ast { struct Struct; struct Enum; }

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
	TYPE_REFERENCE   = 9,
};

static const u32 TYPE_BITCOUNT       = sizeof(u32)*8;
static const u32 TYPE_KIND_BITCOUNT  = 4;
static const u32 TYPE_INDEX_BITCOUNT = TYPE_BITCOUNT - TYPE_KIND_BITCOUNT;

static const u32 PRIMITIVE_COUNT = 12;
static const u32 PRIMITIVE_BEGIN = 1;
static const u32 PRIMITIVE_END   = PRIMITIVE_BEGIN + PRIMITIVE_COUNT;
static const u32 TYPE_POINTER_TO_PRIMITIVE_OFFSET   = PRIMITIVE_COUNT*1;
static const u32 TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET  = PRIMITIVE_COUNT*2;
static const u32 TYPE_ARRAY_TO_PRIMITIVE_OFFSET     = PRIMITIVE_COUNT*3;
static const u32 TYPE_REFERENCE_TO_PRIMITIVE_OFFSET = PRIMITIVE_COUNT*4;
static const u32 TYPE_EXTRA_OFFSET                  = PRIMITIVE_COUNT*5 + 1;
static const u32 CORE_TYPES_COUNT                   = TYPE_EXTRA_OFFSET + 2 + 1;

// Primitive index ranges (index within the primitive kind, 1-based)
static const u32 PRIMITIVE_UINT_FIRST  = 3;   // uint8
static const u32 PRIMITIVE_INT_FIRST   = 7;   // int8
static const u32 PRIMITIVE_FLOAT_FIRST = 11;  // float32

enum CastKind : s32 {
	CAST_IMPLICIT,
	CAST_COERCIVE,
	CAST_EXPLICIT,
};

struct TypeInfo;

struct TypeID {
	u32 id = 0;

	constexpr TypeID() = default;
	constexpr TypeID(u32 value) : id(value) { }

	constexpr operator u32()  const { return id; }
	constexpr explicit operator bool() const { return id != 0; }
	constexpr bool IsValid()  const { return id != 0; }

	TypeKind GetKind()  const { return (TypeKind)(id >> TYPE_INDEX_BITCOUNT); }
	s32      GetIndex() const { return (s32)(id & (-1u >> TYPE_KIND_BITCOUNT)); }

	bool IsInteger()         const { TypeID t = RemoveReference(); return t.GetKind() == TYPE_PRIMITIVE && t.GetIndex() >= PRIMITIVE_UINT_FIRST  && t.GetIndex() < PRIMITIVE_FLOAT_FIRST; }
	bool IsSignedInteger()   const { TypeID t = RemoveReference(); return t.GetKind() == TYPE_PRIMITIVE && t.GetIndex() >= PRIMITIVE_INT_FIRST   && t.GetIndex() < PRIMITIVE_FLOAT_FIRST; }
	bool IsUnsignedInteger() const { TypeID t = RemoveReference(); return t.GetKind() == TYPE_PRIMITIVE && t.GetIndex() >= PRIMITIVE_UINT_FIRST  && t.GetIndex() < PRIMITIVE_INT_FIRST;   }
	bool IsFloat()           const { TypeID t = RemoveReference(); return t.GetKind() == TYPE_PRIMITIVE && t.GetIndex() >= PRIMITIVE_FLOAT_FIRST && t.GetIndex() < PRIMITIVE_END;         }

	bool IsReference()  const { return GetKind() == TYPE_REFERENCE; }
	bool IsPointer()    const { return RemoveReference().GetKind() == TYPE_POINTER; }
	bool IsArray()      const { return RemoveReference().GetKind() == TYPE_ARRAY; }
	bool IsFixedArray() const { return RemoveReference().GetKind() == TYPE_FIXED_ARRAY; }

	bool IsFunctionPointer() const {
		TypeID t = RemoveReference();
		return t.GetKind() == TYPE_POINTER && t.GetSubType().GetKind() == TYPE_FUNCTION;
	}

	TypeID RemoveReference() const {
		if (GetKind() == TYPE_REFERENCE)
			return GetSubType();
		return *this;
	}

	TypeInfo& Get() const;
	TypeInfo* operator ->() const;
	TypeInfo* GetInfo() const;
	TypeID    GetSubType() const;
	u64       GetSize() const;
	TypeID    GetFunctionInputType() const;
	TypeID    GetFunctionOutputType() const;

	TypeID GetPointer() const;
	TypeID GetOptional() const;
	TypeID GetArray() const;
	TypeID GetReference() const;
	TypeID GetFixedArray(u64 length) const;
	TypeID GetArithmeticBackingType() const;
	bool   CanCast(CastKind cast, TypeID to) const;

	bool operator ==(TypeID o) const { return id == o.id; }
	bool operator !=(TypeID o) const { return id != o.id; }
	bool operator  <(TypeID o) const { return id  < o.id; }
	bool operator  >(TypeID o) const { return id  > o.id; }
	bool operator <=(TypeID o) const { return id <= o.id; }
	bool operator >=(TypeID o) const { return id >= o.id; }
};
static_assert(sizeof(TypeID) == 4);

struct ExtensionEntry {
	TypeID type;
	union { TypeID output; u64 length; };
};

struct TypeInfo {
	TypeID pointer;
	TypeID optional;
	TypeID array;
	TypeID reference;
	List<ExtensionEntry> extensions;
	u64 size;

	struct Primitive  { };
	struct Pointer    { TypeID subtype; };
	struct Optional   { TypeID subtype; };
	struct Array      { TypeID subtype; };
	struct FixedArray { TypeID subtype; u64 length; };
	struct Reference  { TypeID subtype; };
	struct Function   { TypeID input; TypeID output; };
	struct Tuple      { ::Array<TypeID> elements; };
	struct Enum       { Ast::Enum* ast; TypeID backing_type; };
	struct Struct     { Ast::Struct* ast; };

	union {
		Primitive  primitive_info;
		Pointer    pointer_info;
		Optional   optional_info;
		Array      array_info;
		FixedArray fixed_info;
		Reference  reference_info;
		Function   function_info;
		Tuple      tuple_info;
		Enum       enum_info;
		Struct     struct_info;
	};
};

static FixedBuffer<TypeInfo, 1<<20> type_infos;

static constexpr TypeID TYPE_NULL    = TypeID(0);
static constexpr TypeID TYPE_BYTE    = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 1);
static constexpr TypeID TYPE_BOOL    = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 2);
static constexpr TypeID TYPE_UINT8   = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 3);
static constexpr TypeID TYPE_UINT16  = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 4);
static constexpr TypeID TYPE_UINT32  = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 5);
static constexpr TypeID TYPE_UINT64  = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 6);
static constexpr TypeID TYPE_INT8    = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 7);
static constexpr TypeID TYPE_INT16   = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 8);
static constexpr TypeID TYPE_INT32   = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 9);
static constexpr TypeID TYPE_INT64   = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 10);
static constexpr TypeID TYPE_FLOAT32 = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 11);
static constexpr TypeID TYPE_FLOAT64 = TypeID((TYPE_PRIMITIVE << TYPE_INDEX_BITCOUNT) | 12);

static constexpr TypeID TYPE_POINTER_BYTE     = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 1);
static constexpr TypeID TYPE_POINTER_BOOL     = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 2);
static constexpr TypeID TYPE_POINTER_UINT8    = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 3);
static constexpr TypeID TYPE_POINTER_UINT32   = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 5);
static constexpr TypeID TYPE_POINTER_UINT64   = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 6);
static constexpr TypeID TYPE_POINTER_INT8     = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 7);
static constexpr TypeID TYPE_POINTER_INT16    = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 8);
static constexpr TypeID TYPE_POINTER_INT32    = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 9);
static constexpr TypeID TYPE_POINTER_INT64    = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 10);
static constexpr TypeID TYPE_POINTER_FLOAT32  = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 11);
static constexpr TypeID TYPE_POINTER_FLOAT64  = TypeID((TYPE_POINTER << TYPE_INDEX_BITCOUNT) | TYPE_POINTER_TO_PRIMITIVE_OFFSET + 12);

static constexpr TypeID TYPE_OPTIONAL_BYTE    = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 1);
static constexpr TypeID TYPE_OPTIONAL_BOOL    = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 2);
static constexpr TypeID TYPE_OPTIONAL_UINT8   = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 3);
static constexpr TypeID TYPE_OPTIONAL_UINT16  = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 4);
static constexpr TypeID TYPE_OPTIONAL_UINT32  = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 5);
static constexpr TypeID TYPE_OPTIONAL_UINT64  = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 6);
static constexpr TypeID TYPE_OPTIONAL_INT8    = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 7);
static constexpr TypeID TYPE_OPTIONAL_INT16   = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 8);
static constexpr TypeID TYPE_OPTIONAL_INT32   = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 9);
static constexpr TypeID TYPE_OPTIONAL_INT64   = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 10);
static constexpr TypeID TYPE_OPTIONAL_FLOAT32 = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 11);
static constexpr TypeID TYPE_OPTIONAL_FLOAT64 = TypeID((TYPE_OPTIONAL << TYPE_INDEX_BITCOUNT) | TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + 12);

static constexpr TypeID TYPE_ARRAY_BYTE       = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 1);
static constexpr TypeID TYPE_ARRAY_BOOL       = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 2);
static constexpr TypeID TYPE_ARRAY_UINT8      = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 3);
static constexpr TypeID TYPE_ARRAY_UINT16     = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 4);
static constexpr TypeID TYPE_ARRAY_UINT32     = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 5);
static constexpr TypeID TYPE_ARRAY_UINT64     = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 6);
static constexpr TypeID TYPE_ARRAY_INT8       = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 7);
static constexpr TypeID TYPE_ARRAY_INT16      = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 8);
static constexpr TypeID TYPE_ARRAY_INT32      = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 9);
static constexpr TypeID TYPE_ARRAY_INT64      = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 10);
static constexpr TypeID TYPE_ARRAY_FLOAT32    = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 11);
static constexpr TypeID TYPE_ARRAY_FLOAT64    = TypeID((TYPE_ARRAY << TYPE_INDEX_BITCOUNT) | TYPE_ARRAY_TO_PRIMITIVE_OFFSET + 12);

static constexpr TypeID TYPE_REFERENCE_BYTE    = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 1);
static constexpr TypeID TYPE_REFERENCE_BOOL    = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 2);
static constexpr TypeID TYPE_REFERENCE_UINT8   = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 3);
static constexpr TypeID TYPE_REFERENCE_UINT16  = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 4);
static constexpr TypeID TYPE_REFERENCE_UINT32  = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 5);
static constexpr TypeID TYPE_REFERENCE_UINT64  = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 6);
static constexpr TypeID TYPE_REFERENCE_INT8    = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 7);
static constexpr TypeID TYPE_REFERENCE_INT16   = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 8);
static constexpr TypeID TYPE_REFERENCE_INT32   = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 9);
static constexpr TypeID TYPE_REFERENCE_INT64   = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 10);
static constexpr TypeID TYPE_REFERENCE_FLOAT32 = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 11);
static constexpr TypeID TYPE_REFERENCE_FLOAT64 = TypeID((TYPE_REFERENCE << TYPE_INDEX_BITCOUNT) | TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + 12);

static constexpr TypeID TYPE_BARE_FUNCTION = TypeID((TYPE_FUNCTION << TYPE_INDEX_BITCOUNT) | TYPE_EXTRA_OFFSET + 0);
static constexpr TypeID TYPE_EMPTY_TUPLE   = TypeID((TYPE_TUPLE    << TYPE_INDEX_BITCOUNT) | TYPE_EXTRA_OFFSET + 1);

static TypeID CreateTypeID(TypeKind kind, u32 index);
static void InitTypeSystem(void);
static TypeID MergeTypeRight(TypeID a, TypeID b);
static TypeID CreateStructType(Ast::Struct* ast, u64 size);
static TypeID CreateEnumType(Ast::Enum* ast, TypeID backing_type);
static TypeID GetTuple(Array<TypeID> types);
static TypeID GetTuple(TypeID* elements, u64 count);
static TypeID GetFunctionType(TypeID input, TypeID output);
static TypeID GetDominantType(TypeID a, TypeID b);


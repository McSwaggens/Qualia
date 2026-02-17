#include "type_system.h"
#include "alloc.h"
#include "general.h"
#include "assert.h"

static TypeID CreateTypeID(TypeKind kind, u32 index) {
	Assume(index < (1<<TYPE_INDEX_BITCOUNT));
	return TypeID((kind << TYPE_INDEX_BITCOUNT) | index);
}

inline TypeID TypeID::GetSubType() const {
	TypeInfo* info = GetInfo();
	switch (GetKind()) {
		case TYPE_POINTER:     return info->pointer_info.subtype;
		case TYPE_OPTIONAL:    return info->optional_info.subtype;
		case TYPE_ARRAY:       return info->array_info.subtype;
		case TYPE_FIXED_ARRAY: return info->fixed_info.subtype;
		case TYPE_REFERENCE:   return info->reference_info.subtype;
		default: return *this;
	}
}

inline u64 TypeID::GetSize() const {
	static constexpr u8 PRIMITIVE_SIZE_LUT[PRIMITIVE_COUNT+1] = {
		0, // 0: unused
		1, // TYPE_BYTE (1)
		1, // TYPE_BOOL (2)
		1, // TYPE_UINT8 (3)
		2, // TYPE_UINT16 (4)
		4, // TYPE_UINT32 (5)
		8, // TYPE_UINT64 (6)
		1, // TYPE_INT8 (7)
		2, // TYPE_INT16 (8)
		4, // TYPE_INT32 (9)
		8, // TYPE_INT64 (10)
		4, // TYPE_FLOAT32 (11)
		8, // TYPE_FLOAT64 (12)
	};
	TypeInfo* info = GetInfo();
	switch (GetKind()) {
		case TYPE_PRIMITIVE:   return PRIMITIVE_SIZE_LUT[id];
		case TYPE_TUPLE:       return info->size;
		case TYPE_FUNCTION:    return 0;
		case TYPE_STRUCT:      return info->size;
		case TYPE_ENUM:        return info->size;
		case TYPE_POINTER:     return 8;
		case TYPE_OPTIONAL:    return info->size;
		case TYPE_ARRAY:       return 16;
		case TYPE_FIXED_ARRAY: return info->size;
		case TYPE_REFERENCE:   return 8;
		default: AssertUnreachable();
	}
}

inline TypeID TypeID::GetFunctionInputType()  const { return GetInfo()->function_info.input; }
inline TypeID TypeID::GetFunctionOutputType() const { return GetInfo()->function_info.output; }

inline TypeInfo& TypeID::Get() const {
	return type_infos[GetIndex()];
}

inline TypeInfo* TypeID::operator ->() const {
	return &type_infos[GetIndex()];
}

inline TypeInfo* TypeID::GetInfo() const {
	Assert(GetIndex() < (s32)type_infos.head);
	return &type_infos[GetIndex()];
}

TypeID TypeID::GetArithmeticBackingType() const {
	TypeID type = RemoveReference();

	if (type == TYPE_BOOL)
		return TYPE_INT8;

	TypeKind kind = type.GetKind();
	TypeInfo* info = type.GetInfo();

	if (kind == TYPE_ENUM)
		return info->enum_info.backing_type;

	return type;
}

static TypeID GetIntegerWithSign(TypeID type, bool sign) {
	if (sign) switch (type) {
		case TYPE_UINT8:  return TYPE_INT8;
		case TYPE_UINT16: return TYPE_INT16;
		case TYPE_UINT32: return TYPE_INT32;
		case TYPE_UINT64: return TYPE_INT64;
		default: return type;
	}

	switch (type) {
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

	// Unwrap references and find dominant type of subtypes
	a = a.RemoveReference();
	b = b.RemoveReference();

	// Check again after unwrapping - (ref)T and T should be compatible
	if (a == b)
		return a;

	if (a < b)
		Swap(a, b);

	if (a.IsInteger() && b.IsInteger()) {
		bool sign = a.IsSignedInteger() && b.IsSignedInteger();
		return GetIntegerWithSign(a, sign);
	}

	if (a.GetKind() == TYPE_PRIMITIVE && b.GetKind() == TYPE_PRIMITIVE) {
	}

	Assert("Invalid dominator combo");
	return TYPE_NULL;
}

bool TypeID::CanCast(CastKind cast, TypeID to) const {
	TypeID from = *this;
	TypeKind from_kind = from.GetKind();
	TypeKind to_kind   = to.GetKind();

	TypeInfo* from_info = from.GetInfo();
	TypeInfo* to_info   = to.GetInfo();

	if (from == to)
		return true;

	if (from == TYPE_BYTE)
		return to.GetSize() == 1;

	if (to == TYPE_BYTE)
		return from.GetSize() == 1;

	switch (from_kind) {
		case TYPE_PRIMITIVE: {
			if (to_kind == TYPE_PRIMITIVE)
				return cast >= CAST_IMPLICIT;

			if (to_kind == TYPE_ENUM)
				return cast >= CAST_EXPLICIT;

			return false;
		}

		case TYPE_TUPLE: {
			if (to_kind == TYPE_TUPLE && cast >= CAST_COERCIVE && from_info->tuple_info.elements.length == to_info->tuple_info.elements.length) {
				for (u64 i = 0; i < from_info->tuple_info.elements.length; i++) {
					if (!from_info->tuple_info.elements[i].CanCast(cast, to_info->tuple_info.elements[i]))
						return false;
				}

				return true;
			}
		} return false;

		case TYPE_FUNCTION: return false;
		case TYPE_STRUCT:   return false;

		case TYPE_ENUM: {
			TypeID backing_type = from_info->enum_info.backing_type;

			switch ((u32)to) {
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

		case TYPE_POINTER: {
			if (to_kind == TYPE_POINTER) {
				if (from == TYPE_POINTER_BYTE || to == TYPE_POINTER_BYTE)
					return cast >= CAST_IMPLICIT;

				return cast >= CAST_EXPLICIT;
			}

			switch ((u32)to) {
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

		case TYPE_OPTIONAL: {
			if (to == TYPE_BOOL)
				return cast >= CAST_COERCIVE;

			TypeID subtype = from.GetSubType();

			if (to == subtype)
				return cast >= CAST_EXPLICIT;

			return false;
		}

		case TYPE_ARRAY:
			return false;

		case TYPE_FIXED_ARRAY: {
			if (from_info->fixed_info.length == 1 && to == from.GetSubType())
				return true;

			if (to_kind == TYPE_FIXED_ARRAY && from_info->fixed_info.length == to_info->fixed_info.length)
				return from.GetSubType().CanCast(cast, to.GetSubType());

			return false;
		}

		case TYPE_REFERENCE: {
			// References implicitly dereference to their subtype
			TypeID subtype = from.GetSubType();
			return subtype.CanCast(cast, to);
		}
	}

	return false;
}


static void InitTypeSystem(void) {
	type_infos.head = CORE_TYPES_COUNT;

	for (s32 prim = PRIMITIVE_BEGIN; prim < PRIMITIVE_END; prim++) {
		TypeInfo* info = &type_infos[prim];
		Zero(info);

		info->size = TypeID(prim).GetSize();

		info->pointer   = CreateTypeID(TYPE_POINTER,  TYPE_POINTER_TO_PRIMITIVE_OFFSET  + prim);
		info->optional  = CreateTypeID(TYPE_OPTIONAL, TYPE_OPTIONAL_TO_PRIMITIVE_OFFSET + prim);
		info->array     = CreateTypeID(TYPE_ARRAY,    TYPE_ARRAY_TO_PRIMITIVE_OFFSET    + prim);
		info->reference = CreateTypeID(TYPE_REFERENCE, TYPE_REFERENCE_TO_PRIMITIVE_OFFSET + prim);

		TypeInfo* pointer  = info->pointer.GetInfo();
		TypeInfo* optional = info->optional.GetInfo();
		TypeInfo* array    = info->array.GetInfo();
		TypeInfo* reference = info->reference.GetInfo();

		Zero(pointer);
		Zero(optional);
		Zero(array);
		Zero(reference);

		pointer->size  = 8;
		optional->size = 1 + info->size;
		array->size    = 16;
		reference->size = 8;

		pointer->pointer_info.subtype   = TypeID(prim);
		optional->optional_info.subtype = TypeID(prim);
		array->array_info.subtype       = TypeID(prim);
		reference->reference_info.subtype = TypeID(prim);
	}

	*TYPE_BARE_FUNCTION.GetInfo() = {
		.size = 0,
		.function_info = {
			.input  = TYPE_EMPTY_TUPLE,
			.output = TYPE_EMPTY_TUPLE,
		}
	};

	TypeInfo* empty = TYPE_EMPTY_TUPLE.GetInfo();
	*empty = {
		.size = 0,
		.tuple_info = {
			.elements = { }
		}
	};

	empty->extensions.Add({ .type = TYPE_BARE_FUNCTION, .output = TYPE_EMPTY_TUPLE });
}

static TypeID CreateType(TypeKind kind, TypeInfo info) {
	return CreateTypeID(kind, (u32)type_infos.AddIndex(info));
}

TypeID TypeID::GetPointer() const {
	Assert(*this);

	TypeInfo* info = GetInfo();

	if (info->pointer) HOT
		return info->pointer;

	TypeID ptr = CreateType(TYPE_POINTER, {
		.size = 8,
		.pointer_info.subtype = *this,
	});

	GetInfo()->pointer = ptr;

	return ptr;
}

TypeID TypeID::GetOptional() const {
	Assert(*this);

	TypeInfo* info = GetInfo();

	if (info->optional)
		return info->optional;

	TypeID result = CreateType(TYPE_OPTIONAL, {
		.size = GetSize() + 1,
		.optional_info.subtype = *this,
	});

	GetInfo()->optional = result;

	return result;
}

TypeID TypeID::GetArray() const {
	Assert(*this);

	TypeInfo* info = GetInfo();

	if (info->array)
		return info->array;

	TypeID result = CreateType(TYPE_ARRAY, {
		.size = 16,
		.array_info.subtype = *this,
	});

	GetInfo()->array = result;

	return result;
}

TypeID TypeID::GetReference() const {
	Assert(*this);
	Assert(!IsReference());

	TypeInfo* info = GetInfo();

	if (info->reference)
		return info->reference;

	TypeID result = CreateType(TYPE_REFERENCE, {
		.size = 8,
		.reference_info.subtype = *this,
	});

	GetInfo()->reference = result;

	return result;
}

TypeID TypeID::GetFixedArray(u64 length) const {
	Assert(*this);

	TypeInfo* info = GetInfo();

	for (u32 i = 0; i < info->extensions.count; i++) {
		ExtensionEntry& entry = info->extensions[i];
		if (entry.length == length && entry.type.GetKind() == TYPE_FIXED_ARRAY)
			return entry.type;
	}

	TypeID result = CreateType(TYPE_FIXED_ARRAY, {
		.size = length * GetSize(),
		.fixed_info = {
			.subtype = *this,
			.length = length,
		}
	});

	info->extensions.Add({ .type = result, .length = length });

	return result;
}

static TypeID GetFunctionType(TypeID input, TypeID output) {
	Assert(input);
	Assert(output);

	TypeInfo* input_info = input.GetInfo();

	for (u32 i = 0; i < input_info->extensions.count; i++) {
		ExtensionEntry& entry = input_info->extensions[i];
		if (entry.output == output && entry.type.GetKind() == TYPE_FUNCTION)
			return entry.type;
	}

	TypeID result = CreateType(TYPE_FUNCTION, {
		.size = 0,
		.function_info = {
			.input  = input,
			.output = output,
		}
	});

	input_info->extensions.Add({ .type = result, .output = output });

	return result;
}

static TypeID GetTuple(Array<TypeID> elements) {
	if (!elements.length)
		return TYPE_EMPTY_TUPLE;

	if (elements.length == 1)
		return elements[0];

	List<ExtensionEntry>& extensions = elements[0].GetInfo()->extensions;

	for (s32 i = 0; i < extensions.count; i++) {
		ExtensionEntry& entry = extensions[i];

		if (entry.length != elements.length)
			continue;

		if (entry.type.GetKind() != TYPE_TUPLE)
			continue;

		TypeInfo* ext_info = entry.type.GetInfo();
		if (elements == ext_info->tuple_info.elements)
			return entry.type;
	}

	u64 size = 0;
	for (s32 i = 0; i < elements.length; i++)
		size += elements[i].GetSize();

	TypeID* new_elements = CopyAlloc(elements.data, elements.length);

	TypeID result = CreateType(TYPE_TUPLE, {
		.size = size,
		.tuple_info.elements = { new_elements, elements.length }
	});

	extensions.Add({ .type = result, .length = elements.length });

	return result;
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
		.size = backing_type.GetSize(),
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

	if (b.GetKind() != TYPE_TUPLE) {
		TypeID types[2] = { a, b };
		return GetTuple(Array<TypeID>(types, 2));
	}

	TypeInfo* b_info = b.GetInfo();
	u64 count = b_info->tuple_info.elements.length+1;

	TypeID elements[count];
	elements[0] = a;
	Copy(elements+1, b_info->tuple_info.elements.Begin(), count-1);

	return GetTuple(Array<TypeID>(elements, count));
}

static TypeID GetTuple(TypeID* elements, u64 count) {
	return GetTuple(Array<TypeID>(elements, count));
}

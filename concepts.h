#ifndef CONCEPTS_H
#define CONCEPTS_H

#include "int.h"
#include "type_system.h"

namespace Concepts {
	template<typename T> consteval bool IsUnsignedInteger() { return false; }
	template<> consteval bool IsUnsignedInteger<u8  >() { return true; }
	template<> consteval bool IsUnsignedInteger<u16 >() { return true; }
	template<> consteval bool IsUnsignedInteger<u32 >() { return true; }
	template<> consteval bool IsUnsignedInteger<u64 >() { return true; }
	template<> consteval bool IsUnsignedInteger<u128>() { return true; }
	template<typename T> concept UnsignedInteger = IsUnsignedInteger<T>();

	template<typename T> consteval bool IsSignedInteger() { return false; }
	template<> consteval bool IsSignedInteger<u8  >() { return true; }
	template<> consteval bool IsSignedInteger<u16 >() { return true; }
	template<> consteval bool IsSignedInteger<u32 >() { return true; }
	template<> consteval bool IsSignedInteger<u64 >() { return true; }
	template<> consteval bool IsSignedInteger<u128>() { return true; }
	template<typename T> concept SignedInteger = IsSignedInteger<T>();

	template<typename T> concept Integer = SignedInteger<T> || UnsignedInteger<T>;

	template<typename T> consteval bool IsFloat() { return false; }
	template<> consteval bool IsFloat<float16>() { return true; }
	template<> consteval bool IsFloat<float32>() { return true; }
	template<> consteval bool IsFloat<float64>() { return true; }
	template<typename T> concept Float = IsFloat<T>();

	template<typename T> struct IsPointerType                    { static constexpr bool value = false; };
	template<typename T> struct IsPointerType<T*>                { static constexpr bool value = true;  };
	template<typename T> struct IsPointerType<T* const>          { static constexpr bool value = true;  };
	template<typename T> struct IsPointerType<T* volatile>       { static constexpr bool value = true;  };
	template<typename T> struct IsPointerType<T* const volatile> { static constexpr bool value = true;  };

	template<typename T> concept Pointer = IsPointerType<T>::value;
	template<typename T> concept Numeric = Integer<T> || Float<T> || Pointer<T>;
}
#endif // CONCEPTS_H

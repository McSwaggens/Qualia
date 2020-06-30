#pragma once

#include "int.h"
#include "list.h"
#include "array.h"
#include "string.h"
#include "token.h"
#include "parser.h"

enum Primitive
{
	PRIMITIVE_BOOL,

	PRIMITIVE_INT,
	PRIMITIVE_INT8,
	PRIMITIVE_INT16,
	PRIMITIVE_INT32,
	PRIMITIVE_INT64,

	PRIMITIVE_UINT,
	PRIMITIVE_UINT8,
	PRIMITIVE_UINT16,
	PRIMITIVE_UINT32,
	PRIMITIVE_UINT64,

	PRIMITIVE_FLOAT16,
	PRIMITIVE_FLOAT32,
	PRIMITIVE_FLOAT64,
};

enum BaseType_Kind
{
	BASETYPE_PRIMITIVE,
	BASETYPE_STRUCT,
	BASETYPE_ENUM,
	BASETYPE_TUPLE,
	BASETYPE_FUNCTION,
};

enum Specifier_Kind
{
	SPECIFIER_POINTER,
	SPECIFIER_OPTIONAL,
	SPECIFIER_FIXED_ARRAY,
	SPECIFIER_DYNAMIC_ARRAY,
};

struct Scope;
struct Struct;
struct Enum;
struct Type;
struct BaseType;
struct Sepcifier;
struct Function;
struct Parameter;
struct Code;

struct Scope
{
	Scope* parent_scope;
	Array<Function> functions;
	Array<Struct>   structs;
	Array<Enum>     enums;
};

struct BaseType
{
	BaseType_Kind kind;

	union
	{
		Struct*   structure;
		Enum*     enumeration;
		Primitive primitive;
		Array<Type> tuple;

		struct
		{
			Array<Type> input_params;
			Type* output;
		};
	};
};

struct Specifier
{
	Specifier_Kind kind;
	u64 array_size;
};

struct Type
{
	BaseType base_type;
	Array<Specifier> specifiers;
};

struct Struct_Member
{
	Ast_Struct_Member* ast;
	String name;
	Type type;
};

struct Struct
{
	Ast_Struct* ast;
	String name;
	u64 size_bytes;
	Array<Struct_Member> members;
};

struct Enum_Member
{
	Ast_Enum_Member* ast;
	String name;
	u64 value;
};

struct Enum
{
	Ast_Enum* ast;
	String    name;
	Array<Enum_Member> members;
};

struct Parameter
{
	Ast_Param* ast;
	String name;
	Type type;
};

struct Code
{
	Ast_Code* ast;
	Scope scope;
};

struct Function
{
	Ast_Function* ast;
	String name;
	Array<Parameter> parameters;
	Code code;
};

enum Statement_Kind
{
	STATEMENT_IF,
	STATEMENT_FOR,
	STATEMENT_WHILE,
	STATEMENT_EXPRESSION,
	STATEMENT_DEFER,
	STATEMENT_RETURN,
	STATEMENT_BREAK,
	STATEMENT_ASSIGNMENT,
	STATEMENT_ASSIGNMENT_PLUS,
	STATEMENT_ASSIGNMENT_MINUS,
	STATEMENT_ASSIGNMENT_MULTIPLY,
	STATEMENT_ASSIGNMENT_DIVIDE,
	STATEMENT_ASSIGNMENT_EXPONENTIAL,
};

struct Statement
{
	Statement_Kind kind;
	Ast_Statement ast;
};

enum Expression_Kind
{
	EXPRESSION_PLUS,
	EXPRESSION_MINUS,
	EXPRESSION_MULTIPLY,
	EXPRESSION_DIVIDE,
	EXPRESSION_EXPONENTIAL,
	EXPRESSION_BITWISE_AND,
	EXPRESSION_BITWISE_OR,
	EXPRESSION_BITWISE_XOR,
	EXPRESSION_BITWISE_NOT,
	EXPRESSION_LOGICAL_AND,
	EXPRESSION_LOGICAL_OR,
	EXPRESSION_LOGICAL_NOT,
	EXPRESSION_EQUAL,
	EXPRESSION_NOT_EQUAL,
	EXPRESSION_DEREFERENCE,
	EXPRESSION_ADDRESS_OF,
};

struct Expression
{
	Expression_Kind kind;
	Ast_Expression* ast;
};

struct Semantic
{
	Scope scope;
	Parse_Info info;
};

void SemanticParse(Parse_Info info);


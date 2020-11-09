#pragma once

#include "int.h"
#include "string.h"
#include "list.h"
#include "token.h"
#include "span.h"

struct Ast_Expression;
struct Ast_VariableDeclaration;
struct Ast_Alias;
struct Ast_Import;
struct Ast_Struct_Member;
struct Ast_Enum_Member;
struct Ast_Statement;
struct Ast_Defer;
struct Ast_Specifier;
struct Ast_BaseType;
struct Ast_Type;
struct Ast_Function;
struct Ast_Struct;
struct Ast_Enum;
struct Ast_Code;
struct Ast_Scope;
struct Ast_Attribute;
struct Type;
struct StackFrame;

void Write(OutputBuffer* buffer, Ast_Expression* expression);
void Write(OutputBuffer* buffer, Ast_Type* type);
void Write(OutputBuffer* buffer, Ast_Type type);
void Write(OutputBuffer* buffer, Type* type);

enum Type_Kind
{
	TYPE_BASETYPE_STRUCT,
	TYPE_BASETYPE_ENUM,
	TYPE_BASETYPE_PRIMITIVE,
	TYPE_BASETYPE_TUPLE,
	TYPE_BASETYPE_FUNCTION,
	TYPE_SPECIFIER_POINTER,
	TYPE_SPECIFIER_OPTIONAL,
	TYPE_SPECIFIER_DYNAMIC_ARRAY,
	TYPE_SPECIFIER_FIXED_ARRAY
};

struct Type
{
	Type_Kind kind;

	union
	{
		Ast_Struct* structure;
		Ast_Enum*   enumeration;
		Token_Kind  primitive;

		struct
		{
			Type* input;
			Type* output;
		} function;

		Array<Type*> tuple;
		Type* subtype;
	};

	Type* specifiers;
	u64 length; // Fixed array size
	u64 size;   // Size of the type in bytes

	// @Optimization: Combine these into single List?
	List<Type*> fixed_arrays;
	List<Type*> tuple_extensions;
	List<Type*> function_extensions;
};

struct Ast_Attribute
{
	Token* token;
	Ast_Expression* expression;
};

enum Ast_Specifier_Kind
{
	AST_SPECIFIER_POINTER,
	AST_SPECIFIER_OPTIONAL,
	AST_SPECIFIER_ARRAY
};

struct Ast_Specifier
{
	Ast_Specifier_Kind kind;
	Token* token; // @Optimization: s[n].token = s[n-1].token+1
	Ast_Expression* size_expression;
};

enum Ast_BaseType_Kind
{
	// @Todo: Add `AST_BASETYPE_ALIAS`?
	AST_BASETYPE_USERTYPE,
	AST_BASETYPE_STRUCT,
	AST_BASETYPE_ENUM,
	AST_BASETYPE_PRIMITIVE,
	AST_BASETYPE_FUNCTION,
	AST_BASETYPE_TUPLE
};

struct Ast_BaseType_Function
{
	Ast_Type* input;
	Ast_Type* output;
};

struct Ast_BaseType
{
	Ast_BaseType_Kind kind;
	Token* token;
	Type*  type;

	union
	{
		Array<Ast_Type> tuple;
		Ast_BaseType_Function function;
		Ast_Struct* structure;
		Ast_Enum*   enumeration;
	};
};

struct Ast_Type
{
	List<Ast_Specifier> specifiers;
	Ast_BaseType basetype;
	Type* type;
};

enum Ast_Expression_Kind
{
	AST_EXPRESSION_TERMINAL,
	AST_EXPRESSION_TERMINAL_FUNCTION,
	AST_EXPRESSION_TERMINAL_LITERAL,
	AST_EXPRESSION_TERMINAL_VARIABLE,
	AST_EXPRESSION_TERMINAL_STRUCT,
	AST_EXPRESSION_TERMINAL_ENUM,
	AST_EXPRESSION_TERMINAL_PRIMITIVE,
	AST_EXPRESSION_TERMINAL_STRUCT_MEMBER,
	AST_EXPRESSION_TERMINAL_ENUM_MEMBER,
	AST_EXPRESSION_UNARY_BINARY_NOT,
	AST_EXPRESSION_UNARY_NOT,
	AST_EXPRESSION_UNARY_MINUS,
	AST_EXPRESSION_UNARY_PLUS,
	AST_EXPRESSION_UNARY_VALUE_OF,
	AST_EXPRESSION_UNARY_ADDRESS_OF,
	AST_EXPRESSION_BINARY_COMPARE_EQUAL,
	AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL,
	AST_EXPRESSION_BINARY_COMPARE_LESS,
	AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL,
	AST_EXPRESSION_BINARY_COMPARE_GREATER,
	AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL,
	AST_EXPRESSION_BINARY_DOT,
	AST_EXPRESSION_BINARY_ADD,
	AST_EXPRESSION_BINARY_SUBTRACT,
	AST_EXPRESSION_BINARY_MULTIPLY,
	AST_EXPRESSION_BINARY_DIVIDE,
	AST_EXPRESSION_BINARY_MODULO,
	AST_EXPRESSION_BINARY_EXPONENTIAL,
	AST_EXPRESSION_BINARY_BITWISE_OR,
	AST_EXPRESSION_BINARY_BITWISE_XOR,
	AST_EXPRESSION_BINARY_BITWISE_AND,
	AST_EXPRESSION_BINARY_LEFT_SHIFT,
	AST_EXPRESSION_BINARY_RIGHT_SHIFT,
	AST_EXPRESSION_BINARY_AND,
	AST_EXPRESSION_BINARY_OR,
	AST_EXPRESSION_CALL,
	AST_EXPRESSION_SUBSCRIPT,
	AST_EXPRESSION_LAMBDA,
	AST_EXPRESSION_TUPLE,
	AST_EXPRESSION_AS,
	AST_EXPRESSION_IF_ELSE,
};

static bool IsTerminalExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_TERMINAL:
		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_LITERAL:
		case AST_EXPRESSION_TERMINAL_VARIABLE:
		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
			return true;
		default:
			return false;
	}
}

static bool IsUnaryExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_UNARY_BINARY_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		case AST_EXPRESSION_UNARY_MINUS:
		case AST_EXPRESSION_UNARY_PLUS:
		case AST_EXPRESSION_UNARY_VALUE_OF:
		case AST_EXPRESSION_UNARY_ADDRESS_OF:
			return true;
		default:
			return false;
	}
}

static bool IsBinaryExpression(Ast_Expression_Kind kind)
{
	switch (kind)
	{
		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		case AST_EXPRESSION_BINARY_DOT:
		case AST_EXPRESSION_BINARY_ADD:
		case AST_EXPRESSION_BINARY_SUBTRACT:
		case AST_EXPRESSION_BINARY_MULTIPLY:
		case AST_EXPRESSION_BINARY_DIVIDE:
		case AST_EXPRESSION_BINARY_MODULO:
		case AST_EXPRESSION_BINARY_EXPONENTIAL:
		case AST_EXPRESSION_BINARY_BITWISE_OR:
		case AST_EXPRESSION_BINARY_BITWISE_XOR:
		case AST_EXPRESSION_BINARY_BITWISE_AND:
		case AST_EXPRESSION_BINARY_LEFT_SHIFT:
		case AST_EXPRESSION_BINARY_RIGHT_SHIFT:
		case AST_EXPRESSION_BINARY_AND:
		case AST_EXPRESSION_BINARY_OR:
			return true;
		default:
			return false;
	}
}

struct Ast_Expression
{
	Ast_Expression_Kind kind;
	bool is_pure;
	bool is_referential_value;
	bool can_constantly_evaluate;
	Type* type;
	Span<Token> span;

	struct Ast_Expression_Unary*         GetUnary()         { return (Ast_Expression_Unary*)this;         }
	struct Ast_Expression_Binary*        GetBinary()        { return (Ast_Expression_Binary*)this;        }
	struct Ast_Expression_Ternary*       GetTernary()       { return (Ast_Expression_Ternary*)this;       }
	struct Ast_Expression_Call*          GetCall()          { return (Ast_Expression_Call*)this;          }
	struct Ast_Expression_Tuple*         GetTuple()         { return (Ast_Expression_Tuple*)this;         }
	struct Ast_Expression_Literal*       GetLiteral()       { return (Ast_Expression_Literal*)this;       }
	struct Ast_Expression_Subscript*     GetSubscript()     { return (Ast_Expression_Subscript*)this;     }
	struct Ast_Expression_Terminal*      GetTerminal()      { return (Ast_Expression_Terminal*)this;      }
	struct Ast_Expression_Variable*      GetVariable()      { return (Ast_Expression_Variable*)this;      }
	struct Ast_Expression_Function*      GetFunction()      { return (Ast_Expression_Function*)this;      }
	struct Ast_Expression_Struct*        GetStruct()        { return (Ast_Expression_Struct*)this;        }
	struct Ast_Expression_Enum*          GetEnum()          { return (Ast_Expression_Enum*)this;          }
	struct Ast_Expression_Struct_Member* GetStructMember()  { return (Ast_Expression_Struct_Member*)this; }
	struct Ast_Expression_Enum_Member*   GetEnumMember()    { return (Ast_Expression_Enum_Member*)this;   }
};

struct Ast_Expression_Unary : Ast_Expression
{
	Ast_Expression* subexpression;
	Token* op;
};

struct Ast_Expression_Binary : Ast_Expression
{
	Ast_Expression* left;
	Ast_Expression* right;
	Token* op;
};

struct Ast_Expression_Ternary : Ast_Expression
{
	Ast_Expression* left;
	Ast_Expression* middle;
	Ast_Expression* right;
	Token* ops[2];
};

struct Ast_Expression_Call : Ast_Expression
{
	Ast_Expression* function;
	Ast_Expression* parameters;
};

struct Ast_Expression_Tuple : Ast_Expression
{
	List<Ast_Expression*> elements;
};

struct Ast_Expression_Literal : Ast_Expression
{
	Token* token;
};

struct Ast_Expression_Subscript : Ast_Expression
{
	Ast_Expression* array;
	Ast_Expression* index;
};

struct Ast_Expression_Terminal : Ast_Expression
{
	Token* token;
	void* ptr;
};

struct Ast_Expression_Variable : Ast_Expression
{
	Token* token;
	Ast_VariableDeclaration* variable;
};

struct Ast_Expression_Function : Ast_Expression
{
	Token* token;
	Ast_Function* function;
};

struct Ast_Expression_Struct : Ast_Expression
{
	Token* token;
	Ast_Struct* structure;
};

struct Ast_Expression_Enum : Ast_Expression
{
	Token* token;
	Ast_Enum* enumeration;
};

struct Ast_Expression_Struct_Member : Ast_Expression
{
	Token* token;
	Ast_Struct_Member* member;
};

struct Ast_Expression_Enum_Member : Ast_Expression
{
	Token* token;
	Ast_Enum_Member* member;
};

struct Ast_Scope
{
	Ast_Scope* parent;
	// Aliases?
	List<Ast_Function> functions;
	List<Ast_Struct> structs;
	List<Ast_Enum> enums;
	List<Ast_VariableDeclaration*> variables;
	List<StackFrame> stack_frames;
};

struct Ast_Code
{
	List<Ast_Statement> statements;
	List<Ast_Defer*> defers;
	Ast_Scope scope;
	u64 frame_size;
	bool does_return;
	bool is_inside_loop;
	bool has_deferrer_that_returns;
};

enum Ast_Statement_Kind 
{
	AST_STATEMENT_BRANCH_BLOCK,
	AST_STATEMENT_DEFER,
	AST_STATEMENT_CLAIM,
	AST_STATEMENT_ALIAS,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_INCREMENT,
	AST_STATEMENT_DECREMENT,
	AST_STATEMENT_EXPRESSION,
	AST_STATEMENT_VARIABLE_DECLARATION,
	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_ASSIGNMENT_ADD,
	AST_STATEMENT_ASSIGNMENT_SUBTRACT,
	AST_STATEMENT_ASSIGNMENT_MULTIPLY,
	AST_STATEMENT_ASSIGNMENT_DIVIDE,
	AST_STATEMENT_ASSIGNMENT_POWER,
};

// @Todo: Redo this branch shit
//       this is ECH!
enum Ast_Branch_Kind
{
	AST_BRANCH_INIT,
	AST_BRANCH_ELSE,
	AST_BRANCH_THEN,
};

struct Ast_Branch
{
	Ast_Branch_Kind kind;
	Token* token;
	Ast_Expression* condition;
	Ast_Branch* else_branch;
	Ast_Branch* then_branch;
	Ast_Code code;
};

struct Ast_BranchBlock
{
	List<Ast_Branch> branches;
};

struct Ast_Defer
{
	Token* token;
	Ast_Code code;
};

struct Ast_Alias
{
	Token* token;
	Ast_Expression* expression; // ????
	// Token for identifier here?
};

struct Ast_Return
{
	Token* token;
	Ast_Expression* expression;
};

struct Ast_Claim
{
	Token* token;
	Ast_Expression* expression;
};

struct Ast_Increment
{
	Token* token;
	Ast_Expression* expression;
};

struct Ast_Decrement
{
	Token* token;
	Ast_Expression* expression;
};

struct Ast_VariableDeclaration
{
	Token* name;
	bool is_parameter;
	bool can_constantly_evaluate;
	bool is_pure;
	bool is_global;
	u64  offset;
	Ast_Type* explicit_type;
	Type* type;
	Ast_Expression* assignment;
	Ast_Attribute attribute;
};

struct StackFrame
{
	char* data;
	Ast_Function* function;
	bool do_return;
	bool do_break;

	inline char* GetData(Ast_VariableDeclaration* variable)
	{
		return data + variable->offset;
	}
};

struct Ast_Assignment
{
	Token* token;
	Ast_Expression* left;
	Ast_Expression* right;
};

struct Ast_Break
{
	Token* token;
};

struct Ast_Statement
{
	Ast_Statement_Kind kind;

	union
	{
		Token*                  token;
		Ast_Assignment          assignment;
		Ast_BranchBlock         branch_block;
		Ast_Defer               defer;
		Ast_Claim               claim;
		Ast_Alias               alias;
		Ast_Break               brk;
		Ast_Return              ret;
		Ast_VariableDeclaration variable_declaration;
		Ast_Expression*         expression;
		Ast_Increment           increment;
		Ast_Decrement           decrement;
	};
};

struct Ast_Function
{
	Token* name;
	List<Ast_VariableDeclaration> parameters;
	Type* type;
	Ast_Type* ast_return_type;
	Type* return_type;
	Ast_Code code;
	Ast_Attribute attribute;
	bool is_pure;
	bool does_return;
	bool is_global;
};

struct Ast_Import
{
	Token* token;
	Token* module;
};

struct Ast_Struct_Member
{
	Token* name;
	Ast_Type type;
	u64 offset;
	Ast_Attribute attribute;
};

struct Ast_Enum_Member
{
	Token* name;
	Ast_Expression* expression;
	Ast_Attribute attribute;
};

struct Ast_Struct
{
	Token* name;
	Type type;
	List<Ast_Struct_Member> members;
	List<Ast_Struct*> closure;
	Ast_Attribute attribute;
};

struct Ast_Enum
{
	Token* name;
	Type type;
	List<Ast_Enum_Member> members;
	Ast_Attribute attribute;
};

struct Ast_Root
{
	List<Ast_Import> imports;
	Ast_Scope scope;
};

struct Parse_Info
{
	Stack_Allocator stack;
	List<Token> tokens;
	List<Span<char>> lines;
	Span<char> code;
	Ast_Root* ast_root;
	String file_path;
};

struct MemoryBlock
{
	char* head;
	u64   size;
	MemoryBlock* prev;
	MemoryBlock* next;
	char  data[];
};

struct Interpreter
{
	MemoryBlock* block;
};

Parse_Info LexicalParse(String file_path);
void ParseFile(String file_path);
void SemanticParse(Parse_Info* info);
void Interpret(Ast_Code* code, char* output, StackFrame* frame, Interpreter* interpreter);
void Interpret(Ast_Function* function, char* input, char* output, Interpreter* interpreter);
void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter);
StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter);

static bool IsPrimitive(Type* type, Token_Kind primitive)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE && type->primitive == primitive;
}

static bool IsConvertableToBool(Type* type)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE
		|| type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_SPECIFIER_OPTIONAL
		|| type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsIntegerType(Type* type)
{
	return IsPrimitive(type, TOKEN_INT8)
		|| IsPrimitive(type, TOKEN_INT16)
		|| IsPrimitive(type, TOKEN_INT32)
		|| IsPrimitive(type, TOKEN_INT64)
		|| IsPrimitive(type, TOKEN_UINT8)
		|| IsPrimitive(type, TOKEN_UINT16)
		|| IsPrimitive(type, TOKEN_UINT32)
		|| IsPrimitive(type, TOKEN_UINT64);
}

static bool IsSignedIntegerType(Type* type)
{
	return IsPrimitive(type, TOKEN_INT8)
		|| IsPrimitive(type, TOKEN_INT16)
		|| IsPrimitive(type, TOKEN_INT32)
		|| IsPrimitive(type, TOKEN_INT64);
}

static bool IsUnsignedIntegerType(Type* type)
{
	return IsPrimitive(type, TOKEN_UINT8)
		|| IsPrimitive(type, TOKEN_UINT16)
		|| IsPrimitive(type, TOKEN_UINT32)
		|| IsPrimitive(type, TOKEN_UINT64);
}

static bool IsFloatType(Type* type)
{
	return IsPrimitive(type, TOKEN_FLOAT16)
		|| IsPrimitive(type, TOKEN_FLOAT32)
		|| IsPrimitive(type, TOKEN_FLOAT64);
}

static bool IsNumericalType(Type* type)
{
	return type->kind == TYPE_BASETYPE_PRIMITIVE
		|| type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsIntegerLikeType(Type* type)
{
	return IsIntegerType(type)
		|| IsPrimitive(type, TOKEN_BOOL)
		|| type->kind == TYPE_SPECIFIER_POINTER
		|| type->kind == TYPE_BASETYPE_ENUM;
}

static bool IsPointer(Type* type)
{
	return type->kind == TYPE_SPECIFIER_POINTER;
}

static bool IsOptional(Type* type)
{
	return type->kind == TYPE_SPECIFIER_OPTIONAL;
}

static bool AreTypesCompatible(Type* a, Type* b)
{
	return a == b || (a->kind == TYPE_BASETYPE_PRIMITIVE && b->kind == TYPE_BASETYPE_PRIMITIVE)
		|| (IsPointer(a) && IsPointer(b)); // @TestMe: This might produce semantic bugs somewhere.
}


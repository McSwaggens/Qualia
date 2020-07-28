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
	u64 size;
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
	AST_EXPRESSION_UNARY,
	AST_EXPRESSION_BINARY,
	AST_EXPRESSION_CALL,
	AST_EXPRESSION_SUBSCRIPT,
	AST_EXPRESSION_LAMBDA,
	AST_EXPRESSION_TUPLE,
	AST_EXPRESSION_AS,
	AST_EXPRESSION_IF_ELSE,
};

struct Ast_Expression
{
	Ast_Expression_Kind kind;
	Token* token;
	Type*  type;

	union
	{
		Ast_Expression* left;
		Ast_VariableDeclaration* variable;
		Ast_Function* function;
		Ast_Struct* structure;
		Ast_Enum* enumeration;
		Ast_Struct_Member* struct_member;
		Ast_Enum_Member* enum_member;
	};

	union
	{
		Ast_Expression* right;
		Ast_Expression** begin;
	};

	union
	{
		Ast_Expression* middle;
		Ast_Expression** end;
	};
};

struct Ast_Scope
{
	Ast_Scope* parent;
	List<Ast_Function> functions;
	List<Ast_Struct> structs;
	List<Ast_Enum> enums;
	List<Ast_VariableDeclaration*> variables;
};

struct Ast_Code
{
	List<Ast_Statement> statements;
	Ast_Scope scope;
	bool does_return;
};

enum Ast_Statement_Kind 
{
	AST_STATEMENT_BRANCH_BLOCK,
	AST_STATEMENT_DEFER,
	AST_STATEMENT_ALIAS,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_EXPRESSION,
	AST_STATEMENT_VARIABLE_DECLARATION,
	AST_STATEMENT_ASSIGNMENT,
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

struct Ast_VariableDeclaration
{
	Token* name;
	bool is_parameter;
	Ast_Type* explicit_type;
	Type* type;
	Ast_Expression* assignment;
	Ast_Attribute attribute;
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
		Ast_BranchBlock         branch;
		Ast_Defer               defer;
		Ast_Alias               alias;
		Ast_Break               brk;
		Ast_Return              ret;
		Ast_VariableDeclaration variable_declaration;
		Ast_Expression*         expression;
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
	bool visited;
	bool scanned;
	bool returns_value;
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

Parse_Info LexicalParse(String file_path);
void ParseFile(String file_path);
void SemanticParse(Parse_Info* info);


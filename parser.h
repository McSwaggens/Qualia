#pragma once

#include "int.h"
#include "string.h"
#include "list.h"
#include "token.h"
#include "span.h"

struct Ast_Expression;
struct Ast_Statement;
struct Ast_Specifier;
struct Ast_BaseType;
struct Ast_Type;
struct Ast_Function;
struct Ast_Struct;
struct Ast_Enum;
struct Ast_Code;
struct Ast_Attribute;

struct Ast_Attribute
{
	Token* token;
	Ast_Expression* expression;
};

enum Ast_Specifier_Kind
{
	AST_SPECIFIER_POINTER,
	AST_SPECIFIER_ARRAY,
	AST_SPECIFIER_OPTIONAL
};

struct Ast_Specifier
{
	Ast_Specifier_Kind kind;
	Token* token; // @Optimization: s[n].token = s[n-1].token+1
	Ast_Expression* size_expression;
};

enum Ast_BaseType_Kind
{
	AST_BASETYPE_USERTYPE,
	AST_BASETYPE_PRIMITIVE,
	AST_BASETYPE_FUNCTION,
	AST_BASETYPE_TUPLE
};

struct Ast_BaseType_Primitive
{
	Token* token;
};

struct Ast_BaseType_UserType
{
	Token* token;
};

struct Ast_BaseType_Tuple
{
	List<Ast_Type> types;
};

struct Ast_BaseType_Function
{
	List<Ast_Type> input;
	Ast_Type* output;
};

struct Ast_BaseType
{
	Ast_BaseType_Kind kind;

	union
	{
		Ast_BaseType_Tuple     tuple;
		Ast_BaseType_Primitive primitive;
		Ast_BaseType_UserType  usertype;
		Ast_BaseType_Function  function;
	};
};

struct Ast_Type
{
	List<Ast_Specifier> specifiers;
	Ast_BaseType basetype;
};

enum Ast_Expression_Kind
{
	AST_EXPRESSION_TERMINAL,
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
	Ast_Expression* left;

	union
	{
		Ast_Expression* right;
		Ast_Expression** begin;
	};

	union
	{
		Ast_Type type;
		Ast_Expression* middle;
		Ast_Expression** end;
	};
};

struct Ast_Code
{
	// @Todo Consolidate this into one big allocation?
	List<Ast_Statement> statements;
	List<Ast_Function> functions;
	List<Ast_Struct> structs;
	List<Ast_Enum> enums;
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
	Ast_Attribute attribute;
	Ast_Type* type;
	Ast_Expression* assignment;
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
		Ast_Return              ret;
		Ast_VariableDeclaration variable_declaration;
		Ast_Expression*         expression;
	};
};

struct Ast_Param
{
	Ast_Type type;
	Token* name;
};

struct Ast_Function
{
	Token*          name;
	Ast_Attribute   attribute;
	List<Ast_Param> params;
	Ast_Type*       return_type;
	Ast_Code        code;
};

struct Ast_Import
{
	Token* token;
	Token* module;
};

struct Ast_Struct_Member
{
	Token* name;
	Ast_Attribute attribute;
	Ast_Type type;
};

struct Ast_Enum_Member
{
	Token* name;
	Ast_Attribute attribute;
	Ast_Expression* expression;
};

struct Ast_Struct
{
	Token* name;
	Ast_Attribute attribute;
	List<Ast_Struct_Member> members;
};

struct Ast_Enum
{
	Token* name;
	Ast_Attribute attribute;
	List<Ast_Enum_Member> members;
};

struct Ast_Root
{
	List<Ast_Import>   imports;
	List<Ast_Function> functions;
	List<Ast_Struct>   structs;
	List<Ast_Enum>     enums;
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

Parse_Info ParseFile(String file_path);
Parse_Info LexicalParse(String file_path);


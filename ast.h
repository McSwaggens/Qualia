#pragma once

#include "string.h"
#include "list.h"
#include "token.h"
#include "type_system.h"
#include "ir.h"

// Forward declarations - these are defined in parser.h
struct Line;
enum IntrinsicID : u32;

namespace Ast {

struct Expression;
struct Expression_Tuple;
struct Variable;
struct Import;
struct Struct_Member;
struct Enum_Member;
struct Statement;
struct Defer;
struct Specifier;
struct BaseType;
struct Type;
struct Function;
struct Struct;
struct Enum;
struct Code;
struct Scope;
struct Return;
struct Break;

enum Specifier_Kind {
	SPECIFIER_POINTER,
	SPECIFIER_OPTIONAL,
	SPECIFIER_FIXED_ARRAY,
	SPECIFIER_ARRAY
};

struct Specifier {
	Specifier_Kind kind;
	Token* token;
	Expression* size_expression;
};

enum BaseType_Kind {
	BASETYPE_USERTYPE,
	BASETYPE_PRIMITIVE,
	BASETYPE_FUNCTION,
	BASETYPE_TUPLE
};

struct BaseType_Function {
	Type* input;
	Type* output;
};

struct BaseType {
	BaseType_Kind kind;
	Token* token;

	union
	{
		Array<Type> tuple;
		BaseType_Function function;
	};
};

struct Type {
	Array<Specifier> specifiers = { };
	BaseType basetype;
};

struct Expression {
	enum Kind {
		TERMINAL_NAME,
		TERMINAL_FUNCTION,
		TERMINAL_INTRINSIC,
		TERMINAL_LITERAL,
		TERMINAL_VARIABLE,
		TERMINAL_STRUCT,
		TERMINAL_ENUM,
		TERMINAL_PRIMITIVE,
		TERMINAL_STRUCT_MEMBER,
		TERMINAL_ENUM_MEMBER,
		TERMINAL_ARRAY_LENGTH,
		TERMINAL_ARRAY_BEGIN,
		TERMINAL_ARRAY_END,
		UNARY_BITWISE_NOT,
		UNARY_NOT,
		UNARY_MINUS,
		UNARY_PLUS,
		UNARY_REFERENCE_OF,
		UNARY_ADDRESS_OF,
		BINARY_COMPARE_EQUAL,
		BINARY_COMPARE_NOT_EQUAL,
		BINARY_COMPARE_LESS,
		BINARY_COMPARE_LESS_OR_EQUAL,
		BINARY_COMPARE_GREATER,
		BINARY_COMPARE_GREATER_OR_EQUAL,
		BINARY_DOT,
		BINARY_ADD,
		BINARY_SUBTRACT,
		BINARY_MULTIPLY,
		BINARY_DIVIDE,
		BINARY_MODULO,
		BINARY_BITWISE_OR,
		BINARY_BITWISE_XOR,
		BINARY_BITWISE_AND,
		BINARY_LEFT_SHIFT,
		BINARY_RIGHT_SHIFT,
		BINARY_AND,
		BINARY_OR,
		IF_ELSE,
		IMPLICIT_CAST,
		CALL,
		DOT_CALL,
		SUBSCRIPT,
		LAMBDA,
		TUPLE,
		ARRAY,
		FIXED_ARRAY,
		AS,
	};

	Kind kind;
	IR::Value value;
	TypeID type;
	Token* begin;
	Token* end;

	Expression(Kind kind, IR::Value value, TypeID type, Token* begin, Token* end) :
		kind(kind),
		value(value),
		type(type),
		begin(begin),
		end(end)
	{ }
};

struct Expression_Implicit_Cast : Expression {
	Expression* subexpression;

	Expression_Implicit_Cast(Expression* subexpr, TypeID target_type) :
		Expression(IMPLICIT_CAST, IR::NewValue(), target_type, subexpr->begin, subexpr->end),
		subexpression(subexpr) { }
};

struct Expression_Unary : Expression {
	Expression* subexpression;
	Token* op;

	Expression_Unary(Kind k, Token* op_token) :
		Expression(k, IR::NewValue(), TYPE_NULL, null, null),
		subexpression(null),
		op(op_token) { }
};

struct Expression_Binary : Expression {
	Expression* left;
	Expression* right;
	Token* op;

	Expression_Binary(Kind k, Token* op_token) :
		Expression(k, IR::NewValue(), TYPE_NULL, null, null),
		left(null),
		right(null),
		op(op_token) { }
};

struct Expression_Ternary : Expression {
	Expression* left;
	Expression* middle;
	Expression* right;
	Token* ops[2];

	Expression_Ternary(Kind k, Token* op1, Token* op2) :
		Expression(k, IR::NewValue(), TYPE_NULL, null, null),
		left(null),
		middle(null),
		right(null),
		ops{op1, op2} { }
};

struct Expression_Call : Expression {
	Expression* function;
	Expression_Tuple* parameters;

	Expression_Call() :
		Expression(CALL, IR::NewValue(), TYPE_NULL, null, null),
		function(null),
		parameters(null) { }
};

struct Expression_Dot_Call : Expression {
	Expression_Binary* dot;
	Expression_Tuple* parameters;

	Expression_Dot_Call() :
		Expression(DOT_CALL, IR::NewValue(), TYPE_NULL, null, null),
		dot(null),
		parameters(null) { }
};

struct Expression_Array : Expression {
	Expression* left;
	Expression* right;

	Expression_Array() :
		Expression(ARRAY, IR::NewValue(), TYPE_NULL, null, null),
		left(null),
		right(null) { }
};

struct Expression_Tuple : Expression {
	Array<Expression*> elements;
	u32 recursive_count;

	Expression_Tuple() :
		Expression(TUPLE, IR::NewValue(), TYPE_NULL, null, null),
		elements{ },
		recursive_count(0) { }
};

struct Expression_Fixed_Array : Expression {
	Array<Expression*> elements;

	Expression_Fixed_Array() :
		Expression(FIXED_ARRAY, IR::NewValue(), TYPE_NULL, null, null),
		elements{ } { }
};

struct Expression_As : Expression {
	Expression* expression;
	Type ast_type;
	Token* op;

	Expression_As(Token* op_token) :
		Expression(AS, IR::NewValue(), TYPE_NULL, null, null),
		expression(null),
		ast_type{ },
		op(op_token) { }
};

struct Expression_Literal : Expression {
	Token* token;

	Expression_Literal(Token* tok) :
		Expression(TERMINAL_LITERAL,
		           IR::NewValue(), TYPE_NULL, null, null),
		token(tok) { }
};

struct Expression_Subscript : Expression {
	Expression* array;
	Expression* index;

	Expression_Subscript() :
		Expression(SUBSCRIPT, IR::NewValue(), TYPE_NULL, null, null),
		array(null),
		index(null) { }
};

struct Expression_Terminal : Expression {
	Token* token;
	void* ptr;

	Expression_Terminal(Token* tok) :
		Expression(TERMINAL_NAME, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		ptr(null) { }
};

struct Expression_Variable : Expression {
	Token* token;
	Variable* variable;

	Expression_Variable(Token* tok, Variable* var) :
		Expression(TERMINAL_VARIABLE, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		variable(var) { }
};

struct Expression_Function : Expression {
	Token* token;
	Function* function;

	Expression_Function(Token* tok, Function* func) :
		Expression(TERMINAL_FUNCTION, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		function(func) { }
};

struct Expression_Intrinsic : Expression {
	Token* token;
	IntrinsicID intrinsic;

	Expression_Intrinsic(Token* tok, IntrinsicID intr) :
		Expression(TERMINAL_INTRINSIC, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		intrinsic(intr) { }
};

struct Expression_Struct : Expression {
	Token* token;
	Struct* structure;

	Expression_Struct(Token* tok, Struct* struc) :
		Expression(TERMINAL_STRUCT, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		structure(struc) { }
};

struct Expression_Enum : Expression {
	Token* token;
	Enum* enumeration;

	Expression_Enum(Token* tok, Enum* enu) :
		Expression(TERMINAL_ENUM, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		enumeration(enu) { }
};

struct Expression_Struct_Member : Expression {
	Token* token;
	Struct_Member* member;

	Expression_Struct_Member(Token* tok, Struct_Member* mem) :
		Expression(TERMINAL_STRUCT_MEMBER, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		member(mem) { }
};

struct Expression_Enum_Member : Expression {
	Token* token;
	Enum_Member* member;

	Expression_Enum_Member(Token* tok, Enum_Member* mem) :
		Expression(TERMINAL_ENUM_MEMBER, IR::NewValue(), TYPE_NULL, null, null),
		token(tok),
		member(mem) { }
};

struct Scope {
	Scope* parent;
	Array<Function> functions;
	Array<Struct> structs;
	Array<Enum> enums;
	List<Variable*> variables;
};

struct Code {
	Array<Statement> statements;
	List<Defer*> defers;
	Scope scope;
	u64 frame_size;
	bool contains_return;
	bool contains_break;
	bool all_paths_return;
	bool all_paths_break;
	bool is_inside_loop;
	bool has_defer_that_returns;
};

enum Statement_Kind {
	STATEMENT_EXPRESSION,
	STATEMENT_VARIABLE_DECLARATION,

	STATEMENT_ASSIGNMENT,
	STATEMENT_ASSIGNMENT_ADD,
	STATEMENT_ASSIGNMENT_SUBTRACT,
	STATEMENT_ASSIGNMENT_MULTIPLY,
	STATEMENT_ASSIGNMENT_DIVIDE,
	STATEMENT_ASSIGNMENT_XOR,

	STATEMENT_INCREMENT,
	STATEMENT_DECREMENT,

	STATEMENT_RETURN,
	STATEMENT_BREAK,

	STATEMENT_CLAIM,

	STATEMENT_BRANCH_BLOCK,
	STATEMENT_DEFER,
};

enum Branch_Clause_Kind : u8 {
	BRANCH_CLAUSE_INIT = 0,
	BRANCH_CLAUSE_ELSE,
	BRANCH_CLAUSE_THEN
};

enum Branch_Kind : u8 {
	BRANCH_NAKED = 0,

	// if bool:
	BRANCH_IF,

	// while bool:
	BRANCH_WHILE,

	// for it in []T:
	// for it in []T where bool:
	// for it in []T, int:
	// for it in []T, int where bool:
	BRANCH_FOR_RANGE,

	// for vardecl, bool:
	// for vardecl, bool, int:
	BRANCH_FOR_VERBOSE,
};

struct Branch_For_Range {
	Variable*   iterator;
	Expression* range;
	Expression* filter;
};

struct Branch_For_Verbose {
	Variable* variable;
	Expression* condition;
	Expression* next;
};

struct Branch {
	Branch_Kind kind;
	Branch_Clause_Kind clause;

	union {
		Expression* if_condition;
		Expression* while_condition;
		Branch_For_Range for_range;
		Branch_For_Verbose for_verbose;
	};

	Branch* else_branch;
	Branch* then_branch;

	Code code;
};

struct BranchBlock {
	Array<Branch> branches;
};

struct Defer {
	Token* token;
	Code code;
	// Defer* next;
};

struct Return {
	Token* token;
	Expression* expression;
	// Defer* defer;
	// @Todo: Prevent return statement if a prior defer in defer chain contains a return statement.
};

struct Break {
	Token* token;
	// BranchBlock* block;
	// Branch* branch;
	// Defer* defer_begin;
	// Defer* defer_end;
};

struct Claim {
	Token* token;
	Expression* expression;
};

struct Increment {
	Token* token;
	Expression* expression;
};

using Variable_Flags = u8;
static const Variable_Flags VARIABLE_FLAG_PARAMETER = (1<<0);
static const Variable_Flags VARIABLE_FLAG_GLOBAL    = (1<<1);
static const Variable_Flags VARIABLE_FLAG_CONSTANT  = (1<<2);
static const Variable_Flags VARIABLE_FLAG_ITERATOR  = (1<<3);

struct Variable {
	String name;
	Token* name_token;
	Variable_Flags flags;
	TypeID type;
	Type* ast_type;
	Expression* assignment;
	IR::Value address;
};

struct Assignment {
	Token* token;
	Expression* left;
	Expression* right;
};

struct Statement {
	Statement_Kind kind;

	union {
		Assignment  assignment;
		BranchBlock branch_block;
		Defer       defer;
		Claim       claim;
		Break       brk;
		Return      ret;
		Variable    variable_declaration; // @FixMe @Optimization: Change to pointer, Variable is yuuuge!
		Increment   increment;
		Expression* expression = null;
	};
};

struct Function {
	String name;
	Token* name_token;
	Array<Variable> parameters; // @Todo: Give parameters their own struct? Parameter?
	Code code;
	TypeID type;
	TypeID return_type;
	List<Return*> returns; // @Todo: Infer return type.
	Type* ast_return_type;
	// Procedure* procedure;
	bool is_pure;
	bool is_global;
};

struct Import {
	Token* token;
	Token* module;
	// @Todo: Finalize import syntax.
	// @Todo: Allow for 'naming' with 'as' keyword.
};

struct Struct_Member {
	String name;
	Token* name_token;
	TypeID type;
	Type ast_type = { };
	u64 offset;
	u32 index;
};

struct Enum_Member {
	String name;
	Token* name_token;
	Expression* expression;
	s64 value;
	u32 index;
};

struct Struct {
	TypeID type;
	String name;
	Token* name_token;
	Array<Struct_Member> members;
	List<Struct*> closure;
};

struct Enum {
	TypeID type;
	String name;
	Token* name_token;
	TypeID underlying_type;
	Array<Enum_Member> members;
};

struct Module {
	Scope scope = { };

	String code = { };
	String file_path;
	String name;

	Array<Token> tokens = { };
	Array<Line> lines = { };

	Array<Module*> users = { };
	Array<Import> imports = { };

	Module(String file_path, String name) :
		file_path(file_path),
		name(name)
	{ }
};

void PrintAST(Module* module);

} // namespace Ast

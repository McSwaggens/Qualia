#pragma once

#include "string.h"
#include "list.h"
#include "token.h"
#include "type_system.h"
#include "ir.h"
#include "file_system.h"

enum IntrinsicID {
	INTRINSIC_SYSTEM_CALL = 0,

	INTRINSIC_COUNT,
	INTRINSIC_INVALID = INTRINSIC_COUNT
};

struct Line {
	Indent16 indent;
	String   string;
	Token*   tokens_begin;
	Token*   tokens_end;
};

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

typedef u8 Expression_Flags;
static const Expression_Flags EXPRESSION_FLAG_REFERENTIAL            = (1<<0);
static const Expression_Flags EXPRESSION_FLAG_PURE                   = (1<<1);
static const Expression_Flags EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE = (1<<2);
static const Expression_Flags EXPRESSION_FLAG_INTERNALLY_REFERENTIAL = (1<<3);

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
	Expression_Flags flags;
	TypeID type;
	Token* begin;
	Token* end;
};

struct Expression_Implicit_Cast : Expression {
	Expression* subexpression;
};

struct Expression_Unary : Expression {
	Expression* subexpression;
	Token* op;
};

struct Expression_Binary : Expression {
	Expression* left;
	Expression* right;
	Token* op;
};

struct Expression_Ternary : Expression {
	Expression* left;
	Expression* middle;
	Expression* right;
	Token* ops[2];
};

struct Expression_Call : Expression {
	Expression* function;
	Expression_Tuple* parameters;
};

struct Expression_Dot_Call : Expression {
	Expression_Binary* dot;
	Expression_Tuple* parameters;
};

struct Expression_Array : Expression {
	Expression* left;
	Expression* right;
};

struct Expression_Tuple : Expression {
	Array<Expression*> elements;
	u32 recursive_count;
};

struct Expression_Fixed_Array : Expression {
	Array<Expression*> elements;
};

struct Expression_As : Expression {
	Expression* expression;
	Type ast_type;
	Token* op;
};

struct Expression_Literal : Expression {
	Token* token;
	IR::Value value = 0;

	union
	{
		s64     value_int;
		float32 value_f32;
		float64 value_f64;
	};
};

struct Expression_Subscript : Expression {
	Expression* array;
	Expression* index;
};

struct Expression_Terminal : Expression {
	Token* token;
	void* ptr;
};

struct Expression_Variable : Expression {
	Token* token;
	Variable* variable;
};

struct Expression_Function : Expression {
	Token* token;
	Function* function;
};

struct Expression_Intrinsic : Expression {
	Token* token;
	IntrinsicID intrinsic;
};

struct Expression_Struct : Expression {
	Token* token;
	Struct* structure;
};

struct Expression_Enum : Expression {
	Token* token;
	Enum* enumeration;
};

struct Expression_Struct_Member : Expression {
	Token* token;
	Struct_Member* member;
};

struct Expression_Enum_Member : Expression {
	Token* token;
	Enum_Member* member;
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

enum Statement_Kind  {
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
	// Block* entry_block;

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
	Stack stack;
	Scope scope;

	Array<char> code;
	String file_path;
	String name;

	Array<Token> tokens;
	Array<Line> lines;

	Array<Module*> users;
	Array<Import> imports;
};

} // namespace Ast

struct StackFrame
{
	char* data;
	Ast::Function* function;
	bool do_return;
	bool do_break;
};

struct Parser {
	Ast::Module* module;
	Token* token;

	template<typename... Args>
	[[noreturn]] void Error(String format, Args&&... args);

	void ParseGlobalScope();
	Ast::Struct ParseStruct(u32 indent);
	Ast::Enum ParseEnum(u32 indent);
	Ast::Expression* ParseExpression(u32 indent, bool assignment_break = false, s32 parent_precedence = 0);
	Ast::Type ParseType(u32 indent);
	void ParseParameters(Ast::Function* function, Token* open_paren, u32 indent);
	Ast::BranchBlock ParseBranchBlock(u32 indent);
	Ast::Statement ParseStatement(u32 indent);
	Ast::Code ParseCode(u32 indent);
	Ast::Function ParseFunction(u32 indent);
	Ast::Import ParseImport(u32 indent);
};

static void SemanticParse(Ast::Module* module);
static u64 CalculateStackFrameSize(Ast::Function* function);
static u64 CalculateStackFrameSize(Ast::Code* code, u64 offset);
static void ScanExpression(Ast::Expression* expression, Ast::Scope* scope, Ast::Module* module);
static void ScanScope(Ast::Scope* scope, Ast::Module* module);
static void ScanCode(Ast::Code* code, Ast::Scope* scope, Ast::Function* function, Ast::Module* module);
static void ScanFunction(Ast::Function* function, Ast::Scope* scope, Ast::Module* module);

static void GenericWrite(OutputBuffer* buffer, Ast::Expression* expression);
static void GenericWrite(OutputBuffer* buffer, Ast::Type* type);
static void GenericWrite(OutputBuffer* buffer, Ast::Type type);
static void GenericWrite(OutputBuffer* buffer, TypeID type);

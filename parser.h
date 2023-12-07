#pragma once

#include "string.h"
#include "list.h"
#include "token.h"
#include "type_system.h"
#include "ir.h"

struct Ast_Expression;
struct Ast_Expression_Tuple;
struct Ast_Variable;
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
struct Ast_Return;
struct Ast_Break;
struct Ast_Attribute;
struct StackFrame;

enum IntrinsicID
{
	INTRINSIC_SYSTEM_CALL = 0,

	INTRINSIC_COUNT,
	INTRINSIC_INVALID = INTRINSIC_COUNT
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
	AST_SPECIFIER_FIXED_ARRAY,
	AST_SPECIFIER_ARRAY
};

struct Ast_Specifier
{
	Ast_Specifier_Kind kind;
	Token* token;
	Ast_Expression* size_expression;
};

enum Ast_BaseType_Kind
{
	AST_BASETYPE_USERTYPE,
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

	union
	{
		Array<Ast_Type> tuple;
		Ast_BaseType_Function function;
	};
};

struct Ast_Type
{
	Array<Ast_Specifier> specifiers;
	Ast_BaseType basetype;
};

enum Ast_Expression_Kind
{
	AST_EXPRESSION_TERMINAL_NAME,
	AST_EXPRESSION_TERMINAL_FUNCTION,
	AST_EXPRESSION_TERMINAL_INTRINSIC,
	AST_EXPRESSION_TERMINAL_LITERAL,
	AST_EXPRESSION_TERMINAL_VARIABLE,
	AST_EXPRESSION_TERMINAL_STRUCT,
	AST_EXPRESSION_TERMINAL_ENUM,
	AST_EXPRESSION_TERMINAL_PRIMITIVE,
	AST_EXPRESSION_TERMINAL_STRUCT_MEMBER,
	AST_EXPRESSION_TERMINAL_ENUM_MEMBER,
	AST_EXPRESSION_TERMINAL_ARRAY_LENGTH,
	AST_EXPRESSION_TERMINAL_ARRAY_BEGIN,
	AST_EXPRESSION_TERMINAL_ARRAY_END,
	AST_EXPRESSION_UNARY_BITWISE_NOT,
	AST_EXPRESSION_UNARY_NOT,
	AST_EXPRESSION_UNARY_MINUS,
	AST_EXPRESSION_UNARY_PLUS,
	AST_EXPRESSION_UNARY_REFERENCE_OF,
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
	// AST_EXPRESSION_BINARY_RANGE,
	AST_EXPRESSION_IF_ELSE,
	AST_EXPRESSION_IMPLICIT_CAST,
	AST_EXPRESSION_CALL,
	AST_EXPRESSION_DOT_CALL,
	AST_EXPRESSION_SUBSCRIPT,
	AST_EXPRESSION_LAMBDA,
	AST_EXPRESSION_TUPLE,
	AST_EXPRESSION_ARRAY,
	AST_EXPRESSION_FIXED_ARRAY,
	AST_EXPRESSION_AS,
};

typedef uint8 Ast_Expression_Flags;
static const Ast_Expression_Flags AST_EXPRESSION_FLAG_REFERENTIAL            = (1<<0);
static const Ast_Expression_Flags AST_EXPRESSION_FLAG_PURE                   = (1<<1);
static const Ast_Expression_Flags AST_EXPRESSION_FLAG_CONSTANTLY_EVALUATABLE = (1<<2);
static const Ast_Expression_Flags AST_EXPRESSION_FLAG_INTERNALLY_REFERENTIAL = (1<<3);

struct Ast_Expression
{
	Ast_Expression_Kind kind;
	Ast_Expression_Flags flags;
	TypeID type;
	Token* begin;
	Token* end;
};

struct Ast_Expression_Implicit_Cast : Ast_Expression
{
	Ast_Expression* subexpression;
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
	Ast_Expression_Tuple* parameters;
};

struct Ast_Expression_Dot_Call : Ast_Expression
{
	Ast_Expression_Binary* dot;
	Ast_Expression_Tuple* parameters;
};

struct Ast_Expression_Array : Ast_Expression
{
	Ast_Expression* left;
	Ast_Expression* right;
};

struct Ast_Expression_Tuple : Ast_Expression
{
	Array<Ast_Expression*> elements;
	uint32 recursive_count;
};

struct Ast_Expression_Fixed_Array : Ast_Expression
{
	Array<Ast_Expression*> elements;
};

struct Ast_Expression_As : Ast_Expression
{
	Ast_Expression* expression;
	Ast_Type ast_type;
	Token* op;
};

struct Ast_Expression_Literal : Ast_Expression
{
	Token* token;

	union
	{
		bool    value_bool;
		int64   value_int;
		float16 value_f16;
		float32 value_f32;
		float64 value_f64;
	};
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
	Ast_Variable* variable;
};

struct Ast_Expression_Function : Ast_Expression
{
	Token* token;
	Ast_Function* function;
};

struct Ast_Expression_Intrinsic : Ast_Expression
{
	Token* token;
	IntrinsicID intrinsic;
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
	Array<Ast_Function> functions;
	Array<Ast_Struct> structs;
	Array<Ast_Enum> enums;
	List<Ast_Variable*> variables;
	List<StackFrame> stack_frames;
};

struct Ast_Code
{
	Array<Ast_Statement> statements;
	List<Ast_Defer*> defers;
	Ast_Scope scope;
	uint64 frame_size;
	bool contains_return;
	bool contains_break;
	bool all_paths_return;
	bool all_paths_break;
	bool is_inside_loop;
	bool has_defer_that_returns;
};

enum Ast_Statement_Kind 
{
	AST_STATEMENT_EXPRESSION,
	AST_STATEMENT_VARIABLE_DECLARATION,

	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_ASSIGNMENT_ADD,
	AST_STATEMENT_ASSIGNMENT_SUBTRACT,
	AST_STATEMENT_ASSIGNMENT_MULTIPLY,
	AST_STATEMENT_ASSIGNMENT_DIVIDE,
	AST_STATEMENT_ASSIGNMENT_EXPONENTIAL,

	AST_STATEMENT_INCREMENT,
	AST_STATEMENT_DECREMENT,

	AST_STATEMENT_RETURN,
	AST_STATEMENT_BREAK,

	AST_STATEMENT_CLAIM,

	AST_STATEMENT_BRANCH_BLOCK,
	AST_STATEMENT_DEFER,
};

enum Ast_Branch_Clause_Kind : uint8
{
	AST_BRANCH_CLAUSE_INIT = 0,
	AST_BRANCH_CLAUSE_ELSE,
	AST_BRANCH_CLAUSE_THEN
};

enum Ast_Branch_Kind : uint8
{
	AST_BRANCH_NAKED = 0,

	// if bool:
	AST_BRANCH_IF,

	// while bool:
	AST_BRANCH_WHILE,

	// for it in []T:
	// for it in []T where bool:
	// for it in []T, int:
	// for it in []T, int where bool:
	AST_BRANCH_FOR_RANGE,

	// for vardecl, bool:
	// for vardecl, bool, int:
	AST_BRANCH_FOR_VERBOSE,
};

struct Ast_Branch_For_Range
{
	Ast_Variable*   iterator;
	Ast_Expression* range;
	Ast_Expression* filter;
	Ast_Expression* stride;
};

struct Ast_Branch_For_Verbose
{
	Ast_Variable* variable;
	Ast_Expression* condition;
	Ast_Expression* stride;
};

struct Ast_Branch
{
	Ast_Branch_Kind kind;
	Ast_Branch_Clause_Kind clause;
	Block* ir;

	union
	{
		Ast_Expression* if_condition;
		Ast_Expression* while_condition;
		Ast_Branch_For_Range for_range;
		Ast_Branch_For_Verbose for_verbose;
	};

	Ast_Branch* else_branch;
	Ast_Branch* then_branch;

	Ast_Code code;
};

struct Ast_BranchBlock
{
	Array<Ast_Branch> branches;
};

struct Ast_Defer
{
	Token* token;
	Ast_Code code;
	// Ast_Defer* next;
};

struct Ast_Return
{
	Token* token;
	Ast_Expression* expression;
	// Ast_Defer* defer;
	// @Todo: Prevent return statement if a prior defer in defer chain contains a return statement.
};

struct Ast_Break
{
	Token* token;
	// Ast_BranchBlock* block;
	// Ast_Branch* branch;
	// Ast_Defer* defer_begin;
	// Ast_Defer* defer_end;
};

struct Ast_Claim
{
	Token* token;
	Ast_Expression* expression;
};

struct Ast_Increment // Nudge?
{
	Token* token;
	Ast_Expression* expression;
};

using Ast_Variable_Flags = uint8;
static const Ast_Variable_Flags AST_VARIABLE_FLAG_PARAMETER = (1<<0);
static const Ast_Variable_Flags AST_VARIABLE_FLAG_GLOBAL    = (1<<1);
static const Ast_Variable_Flags AST_VARIABLE_FLAG_CONSTANT  = (1<<2);
static const Ast_Variable_Flags AST_VARIABLE_FLAG_ITERATOR  = (1<<3);

struct Ast_Variable
{
	String name;
	Token* name_token;
	Ast_Variable_Flags flags;
	TypeID type;
	Ast_Type* ast_type;
	Ast_Expression* assignment;
	Ast_Attribute* attribute;
	Value ir;
	uint64 offset; // @RemoveMe?
	// @Todo: Add span
	Ast_Variable() = default;
};

struct StackFrame
{
	char* data;
	Ast_Function* function;
	bool do_return;
	bool do_break;
};

static inline char* StackFrameGetVariable(StackFrame* frame, Ast_Variable* variable)
{
	return frame->data + variable->offset;
}

struct Ast_Assignment
{
	Token* token;
	Ast_Expression* left;
	Ast_Expression* right;
};

struct Ast_Statement
{
	Ast_Statement_Kind kind;

	union
	{
		Ast_Assignment  assignment;
		Ast_BranchBlock branch_block;
		Ast_Defer       defer;
		Ast_Claim       claim;
		Ast_Break       brk;
		Ast_Return      ret;
		Ast_Variable    variable_declaration = Ast_Variable(); // @FixMe @Optimization: Change to pointer, Ast_Variable is yuuuge!
		Ast_Increment   increment;
		Ast_Expression* expression;
	};

	// Ast_Statement() = defau
};

struct Ast_Function
{
	String name;
	Token* name_token;
	Array<Ast_Variable> parameters; // @Todo: Give parameters their own struct? Ast_Parameter?
	Ast_Code code;
	TypeID type;
	TypeID return_type;
	List<Ast_Return*> returns; // @Todo: Infer return type.
	Ast_Type* ast_return_type;
	Ast_Attribute* attribute;
	Procedure* ir;
	bool is_pure;
	bool is_global;
	// @Todo: Expression functions =>
};

struct Ast_Import
{
	Token* token;
	Token* module;
	// @Todo: Finalize import syntax.
	// @Todo: Allow for 'naming' with 'as' keyword.
};

struct Ast_Struct_Member
{
	String name;
	Token* name_token;
	TypeID type;
	Ast_Type ast_type;
	uint64 offset;
	uint32 index;
};

struct Ast_Enum_Member
{
	String name;
	Token* name_token;
	Ast_Expression* expression;
	int64 value;
	uint32 index;
};

struct Ast_Struct
{
	TypeID type;
	String name;
	Token* name_token;
	Array<Ast_Struct_Member> members;
	List<Ast_Struct*> closure;
	Ast_Attribute* attribute;
};

struct Ast_Enum
{
	TypeID type;
	String name;
	Token* name_token;
	TypeID underlying_type;
	Array<Ast_Enum_Member> members;
	Ast_Attribute* attribute;
};

struct Line
{
	int16 indent;
	String string;
	int64 tokens_begin_index;
	int64 tokens_count;
};

struct Ast_Module
{
	Stack stack;
	Ast_Scope scope;

	String code;
	String file_path;
	String name;

	Array<Token> tokens;
	Array<Line> lines;
	Array<Ast_Module*> users;
	Array<Ast_Import> imports;
};

static void LexerParse(Ast_Module* module);
static void InitIntrinsicFunctions(Ast_Module* module);
static Ast_Module* ParseFile(String file_path);
static void SemanticParse(Ast_Module* module);
static uint64 CalculateStackFrameSize(Ast_Function* function);
static uint64 CalculateStackFrameSize(Ast_Code* code, uint64 offset);
static void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Ast_Module* module);
static void ScanScope(Ast_Scope* scope, Ast_Module* module);
static void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Ast_Module* module);
static void ScanFunction(Ast_Function* function, Ast_Scope* scope, Ast_Module* module);

static void GenericWrite(OutputBuffer* buffer, Ast_Expression* expression);
static void GenericWrite(OutputBuffer* buffer, Ast_Type* type);
static void GenericWrite(OutputBuffer* buffer, Ast_Type type);
static void GenericWrite(OutputBuffer* buffer, TypeID type);


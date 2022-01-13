#pragma once

#include "string.h"
#include "list.h"
#include "token.h"
#include "span.h"
#include "type.h"
#include "ir.h"

struct Ast_Expression;
struct Ast_Expression_Tuple;
struct Ast_VariableDeclaration;
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

using Ast_ControlFlow_Flags = u8;
const Ast_ControlFlow_Flags AST_CONTAINS_RETURN  = (1<<0);
const Ast_ControlFlow_Flags AST_CONTAINS_BREAK   = (1<<1);
const Ast_ControlFlow_Flags AST_ALL_PATHS_RETURN = (1<<2);
const Ast_ControlFlow_Flags AST_ALL_PATHS_BREAK  = (1<<3);

void Write(OutputBuffer* buffer, Ast_Expression* expression);
void Write(OutputBuffer* buffer, Ast_Type* type);
void Write(OutputBuffer* buffer, Ast_Type type);
void Write(OutputBuffer* buffer, Type* type);

using Intrinsic_Function_Type = void (*)(void*, void*);

struct Intrinsic_Function
{
	String name;
	Type* input;
	Type* output;
	Type* type;
	void (*function)(void* input, void* output);
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
	Type*  type;

	union
	{
		Array<Ast_Type> tuple;
		Ast_BaseType_Function function;
	};
};

struct Ast_Type // Change to 'Ast_Type_Description'?
{
	Array<Ast_Specifier> specifiers;
	Ast_BaseType basetype;
	Type* type;
};

enum Ast_Expression_Kind
{
	AST_EXPRESSION_TERMINAL_NAME,
	AST_EXPRESSION_TERMINAL_FUNCTION,
	AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION,
	AST_EXPRESSION_TERMINAL_LITERAL,
	AST_EXPRESSION_TERMINAL_VARIABLE,
	AST_EXPRESSION_TERMINAL_STRUCT,
	AST_EXPRESSION_TERMINAL_ENUM,
	AST_EXPRESSION_TERMINAL_PRIMITIVE,
	AST_EXPRESSION_TERMINAL_STRUCT_MEMBER,
	AST_EXPRESSION_TERMINAL_ENUM_MEMBER,
	AST_EXPRESSION_TERMINAL_ARRAY_LENGTH,
	AST_EXPRESSION_TERMINAL_ARRAY_DATA,
	AST_EXPRESSION_IMPLICIT_CAST,
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
	AST_EXPRESSION_CALL,
	AST_EXPRESSION_DOT_CALL,
	AST_EXPRESSION_SUBSCRIPT,
	AST_EXPRESSION_LAMBDA,
	AST_EXPRESSION_TUPLE,
	AST_EXPRESSION_DYNAMIC_ARRAY,
	AST_EXPRESSION_FIXED_ARRAY,
	AST_EXPRESSION_AS,
	AST_EXPRESSION_IF_ELSE,
};

struct Ast_Expression
{
	Ast_Expression_Kind kind;
	bool is_pure;
	bool can_constantly_evaluate;
	bool is_referential_value;
	Type* type;
	Span<Token> span;
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

struct Ast_Expression_Dynamic_Array : Ast_Expression
{
	Ast_Expression* left;
	Ast_Expression* right;
};

struct Ast_Expression_Tuple : Ast_Expression
{
	Array<Ast_Expression*> elements;
	u32 recursive_count;
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
		u8 value_byte;
		bool value_bool;

		char* value_pointer;

		s8  value_int8;
		s16 value_int16;
		s32 value_int32;
		s64 value_int64;

		u8  value_uint8;
		u16 value_uint16;
		u32 value_uint32;
		u64 value_uint64;

		f32 value_float32;
		f64 value_float64;
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
	Ast_VariableDeclaration* variable;
};

struct Ast_Expression_Function : Ast_Expression
{
	Token* token;
	Ast_Function* function;
};

struct Ast_Expression_Intrinsic_Function : Ast_Expression
{
	Token* token;
	Intrinsic_Function* intrinsic_function;
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
	List<Ast_VariableDeclaration*> variables;
	List<StackFrame> stack_frames;
};

struct Ast_Code
{
	Array<Ast_Statement> statements;
	List<Ast_Defer*> defers;
	Ast_Scope scope;
	u64 frame_size;
	Ast_ControlFlow_Flags control_flow_flags;
	// @FixMe: Should be using flags here not bools.
	bool does_return;
	bool does_break;
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
	AST_STATEMENT_ASSIGNMENT_POWER,

	AST_STATEMENT_INCREMENT,
	AST_STATEMENT_DECREMENT,

	AST_STATEMENT_RETURN,
	AST_STATEMENT_BREAK,

	AST_STATEMENT_CLAIM,

	AST_STATEMENT_BRANCH_BLOCK,
	AST_STATEMENT_DEFER,
};

enum Ast_Branch_Clause_Kind : u8
{
	AST_BRANCH_CLAUSE_INIT = 0,
	AST_BRANCH_CLAUSE_ELSE,
	AST_BRANCH_CLAUSE_THEN
};

enum Ast_Branch_Kind : u8
{
	AST_BRANCH_NAKED = 0,

	// if bool:
	AST_BRANCH_IF,

	// while bool:
	AST_BRANCH_WHILE,

	// for int:
	AST_BRANCH_FOR_COUNT,

	// for it in []T:
	// for it in []T where bool:
	// for it in []T, int:
	// for it in []T, int where bool:
	AST_BRANCH_FOR_RANGE,

	// for vardecl, bool:
	// for vardecl, bool, int:
	AST_BRANCH_FOR_VERBOSE,
};

using Ast_Branch_Index = u16;
const Ast_Branch_Index AST_BRANCH_INDEX_NONE = -1;

struct Ast_Branch
{
	Ast_Branch_Kind kind;
	Ast_Branch_Clause_Kind clause;

	Token* token;

	union
	{
		struct
		{
			Ast_VariableDeclaration* iterator;
			Ast_Expression* range;
			Ast_Expression* filter;
		};

		struct
		{
			Ast_VariableDeclaration* variable;
			Ast_Expression* condition;
		};

		Ast_Expression* count;
	};

	Ast_Expression* stride;

	Ast_Branch_Index else_branch_index;
	Ast_Branch_Index then_branch_index;

	Ast_Code code;
};

struct Ast_BranchBlock
{
	Array<Ast_Branch> branches;
	Ast_ControlFlow_Flags control_flow_flags;
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

struct Ast_VariableDeclaration
{
	Token* name;
	Type* type;
	Ast_Type* explicit_type;
	Ast_Expression* assignment;
	Ast_Attribute attribute;
	IrLogicIndex stack;
	u64 offset;
	bool is_parameter;
	bool is_pure;
	bool is_global;
	bool is_constant;
	bool is_iterator;
	bool can_constantly_evaluate;
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

struct Ast_Statement
{
	Ast_Statement_Kind kind;

	union
	{
		Ast_Assignment          assignment;
		Ast_BranchBlock         branch_block;
		Ast_Defer               defer;
		Ast_Claim               claim;
		Ast_Break               brk;
		Ast_Return              ret;
		Ast_VariableDeclaration variable_declaration; // @FixMe @Optimization: Change to pointer, Ast_VariableDeclaration is yuuuge!
		Ast_Increment           increment;
		Ast_Expression*         expression;
	};
};

struct Ast_Function
{
	String name;
	Token* name_token;
	Array<Ast_VariableDeclaration> parameters;
	Ast_Code code;
	Type* type;
	Type* return_type;
	List<Ast_Return*> returns; // @Todo: Infer return type.
	Ast_Type* ast_return_type;
	Ast_Attribute attribute;
	IrFunctionInfoIndex ir_index;
	bool is_pure;
	bool is_global;
	// @Todo: Implement 'inline' keyword.
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
	Token* name;
	Ast_Type type;
	u64 offset;
	u32 index;
	Ast_Attribute attribute;
};

struct Ast_Enum_Member
{
	Token* name;
	Ast_Expression* expression;
	u32 index;
	Ast_Attribute attribute;
};

struct Ast_Struct
{
	Token* name;
	Type type;
	Array<Ast_Struct_Member> members;
	List<Ast_Struct*> closure;
	Ast_Attribute attribute;
};

struct Ast_Enum
{
	Token* name;
	Type type;
	Type* underlying_type;
	Array<Ast_Enum_Member> members;
	Ast_Attribute attribute;
};

struct Ast_Module
{
	Stack stack;
	List<Token> tokens;
	List<Span<char>> lines;
	Span<char> code;
	String file_path;
	Array<Ast_Import> imports;
	Ast_Scope scope;
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

extern Array<Intrinsic_Function> intrinsic_functions;

void LexicalParse(String file_path, Ast_Module* module);
void InitIntrinsicFunctions(Ast_Module* module);
Ast_Module* ParseFile(String file_path);
void SemanticParse(Ast_Module* module);
void Interpret(Ast_Code* code, char* output, StackFrame* frame, Interpreter* interpreter);
void Interpret(Ast_Function* function, char* input, char* output, Interpreter* interpreter);
void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter);
StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter);
u8 GetTypePrecedence(Type* type);
Type* GetDominantType(Type* a, Type* b);
void Convert(Type* from_type, Value* from_value, Type* to_type, Value* to_value);
u64 CalculateStackFrameSize(Ast_Function* function);
u64 CalculateStackFrameSize(Ast_Code* code, u64 offset);
MemoryBlock* CreateMemoryBlock(u64 min_size, MemoryBlock* prev = null);
Interpreter* CreateInterpreter(Ast_Module* module);
void ScanExpression(Ast_Expression* expression, Ast_Scope* scope, Ast_Module* module);
void ScanScope(Ast_Scope* scope, Ast_Module* module);
void ScanCode(Ast_Code* code, Ast_Scope* scope, Ast_Function* function, Ast_Module* module);
void ScanFunction(Ast_Function* function, Ast_Scope* scope, Ast_Module* module);


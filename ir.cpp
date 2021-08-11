#include "ir.h"
#include "parser.h"
#include "print.h"
#include "assert.h"

static constexpr IrValue NoValue()
{
	return { IR_VALUE_NONE, null };
}

static constexpr IrValue Constant(s64 n, Type* type = &type_int64)
{
	IrValue v;
	v.kind = IR_VALUE_CONSTANT;
	v.type = type;
	v.value_int64 = n;
	return v;
}

static constexpr IrValue ConstantFloat64(f64 f)
{
	IrValue v;
	v.kind = IR_VALUE_CONSTANT;
	v.type = &type_float32;
	v.value_float64 = f;
	return v;
}

static constexpr IrValue ConstantFloat32(f32 f)
{
	IrValue v;
	v.kind = IR_VALUE_CONSTANT;
	v.type = &type_float32;
	v.value_float32 = f;
	return v;
}

constexpr IrValue constant_true  = Constant(1, &type_bool);
constexpr IrValue constant_false = Constant(0, &type_bool);

u32 tuple_allocation_count = 0;

static IrValue MakeTupleValue(Type* type, Array<IrValue> elements)
{
	IrValue result;
	ZeroMemory(&result);
	result.kind = IR_VALUE_TUPLE;
	result.type = type;
	result.tuple = elements;
	tuple_allocation_count++;
	return result;
}

void Release(IrValue value)
{
	// @ConsiderTheFollowing:
	//  Just keep a list of the tuple allocations and mass DeAllocate them at the end of ir generation?

	if (value.kind == IR_VALUE_TUPLE)
	{
		Assert(tuple_allocation_count);
		tuple_allocation_count--;

		for (u32 i = 0; i < value.tuple.count; i++)
		{
			Release(value.tuple[i]);
		}

		DeAllocateArray(value.tuple);
	}
}

IrValue Dependency(IrValue dependency, IrInstruction* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_NONE: break;
		case IR_VALUE_INSTRUCTION: dependency.instruction->users.Add(user); break;
		case IR_VALUE_BLOCK: dependency.block->users.Add(user); break;
		case IR_VALUE_FUNCTION: dependency.function->users.Add(user); break;
		case IR_VALUE_TUPLE: break;
		case IR_VALUE_CONSTANT: break;
		case IR_VALUE_LARGE_CONSTANT: break;
		case IR_VALUE_GLOBAL: Assert();
	}

	return dependency;
}

void RemoveDependency(IrValue dependency, IrInstruction* user)
{
	switch (dependency.kind)
	{
		case IR_VALUE_NONE: break;
		case IR_VALUE_INSTRUCTION: dependency.instruction->users.Remove(user); break;
		case IR_VALUE_BLOCK: dependency.block->users.Remove(user); break;
		case IR_VALUE_FUNCTION: dependency.function->users.Remove(user); break;
		case IR_VALUE_TUPLE: break;
		case IR_VALUE_CONSTANT: break;
		case IR_VALUE_LARGE_CONSTANT: break;
		case IR_VALUE_GLOBAL: Assert();
	}
}

IrInstruction* AllocateIrInstruction(IrBlock* block)
{
	IrInstruction* result;

	if (block->head)
	{
		result = block->head;
		block->head = result->next;
	}
	else
	{
		IrInstruction_Bucket* bucket = Allocate<IrInstruction_Bucket>();
		ZeroMemory(bucket);

		block->buckets.Add(bucket);

		for (u32 i = 1; i < IR_INSTRUCTION_BUCKET_COUNT-1; i++)
		{
			IrInstruction* instruction = bucket->instructions + i;
			instruction->next = instruction + 1;
		}

		bucket->instructions[IR_INSTRUCTION_BUCKET_COUNT-1].next = null;

		block->head = bucket->instructions + 1;

		result = bucket->instructions;
	}

	return result;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrBlock* block)
{
	IrInstruction* instruction = AllocateIrInstruction(block);
	ZeroMemory(instruction);
	instruction->kind = kind;
	instruction->type = type;
	instruction->id = type ? block->function->register_id_counter++ : -1;
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, block);
	instruction->a = Dependency(a, instruction);
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrValue b, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, a, block);
	instruction->b = Dependency(b, instruction);
	return instruction;
}

IrInstruction* PushInstruction(IrInstruction_Kind kind, Type* type, IrValue a, IrValue b, IrValue c, IrBlock* block)
{
	IrInstruction* instruction = PushInstruction(kind, type, a, b, block);
	instruction->c = Dependency(c, instruction);
	return instruction;
}

IrValue GetValue(IrInstruction* instruction)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_INSTRUCTION;
	value.instruction = instruction;
	value.type = instruction->type;
	return value;
}

IrValue GetValue(IrFunction* function)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_FUNCTION;
	value.function = function;
	value.type = function->function->type;
	return value;
}

IrValue ConvertToIR(Ast_Function* function);

IrValue GetValue(Ast_Function* function)
{
	if (function->ir)
	{
		return GetValue(function->ir);
	}

	return ConvertToIR(function);
}

IrValue GetValue(IrBlock* block)
{
	IrValue value;
	ZeroMemory(&value);
	value.kind = IR_VALUE_BLOCK;
	value.block = block;
	return value;
}

IrBlock* NewBlock(IrFunction* function)
{
	IrBlock* block = Allocate<IrBlock>();
	ZeroMemory(block);
	block->id = function->block_id_counter++; // Can't use function->blocks.count because of branch ordering nonsense.
	block->function = function;
	function->blocks.Add(block);
	return block;
}

IrValue PushLoad(IrValue address, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(IsTrivialType(address.type->subtype));
	return GetValue(PushInstruction(IR_INSTRUCTION_LOAD, address.type->subtype, address, block));
}

IrValue PushConvertToBool(IrValue v, IrBlock* block)
{
	if (v.type->kind == TYPE_BASETYPE_BOOL)
	{
		return v;
	}

	return GetValue(PushInstruction(IR_INSTRUCTION_COMPARE_NOT_EQUAL, &type_bool, v, Constant(0, v.type), block));
}

IrValue PushSelect(IrValue selector, IrValue a, IrValue b, IrBlock* block)
{
	return GetValue(PushInstruction(IR_INSTRUCTION_SELECT, a.type, selector, a, b, block));
}

IrValue PushSignExtend(IrValue v, Type* to, IrBlock* block)
{
	return GetValue(PushInstruction(IR_INSTRUCTION_SIGN_EXTEND, to, v, block));
}

IrValue PushZeroExtend(IrValue v, Type* to, IrBlock* block)
{
	return GetValue(PushInstruction(IR_INSTRUCTION_SIGN_EXTEND, to, v, block));
}

IrValue ConvertValue(IrValue v, Type* to, IrBlock* block)
{
	Type* from = v.type;

	if (from == to) return v;

	Print("ConvertValue(%, %)\n", v, to);

	if (v.kind == IR_VALUE_TUPLE)
	{
		Assert(to->kind == TYPE_BASETYPE_TUPLE);

		for (u32 i = 0; i < v.tuple.count; i++)
		{
			IrValue* element = &v.tuple[i];
			*element = ConvertValue(*element, to->tuple[i], block);
		}

		v.type = to;
		return v;
	}

	if (from->kind == TYPE_SPECIFIER_POINTER &&
		from->subtype->kind == TYPE_BASETYPE_TUPLE &&
		to->kind == TYPE_BASETYPE_TUPLE)
	{
		if (from->subtype == to) return v;

		if (v.kind == IR_VALUE_TUPLE)
		{
		}

		Assert();
	}

	if (to->kind == TYPE_BASETYPE_BOOL)
	{
		return PushConvertToBool(v, block);
	}

	if (from->kind == TYPE_BASETYPE_BOOL)
	{
		return PushSelect(v, Constant(1, to), Constant(0, to), block);
	}

	if (IsEnum(from) && IsInteger(to))
	{
		v.type = from->enumeration->underlying_type;
		v = ConvertValue(v, to, block);
		return v;
	}

	if (IsInteger(from) && IsEnum(to))
	{
		v = ConvertValue(v, to->enumeration->underlying_type, block);
		v.type = to;
		return v;
	}

	if (IsSignedInteger(from) && IsInteger(to) && from->size < to->size)
	{
		return PushSignExtend(v, to, block);
	}

	if (IsUnsignedInteger(from) && IsInteger(to) && from->size < to->size)
	{
		return PushZeroExtend(v, to, block);
	}

	if (IsInteger(from) && IsFloat(to))
	{
		return GetValue(PushInstruction(IR_INSTRUCTION_INT_TO_FLOAT, to, v, block));
	}

	if (IsFloat(from) && IsInteger(to))
	{
		return GetValue(PushInstruction(IR_INSTRUCTION_FLOAT_TO_INT, to, v, block));
	}

	if (IsFloat(from) && IsFloat(to))
	{
		return GetValue(PushInstruction(IR_INSTRUCTION_FLOAT_CONVERT, to, v, block));
	}

	Assert();
	return v;
}

void PushStore(IrValue address, IrValue value, IrBlock* block)
{
	Print("PushStore(%, %)\n", address, value);
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(address.type->subtype == value.type);

	PushInstruction(IR_INSTRUCTION_STORE, null, address, value, block);
}

void PushCopy(IrValue destination, IrValue source, IrBlock* block)
{
	Assert(destination.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(source.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(destination.type->subtype == source.type->subtype);

	PushInstruction(IR_INSTRUCTION_COPY, null, destination, source, block);
}

void PushStoreOrCopy(IrValue destination, IrValue source_or_value, IrBlock* block)
{
	Print("PushStoreOrCopy(%, %)\n", destination, source_or_value);
	if (destination.type == source_or_value.type)
	{
		PushCopy(destination, source_or_value, block);
	}
	else
	{
		PushStore(destination, source_or_value, block);
	}
}

void PushJump(IrBlock* from, IrBlock* to)
{
	Assert(!from->control);
	IrInstruction* instruction = PushInstruction(IR_INSTRUCTION_JUMP, null, from);
	instruction->a = Dependency(GetValue(to), instruction);
	from->control = instruction;
}

void PushBranch(IrBlock* from, IrValue condition, IrBlock* block_true, IrBlock* block_false)
{
	Assert(!from->control);
	IrInstruction* instruction = PushInstruction(IR_INSTRUCTION_BRANCH, null, condition, from);
	instruction->b = Dependency(GetValue(block_true), instruction);
	instruction->c = Dependency(GetValue(block_false), instruction);
	from->control = instruction;
}

void PushReturn(IrBlock* from)
{
	PushInstruction(IR_INSTRUCTION_RETURN, null, from);
}

IrValue TrimPointers(IrValue v, IrBlock* block)
{
	Assert(v.type);
	Assert(v.type->kind == TYPE_SPECIFIER_POINTER);

	while (v.type->subtype->kind == TYPE_SPECIFIER_POINTER)
	{
		v = PushLoad(v, block);
	}

	return v;
}

IrValue PushStack(Type* type, IrBlock* block)
{
	return GetValue(PushInstruction(IR_INSTRUCTION_STACK_ALLOCATE, GetPointer(type), Constant(type->size), block));
}

IrValue GetTupleMember(IrValue address, u64 index, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(address.type->subtype->kind == TYPE_BASETYPE_TUPLE);
	Assert(index < address.type->subtype->tuple.count);

	return GetValue(PushInstruction(IR_INSTRUCTION_MEMBER, GetPointer(address.type->subtype->tuple[index]), address, Constant(index), block));
}

IrValue GetStructMember(IrValue address, Ast_Struct_Member* member, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(address.type->subtype->kind == TYPE_BASETYPE_STRUCT);

	return GetValue(PushInstruction(IR_INSTRUCTION_MEMBER, GetPointer(member->type.type), address, Constant(member->index), block));
}

IrValue DeReference(IrValue address, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);

	if (IsTrivialType(address.type->subtype))
	{
		return PushLoad(address, block);
	}
	else
	{
		IrValue stack = PushStack(address.type->subtype, block);
		PushCopy(stack, address, block);
		return stack;
	}
}

IrValue ExpandTuple(IrValue address, Array<IrValue> array, IrBlock* block)
{
	Assert(address.type->kind == TYPE_SPECIFIER_POINTER);
	Assert(address.type->subtype->kind == TYPE_BASETYPE_TUPLE);

	IrValue result;
	ZeroMemory(&result);
	result.kind = IR_VALUE_TUPLE;
	result.type = address.type->subtype;
	result.tuple = array;

	for (u32 i = 0; i < address.type->subtype->tuple.count; i++)
	{
		result.tuple[i] = GetTupleMember(address, i, block);
	}

	return result;
}

void Assign(IrValue destination, IrValue value, IrBlock* block)
{
	bool destination_is_tuple = destination.kind == IR_VALUE_TUPLE;
	bool value_is_tuple = value.kind == IR_VALUE_TUPLE;


	if (destination_is_tuple || value_is_tuple)
	{
		Type* tuple_type = destination_is_tuple ? destination.type : value.type;
		u32 tuple_count = tuple_type->tuple.count;

		IrValue expanded[tuple_count];
		Array<IrValue> expanded_array = Array<IrValue>(expanded, tuple_count);

		if (destination.kind != IR_VALUE_TUPLE)
		{
			destination = ExpandTuple(destination, expanded_array, block);
		}
		else if (value.kind != IR_VALUE_TUPLE)
		{
			value = ExpandTuple(value, expanded_array, block);
		}

		for (u32 i = 0; i < tuple_count; i++)
		{
			Assign(destination.tuple[i], value.tuple[i], block);
		}
	}
	else
	{
		PushStoreOrCopy(destination, value, block);
	}
}

IrInstruction* PushPhi(IrBlock* block)
{
	return PushInstruction(IR_INSTRUCTION_PHI, &type_bool, block);
}

void AddToPhi(IrInstruction* instruction, IrPhi phi)
{
	instruction->phis.Add(phi);
	Dependency(GetValue(phi.block), instruction);
	Dependency(phi.value, instruction);
}

IrValue ConvertToIR(Ast_Expression* expression, bool force_dereference, IrBlock*& block)
{
	IrValue result = NoValue();

	switch (expression->kind)
	{
		case AST_EXPRESSION_IMPLICIT_CAST:
		{
			Ast_Expression_Implicit_Cast* cast = (Ast_Expression_Implicit_Cast*)expression;
			IrValue value = ConvertToIR(cast->subexpression, true, block);
			value = ConvertValue(value, cast->type, block);
			result = value;
		} break;

		case AST_EXPRESSION_TERMINAL_NAME:
		case AST_EXPRESSION_TERMINAL_FUNCTION:
		case AST_EXPRESSION_TERMINAL_INTRINSIC_FUNCTION:
			Assert();

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			IrValue v;
			ZeroMemory(&v);
			v.kind = IR_VALUE_CONSTANT;
			v.type = literal->type;

			switch (literal->type->kind)
			{
				case TYPE_BASETYPE_BYTE:    v.value_byte    = literal->value_byte;    break;
				case TYPE_BASETYPE_BOOL:    v.value_bool    = literal->value_bool;    break;
				case TYPE_BASETYPE_UINT8:   v.value_uint8   = literal->value_uint8;   break;
				case TYPE_BASETYPE_UINT16:  v.value_uint16  = literal->value_uint16;  break;
				case TYPE_BASETYPE_UINT32:  v.value_uint32  = literal->value_uint32;  break;
				case TYPE_BASETYPE_UINT64:  v.value_uint64  = literal->value_uint64;  break;
				case TYPE_BASETYPE_INT8:    v.value_int8    = literal->value_int8;    break;
				case TYPE_BASETYPE_INT16:   v.value_int16   = literal->value_int16;   break;
				case TYPE_BASETYPE_INT32:   v.value_int32   = literal->value_int32;   break;
				case TYPE_BASETYPE_INT64:   v.value_int64   = literal->value_int64;   break;
				// case TYPE_BASETYPE_FLOAT16: v.value_float16 = literal->value_float16; break;
				case TYPE_BASETYPE_FLOAT32: v.value_float32 = literal->value_float32; break;
				case TYPE_BASETYPE_FLOAT64: v.value_float64 = literal->value_float64; break;
				default: Assert();
			}

			result = v;
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_VariableDeclaration* var = ((Ast_Expression_Variable*)expression)->variable;
			result = var->address;
		} break;

		case AST_EXPRESSION_TERMINAL_STRUCT:
		case AST_EXPRESSION_TERMINAL_STRUCT_MEMBER:
		case AST_EXPRESSION_TERMINAL_ENUM:
		case AST_EXPRESSION_TERMINAL_ENUM_MEMBER:
		case AST_EXPRESSION_TERMINAL_PRIMITIVE:
			Assert();

		case AST_EXPRESSION_TERMINAL_ARRAY_DATA:
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH:
			Assert();

		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Assert();
		}

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrValue value = ConvertToIR(unary->subexpression, true, block);
			IrValue zero = Constant(0);

			if (unary->type->kind == TYPE_BASETYPE_FLOAT64)
			{
				zero = ConstantFloat64(0);
			}
			else if (unary->type->kind == TYPE_BASETYPE_FLOAT32)
			{
				zero = ConstantFloat32(0);
			}

			result = GetValue(PushInstruction(IR_INSTRUCTION_SUBTRACT, unary->type, zero, value, block));
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		case AST_EXPRESSION_UNARY_NOT:
		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			IrValue value = ConvertToIR(unary->subexpression, true, block);

			IrInstruction_Kind kind;
			switch (expression->kind)
			{
				case AST_EXPRESSION_UNARY_BITWISE_NOT: kind = IR_INSTRUCTION_BITWISE_NOT; break;
				case AST_EXPRESSION_UNARY_NOT:         kind = IR_INSTRUCTION_NOT;         break;
				case AST_EXPRESSION_UNARY_PLUS:        kind = IR_INSTRUCTION_POSITIVE;    break;
				default: Unreachable();
			}

			result = GetValue(PushInstruction(kind, expression->type, value, block));
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			result = ConvertToIR(unary->subexpression, true, block);
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			result = ConvertToIR(unary->subexpression, false, block);
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left = ConvertToIR(binary->left, true, block);
			IrValue right = ConvertToIR(binary->right, true, block);

			IrInstruction_Kind kind;

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);
			left  = ConvertValue(left,  dominant, block);
			right = ConvertValue(right, dominant, block);

			switch (expression->kind)
			{
				case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            kind = IR_INSTRUCTION_COMPARE_EQUAL;            break;
				case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        kind = IR_INSTRUCTION_COMPARE_NOT_EQUAL;        break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS:             kind = IR_INSTRUCTION_COMPARE_LESS;             break;
				case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    kind = IR_INSTRUCTION_COMPARE_LESS_OR_EQUAL;    break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER:          kind = IR_INSTRUCTION_COMPARE_GREATER;          break;
				case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: kind = IR_INSTRUCTION_COMPARE_GREATER_OR_EQUAL; break;
				default: Unreachable();
			}

			result = GetValue(PushInstruction(kind, &type_bool, left, right, block));
		} break;

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
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left = ConvertToIR(binary->left, true, block);
			IrValue right = ConvertToIR(binary->right, true, block);

			IrInstruction_Kind kind;

			switch (expression->kind)
			{
				case AST_EXPRESSION_BINARY_ADD:
				{
					if (binary->left->type->kind == TYPE_SPECIFIER_POINTER && IsInteger(binary->right->type))
					{
						right = ConvertValue(right, &type_int64, block);
						kind = IR_INSTRUCTION_ELEMENT;
					}
					else
					{
						kind = IR_INSTRUCTION_ADD;
					}
				} break;

				case AST_EXPRESSION_BINARY_SUBTRACT:
				{
					if (binary->left->type->kind == TYPE_SPECIFIER_POINTER && binary->right->type->kind == TYPE_SPECIFIER_POINTER)
					{
						left = GetValue(PushInstruction(IR_INSTRUCTION_SUBTRACT, &type_int64, left, right, block));
						right = Constant(left.type->size);
						kind = IR_INSTRUCTION_DIVIDE;
					}
					else if (binary->left->type->kind == TYPE_SPECIFIER_POINTER && IsConvertableTo(binary->right->type, &type_int64))
					{
						right = ConvertValue(right, &type_int64, block);
						right = GetValue(PushInstruction(IR_INSTRUCTION_SUBTRACT, &type_int64, Constant(0), right, block));
						kind = IR_INSTRUCTION_ELEMENT;
					}
					else
					{
						kind = IR_INSTRUCTION_SUBTRACT;
					}
				} break;

				case AST_EXPRESSION_BINARY_MULTIPLY:    kind = IR_INSTRUCTION_MULTIPLY;            break;
				case AST_EXPRESSION_BINARY_DIVIDE:      kind = IR_INSTRUCTION_DIVIDE;              break;
				case AST_EXPRESSION_BINARY_MODULO:      kind = IR_INSTRUCTION_MODULO;              break;
				case AST_EXPRESSION_BINARY_EXPONENTIAL: kind = IR_INSTRUCTION_EXPONENTIAL;         break;
				case AST_EXPRESSION_BINARY_BITWISE_OR:  kind = IR_INSTRUCTION_BITWISE_OR;          break;
				case AST_EXPRESSION_BINARY_BITWISE_XOR: kind = IR_INSTRUCTION_BITWISE_XOR;         break;
				case AST_EXPRESSION_BINARY_BITWISE_AND: kind = IR_INSTRUCTION_BITWISE_AND;         break;
				case AST_EXPRESSION_BINARY_LEFT_SHIFT:  kind = IR_INSTRUCTION_BITWISE_LEFT_SHIFT;  break;
				case AST_EXPRESSION_BINARY_RIGHT_SHIFT: kind = IR_INSTRUCTION_BITWISE_RIGHT_SHIFT; break;

				default: Unreachable();
			}

			result = GetValue(PushInstruction(kind, expression->type, left, right, block));
		} break;

		case AST_EXPRESSION_BINARY_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;
			IrValue left = PushConvertToBool(ConvertToIR(binary->left, true, block), block);

			IrBlock* true_branch = NewBlock(block->function);
			IrBlock* exit_branch = NewBlock(block->function);

			PushBranch(block, left, true_branch, exit_branch);

			IrValue right = PushConvertToBool(ConvertToIR(binary->right, true, true_branch), true_branch);

			PushJump(true_branch, exit_branch);

			IrInstruction* phi = PushPhi(exit_branch);
			AddToPhi(phi, { block, constant_false });
			AddToPhi(phi, { true_branch, right });

			block = exit_branch;
			result = GetValue(phi);
		} break;

		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			IrValue left = PushConvertToBool(ConvertToIR(binary->left, true, block), block);

			IrBlock* exit_branch = NewBlock(block->function);
			IrBlock* false_branch = NewBlock(block->function);

			PushBranch(block, left, exit_branch, false_branch);

			IrValue right = PushConvertToBool(ConvertToIR(binary->right, true, false_branch), false_branch);

			PushJump(false_branch, exit_branch);

			IrInstruction* phi = PushPhi(exit_branch);
			AddToPhi(phi, { block, constant_true });
			AddToPhi(phi, { false_branch, right });

			block = exit_branch;

			result = GetValue(phi);
		} break;

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			if (binary->left->kind == AST_EXPRESSION_TERMINAL_ENUM)
			{
				Ast_Enum_Member* member = ((Ast_Expression_Enum_Member*)binary->right)->member;
				Assert(member->expression->type);
				result = ConvertToIR(member->expression, false, block); // @Todo @FixMe: This should be a constant.
			}
			else
			{
				IrValue left = TrimPointers(ConvertToIR(binary->left, false, block), block);
				Ast_Struct_Member* member = ((Ast_Expression_Struct_Member*)binary->right)->member;

				result = GetStructMember(left, member, block);
			}
		} break;

		case AST_EXPRESSION_DOT_CALL:
		{
			Ast_Expression_Dot_Call* call = (Ast_Expression_Dot_Call*)expression;
			IrValue params = ConvertToIR(call->parameters, true, block);
			Assert(call->dot->right->kind == AST_EXPRESSION_TERMINAL_FUNCTION);
			PushInstruction(IR_INSTRUCTION_CALL, null, GetValue(((Ast_Expression_Function*)call->dot->right)->function), block);
		} break;

		case AST_EXPRESSION_CALL:
		{
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;
			Assert(call->function->kind == AST_EXPRESSION_TERMINAL_FUNCTION);
			IrValue params = ConvertToIR(call->parameters, true, block);
			PushInstruction(IR_INSTRUCTION_CALL, null, GetValue(((Ast_Expression_Function*)call->function)->function), block);
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;

			IrValue array = ConvertToIR(subscript->array, true, block);
			IrValue index = ConvertToIR(subscript->index, true, block);

			result = GetValue(PushInstruction(IR_INSTRUCTION_ELEMENT, GetPointer(subscript->type), array, index, block));
		} break;

		case AST_EXPRESSION_IF_ELSE:
		{
			Ast_Expression_Ternary* ternary = (Ast_Expression_Ternary*)expression;

			IrValue stack = PushStack(ternary->type, block);

			IrValue condition = PushConvertToBool(ConvertToIR(ternary->middle, true, block), block);

			IrBlock* left_branch  = NewBlock(block->function);
			IrBlock* right_branch = NewBlock(block->function);

			PushBranch(block, condition, left_branch, right_branch);

			IrValue left_value  = ConvertToIR(ternary->left,  false, left_branch);
			IrValue right_value = ConvertToIR(ternary->right, false, right_branch);

			Assign(stack, left_value, left_branch);
			Assign(stack, right_value, right_branch);

			Release(left_value);
			Release(right_value);

			IrBlock* exit_branch = NewBlock(block->function);

			PushJump(left_branch, exit_branch);
			PushJump(right_branch, exit_branch);

			block = exit_branch;

			result = stack;
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			if (tuple->elements.count == 0)
			{
				result = NoValue();
				break;
			}

			if (tuple->elements.count == 1)
			{
				result = ConvertToIR(tuple->elements[0], force_dereference, block);
				break;
			}

			IrValue value;
			ZeroMemory(&value);
			value.kind = IR_VALUE_TUPLE;
			value.type = tuple->type;
			value.tuple = AllocateArray<IrValue>(tuple->elements.count);
			tuple_allocation_count++;

			for (u32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				value.tuple[i] = ConvertToIR(element, force_dereference, block);
			}

			result = value;
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			IrValue v = ConvertToIR(as->expression, true, block);

			v = ConvertValue(v, as->type, block);

			result = v;
		} break;

		case AST_EXPRESSION_LAMBDA:
			Assert();
	}

	Assert(result.kind != IR_VALUE_NONE || expression->type == &empty_tuple || expression->kind == AST_EXPRESSION_CALL || expression->kind == AST_EXPRESSION_DOT_CALL);

	if (expression->is_referential_value && force_dereference)
	{
		result = DeReference(result, block);
	}

	return result;
}

IrBlock* ConvertToIR(Ast_Code* code, IrBlock* block, IrBlock* exit_block, IrBlock* break_block, IrFunction* function)
{
	if (!block)
	{
		block = NewBlock(function);
	}

	IrBlock* initial_block = block;

	for (Ast_Statement* statement = code->statements; statement < code->statements.End(); statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* branch_block = &statement->branch_block;

				for (Ast_Branch* branch = branch_block->branches; branch < branch_block->branches.End(); branch++)
				{
					branch->initial_condition_block = NewBlock(function);
				}

				PushJump(block, branch_block->branches[0].initial_condition_block);

				IrBlock* exit_block = NewBlock(function);

				for (Ast_Branch* branch = branch_block->branches; branch < branch_block->branches.End(); branch++)
				{
					IrBlock* else_branch = exit_block;
					IrBlock* then_branch = exit_block;

					if (branch->else_branch)
					{
						else_branch = branch->else_branch->initial_condition_block;
					}

					if (branch->then_branch)
					{
						then_branch = branch->then_branch->initial_condition_block;
					}

					if (branch->condition)
					{
						if (branch->token->kind == TOKEN_WHILE)
						{
							IrBlock* loop_block = NewBlock(function);
							IrBlock* inner_block = ConvertToIR(&branch->code, null, loop_block, then_branch, function);

							{
								IrValue condition = ConvertToIR(branch->condition, true, branch->initial_condition_block);
								IrValue test = PushConvertToBool(condition, branch->initial_condition_block);

								PushBranch(branch->initial_condition_block, test, inner_block, else_branch);
							}

							{
								IrValue condition = ConvertToIR(branch->condition, true, loop_block);
								IrValue test = PushConvertToBool(condition, loop_block);

								PushBranch(loop_block, test, inner_block, then_branch);
							}
						}
						else
						{
							Assert(branch->token->kind == TOKEN_IF);

							IrBlock* inner_block = ConvertToIR(&branch->code, null, then_branch, break_block, function);
							IrValue condition = ConvertToIR(branch->condition, true, branch->initial_condition_block);
							IrValue test = PushConvertToBool(condition, branch->initial_condition_block);

							PushBranch(branch->initial_condition_block, test, inner_block, else_branch);
						}
					}
					else
					{
						ConvertToIR(&branch->code, branch->initial_condition_block, exit_block, break_block, function);
					}
				}

				block = exit_block;
			} break;

			case AST_STATEMENT_DEFER:
			{
				Assert();
				ConvertToIR(&statement->defer.code, null, exit_block, break_block, function);
			} break;

			case AST_STATEMENT_CLAIM:
			{
				Assert();
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Return* ret = &statement->ret;

				if (ret->expression)
				{
					IrValue v = ConvertToIR(ret->expression, true, block);
				}
				// @Todo: PushStore return value or something...
				PushReturn(block);
				return initial_block;
			} break;

			case AST_STATEMENT_BREAK:
			{
				Assert(break_block);
				PushJump(block, break_block);
				return initial_block;
			} break;

			case AST_STATEMENT_INCREMENT:
			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				bool direction = statement->kind == AST_STATEMENT_INCREMENT;

				IrValue address = ConvertToIR(inc->expression, false, block);
				IrValue value = PushLoad(address, block);

				IrInstruction_Kind kind = IR_INSTRUCTION_ADD;
				IrValue one = Constant(direction ? 1 : -1);

				if (inc->expression->type->kind == TYPE_SPECIFIER_POINTER)
				{
					kind = IR_INSTRUCTION_ELEMENT;
				}
				else if (inc->expression->type->kind == TYPE_BASETYPE_FLOAT64)
				{
					one = ConstantFloat64(direction ? 1 : -1);
				}
				else if (inc->expression->type->kind == TYPE_BASETYPE_FLOAT32)
				{
					one = ConstantFloat32(direction ? 1 : -1);
				}

				value = GetValue(PushInstruction(IR_INSTRUCTION_ADD, inc->expression->type, value, one, block));
				PushStore(address, value, block);
			} break;

			case AST_STATEMENT_EXPRESSION:
			{
				Release(ConvertToIR(statement->expression, false, block));
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_VariableDeclaration* var = &statement->variable_declaration;

				IrValue address = GetValue(PushInstruction(IR_INSTRUCTION_STACK_ALLOCATE, GetPointer(var->type), Constant(var->type->size), block));
				var->address = address;

				if (var->assignment)
				{
					IrValue v = ConvertToIR(var->assignment, false, block);
					PushStoreOrCopy(address, v, block);
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;

				IrValue left = ConvertToIR(assignment->left, false, block);
				IrValue right = ConvertToIR(assignment->right, true, block);

				right = ConvertValue(right, assignment->left->type, block);

				Assign(left, right, block);

				Release(left);
				Release(right);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_POWER:
			{
				Ast_Assignment* assignment = &statement->assignment;
				IrInstruction_Kind kind;

				IrValue address = ConvertToIR(assignment->left, false, block);
				IrValue left = DeReference(address, block);
				IrValue right = ConvertToIR(assignment->right, true, block);

				if (assignment->left->type->kind == TYPE_SPECIFIER_POINTER && IsInteger(assignment->right->type))
				{
					if (statement->kind == AST_STATEMENT_ASSIGNMENT_SUBTRACT)
					{
						right = GetValue(PushInstruction(IR_INSTRUCTION_SUBTRACT, right.type, Constant(0), right, block));
					}

					kind = IR_INSTRUCTION_ELEMENT;
				}
				else switch (statement->kind)
				{
					case AST_STATEMENT_ASSIGNMENT_ADD:      kind = IR_INSTRUCTION_ADD;         break;
					case AST_STATEMENT_ASSIGNMENT_SUBTRACT: kind = IR_INSTRUCTION_SUBTRACT;    break;
					case AST_STATEMENT_ASSIGNMENT_MULTIPLY: kind = IR_INSTRUCTION_MULTIPLY;    break;
					case AST_STATEMENT_ASSIGNMENT_DIVIDE:   kind = IR_INSTRUCTION_DIVIDE;      break;
					case AST_STATEMENT_ASSIGNMENT_POWER:    kind = IR_INSTRUCTION_EXPONENTIAL; break;
					default: Unreachable();
				}

				IrValue value = GetValue(PushInstruction(kind, left.type, left, right, block));
				PushStore(address, value, block);
			} break;
		}
	}

	if (exit_block)
	{
		PushJump(block, exit_block);
	}
	else
	{
		PushReturn(block);
	}

	return initial_block;
}

IrValue ConvertToIR(Ast_Function* function)
{
	if (function->ir)
	{
		return GetValue(function->ir);
	}

	IrFunction* ir = Allocate<IrFunction>();
	ZeroMemory(ir);
	ir->function = function;
	function->ir = ir;

	IrBlock* entry_block = NewBlock(ir);

	for (u32 i = 0; i < function->parameters.count; i++)
	{
		Ast_VariableDeclaration* param = &function->parameters[i];

		if (IsTrivialType(param->type))
		{
			param->address = GetValue(PushInstruction(IR_INSTRUCTION_STACK_ALLOCATE, GetPointer(param->type), Constant(param->type->size), entry_block));
			IrValue v = GetValue(PushInstruction(IR_INSTRUCTION_PARAMETER, param->type, Constant(i), entry_block));
			PushStore(param->address, v, entry_block);
		}
		else
		{
			param->address = GetValue(PushInstruction(IR_INSTRUCTION_PARAMETER, GetPointer(param->type), Constant(i), entry_block));
		}
	}

	ConvertToIR(&function->code, entry_block, null, null, ir);

	Print("Unherded cats = %\n", tuple_allocation_count);
	Print("%\n", ir);

	standard_output_buffer.Flush();

	return GetValue(function->ir);
}

void ConvertToIR(Ast_Root* root)
{
	for (Ast_Function* function = root->scope.functions; function < root->scope.functions.End(); function++)
	{
		ConvertToIR(function);
	}

	Print("sizeof(IrInstruction_Bucket) = %\n", sizeof(IrInstruction_Bucket));
	Print("sizeof(IrInstruction) = %\n", sizeof(IrInstruction)); // @FixMe: Fuuuuuuuuuuuuuuuuuuck

	Assert(tuple_allocation_count == 0, "Cats weren't herded currectly.\n");
}


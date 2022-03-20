#include "interpreter.h"
#include "parser.h"
#include "print.h"
#include "assert.h"
#include "general.h"
#include "memory.h"

template<typename T>
static T ConvertNumerical(Value* value, Type_Kind type)
{
	switch (type)
	{
		case TYPE_BASETYPE_BYTE: return (uint8)value->i8;
		case TYPE_BASETYPE_BOOL: return (bool)value->i8;

		case TYPE_BASETYPE_INT8:  return (int8)value->i8;
		case TYPE_BASETYPE_INT16: return (int16)value->i16;
		case TYPE_BASETYPE_INT32: return (int32)value->i32;
		case TYPE_BASETYPE_INT64: return (int64)value->i64;

		case TYPE_BASETYPE_UINT8:  return (uint8)value->i8;
		case TYPE_BASETYPE_UINT16: return (uint16)value->i16;
		case TYPE_BASETYPE_UINT32: return (uint32)value->i32;
		case TYPE_BASETYPE_UINT64: return (uint64)value->i64;

		case TYPE_BASETYPE_ENUM:     return value->i64;
		case TYPE_SPECIFIER_POINTER: return value->i64;

		case TYPE_BASETYPE_FLOAT16: return value->f16;
		case TYPE_BASETYPE_FLOAT32: return value->f32;
		case TYPE_BASETYPE_FLOAT64: return value->f64;

		default:
			Assert();
			Unreachable();
	}
}

void Convert(Type* from_type, Value* from_value, Type* to_type, Value* to_value)
{
	if (from_type == to_type)
	{
		CopyMemory(to_value->data, from_value->data, from_type->size);
	}
	else switch (to_type->kind)
	{
		case TYPE_BASETYPE_BYTE: to_value->i8 = ConvertNumerical<uint64>(from_value, from_type->kind);  break;
		case TYPE_BASETYPE_BOOL: to_value->i8 = ConvertNumerical<bool>(from_value, from_type->kind); break;

		case TYPE_BASETYPE_INT8:  to_value->i8  = ConvertNumerical<int64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_INT16: to_value->i16 = ConvertNumerical<int64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_INT32: to_value->i32 = ConvertNumerical<int64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_INT64: to_value->i64 = ConvertNumerical<int64>(from_value, from_type->kind); break;

		case TYPE_BASETYPE_UINT8:  to_value->i8  = ConvertNumerical<uint64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_UINT16: to_value->i16 = ConvertNumerical<uint64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_UINT32: to_value->i32 = ConvertNumerical<uint64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_UINT64: to_value->i64 = ConvertNumerical<uint64>(from_value, from_type->kind); break;

		case TYPE_BASETYPE_ENUM:     to_value->i64 = ConvertNumerical<uint64>(from_value, from_type->kind); break; // @FixMe
		case TYPE_SPECIFIER_POINTER: to_value->i64 = ConvertNumerical<uint64>(from_value, from_type->kind); break; // @FixMe

		case TYPE_BASETYPE_FLOAT16: to_value->f32 = ConvertNumerical<float64>(from_value, from_type->kind); break; // @Bug
		case TYPE_BASETYPE_FLOAT32: to_value->f32 = ConvertNumerical<float64>(from_value, from_type->kind); break;
		case TYPE_BASETYPE_FLOAT64: to_value->f64 = ConvertNumerical<float64>(from_value, from_type->kind); break;

		case TYPE_SPECIFIER_FIXED_ARRAY:
		{
			Assert(from_type->kind == TYPE_SPECIFIER_FIXED_ARRAY);
			Assert(from_type->length == to_type->length);

			for (uint32 i = 0; i < to_type->length; i++)
			{
				Value* from = (Value*)(from_value->data + from_type->subtype->size * i);
				Value* to   = (Value*)(to_value->data + to_type->subtype->size * i);
				Convert(from_type->subtype, from, to_type->subtype, to);
			}
		} break;

		case TYPE_BASETYPE_TUPLE:
		{
			Assert(from_type->kind == TYPE_BASETYPE_TUPLE);
			Assert(from_type->tuple.count == to_type->tuple.count);

			char* from_element = from_value->data;
			char* to_element = to_value->data;

			for (uint32 i = 0; i < from_type->tuple.count; i++)
			{
				Convert(from_type->tuple[i], (Value*)from_element, to_type->tuple[i], (Value*)to_element);

				from_element += from_type->tuple[i]->size;
				to_element += to_type->tuple[i]->size;
			}

		} break;

		default: Assert();
	}
}

void Interpret(Ast_Expression* expression, char* output, bool allow_referential, StackFrame* frame, Interpreter* interpreter)
{
	switch (expression->kind)
	{
		case AST_EXPRESSION_TERMINAL_ARRAY_BEGIN:  Assert();
		case AST_EXPRESSION_TERMINAL_ARRAY_END:    Assert();
		case AST_EXPRESSION_TERMINAL_ARRAY_LENGTH: Assert();

		case AST_EXPRESSION_BINARY_DOT:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			if (binary->right->kind == AST_EXPRESSION_TERMINAL_STRUCT_MEMBER)
			{
				Ast_Expression_Struct_Member* struct_member = (Ast_Expression_Struct_Member*)binary->right;

				if ((binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL) || binary->left->type->kind == TYPE_SPECIFIER_POINTER)
				{
					char* ref;
					Interpret(binary->left, (char*)&ref, true, frame, interpreter);

					Type* type = binary->left->type;

					if ((binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL) && type->kind == TYPE_SPECIFIER_POINTER)
					{
						ref = *(char**)ref;
					}

					while (type->kind == TYPE_SPECIFIER_POINTER && type->subtype->kind == TYPE_SPECIFIER_POINTER)
					{
						type = type->subtype;
						ref = *(char**)ref;
					}

					ref += struct_member->member->offset;

					if (allow_referential)
					{
						*(char**)output = ref;
					}
					else
					{
						CopyMemory(output, ref, binary->right->type->size);
					}
				}
				else
				{
					Assert(!allow_referential);

					char data[binary->left->type->size];
					Interpret(binary->left, data, false, frame, interpreter);

					CopyMemory(output, data + struct_member->member->offset, binary->right->type->size);
				}
			}
			else if (binary->right->kind == AST_EXPRESSION_TERMINAL_ENUM_MEMBER)
			{
				Ast_Expression_Enum* enum_expression = (Ast_Expression_Enum*)binary->left;
				Ast_Expression_Enum_Member* enum_member_expression = (Ast_Expression_Enum_Member*)binary->right;

				// @Performance @FixMe: This should be pre-computed...
				Ast_Expression* value_expression = enum_member_expression->member->expression;
				char data[value_expression->type->size];
				Interpret(value_expression, data, false, frame, interpreter);
				Convert(value_expression->type, (Value*)data, binary->type, (Value*)output);
			}
			else if (binary->right->kind == AST_EXPRESSION_TERMINAL_ARRAY_BEGIN)
			{
				Assert(binary->left->type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY);

				if (allow_referential)
				{
					Assert((binary->flags & AST_EXPRESSION_FLAG_REFERENTIAL));
					Assert((binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL));
					Array_Value* dynamic_array_ref;
					Interpret(binary->left, (char*)&dynamic_array_ref, true, frame, interpreter);
					*(char**)output = (char*)&dynamic_array_ref->address;
				}
				else
				{
					Array_Value array;
					Interpret(binary->left, (char*)&array, false, frame, interpreter);
					*(char**)output = array.address;
				}
			}
			else if (binary->right->kind == AST_EXPRESSION_TERMINAL_ARRAY_LENGTH)
			{
				if (binary->left->type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
				{
					*(uint64*)output = binary->left->type->length;
				}
				else
				{
					Assert(binary->left->type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY);

					if (allow_referential)
					{
						Assert((binary->flags & AST_EXPRESSION_FLAG_REFERENTIAL));
						Assert((binary->left->flags & AST_EXPRESSION_FLAG_REFERENTIAL));
						Array_Value* dynamic_array_ref;
						Interpret(binary->left, (char*)&dynamic_array_ref, true, frame, interpreter);
						*(char**)output = (char*)&dynamic_array_ref->length;
					}
					else
					{
						Array_Value array;
						Interpret(binary->left, (char*)&array, false, frame, interpreter);
						*(uint64*)output = array.length;
					}
				}
			}
			else
			{
				Assert();
			}
		} break;

		case AST_EXPRESSION_AS:
		{
			Ast_Expression_As* as = (Ast_Expression_As*)expression;
			// Interpret(as->expression, as->
			Assert();
		} break;

		case AST_EXPRESSION_SUBSCRIPT:
		{
			Ast_Expression_Subscript* subscript = (Ast_Expression_Subscript*)expression;
			uint64 subsize = subscript->array->type->subtype->size;

			if (subscript->array->type->kind == TYPE_SPECIFIER_POINTER)
			{
				char* p;
				Interpret(subscript->array, (char*)&p, false, frame, interpreter);

				char index_data[subscript->index->type->size];
				Interpret(subscript->index, index_data, false, frame, interpreter);
				uint64 index = ConvertNumerical<uint64>((Value*)index_data, subscript->index->type->kind);

				p += index * subsize;

				if (allow_referential)
				{
					*(char**)output = p;
				}
				else
				{
					CopyMemory(output, p, subsize);
				}
			}
			else if (subscript->array->type->kind == TYPE_SPECIFIER_DYNAMIC_ARRAY)
			{
				Array_Value array;
				Interpret(subscript->array, (char*)&array, false, frame, interpreter);

				char index_data[subscript->index->type->size];
				Interpret(subscript->index, index_data, false, frame, interpreter);
				uint64 index = ConvertNumerical<uint64>((Value*)index_data, subscript->index->type->kind);

				char* p = array.address;

				Assert(index < array.length);

				p += index * subsize;

				if (allow_referential)
				{
					*(char**)output = p;
				}
				else
				{
					CopyMemory(output, p, subsize);
				}
			}
			else if (subscript->array->type->kind == TYPE_SPECIFIER_FIXED_ARRAY)
			{
				if ((subscript->array->flags & AST_EXPRESSION_FLAG_REFERENTIAL))
				{
					char* p;
					Interpret(subscript->array, (char*)&p, true, frame, interpreter);

					char index_data[subscript->index->type->size];
					Interpret(subscript->index, index_data, false, frame, interpreter);
					uint64 index = ConvertNumerical<uint64>((Value*)index_data, subscript->index->type->kind);

					p += index * subsize;

					if (allow_referential)
					{
						*(char**)output = p;
					}
					else
					{
						CopyMemory(output, p, subsize);
					}
				}
				else
				{
					Assert(!allow_referential);

					char array[subscript->array->type->size];
					Interpret(subscript->array, array, false, frame, interpreter);

					char index_data[subscript->index->type->size];
					Interpret(subscript->index, index_data, false, frame, interpreter);
					uint64 index = ConvertNumerical<uint64>((Value*)index_data, subscript->index->type->kind);

					CopyMemory(output, array + index * subsize, subsize);
				}
			}
			else Assert();
		} break;

		case AST_EXPRESSION_FIXED_ARRAY:
		{
			Ast_Expression_Fixed_Array* fixed_array = (Ast_Expression_Fixed_Array*)expression;
			Type* subtype = fixed_array->type->subtype;

			for (uint32 i = 0; i < fixed_array->elements.count; i++)
			{
				Ast_Expression* element = fixed_array->elements[i];
				char element_data[element->type->size];
				Interpret(element, element_data, false, frame, interpreter);
				Convert(element->type, (Value*)element_data, subtype, (Value*)(output + subtype->size * i));
			}
		} break;

		case AST_EXPRESSION_TERMINAL_VARIABLE:
		{
			Ast_Expression_Variable* variable = (Ast_Expression_Variable*)expression;

			if (allow_referential)
			{
				*(char**)output = StackFrameGetVariable(frame, variable->variable);
			}
			else
			{
				CopyMemory(output, StackFrameGetVariable(frame, variable->variable), expression->type->size);
			}
		} break;

		case AST_EXPRESSION_BINARY_AND:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			char left[binary->left->type->size];
			Interpret(binary->left, left, false, frame, interpreter);
			bool l = ConvertNumerical<bool>((Value*)left, binary->left->type->kind);

			if (!l)
			{
				*(bool*)output = false;
				break;
			}

			char right[binary->right->type->size];
			Interpret(binary->right, right, false, frame, interpreter);
			bool r = ConvertNumerical<bool>((Value*)right, binary->right->type->kind);

			*(bool*)output = r;

			Print("% % % = %\n", l, binary->op, r, *(bool*)output);
		} break;

		case AST_EXPRESSION_BINARY_OR:
		{
			Ast_Expression_Binary* binary = (Ast_Expression_Binary*)expression;

			char left[binary->left->type->size];
			Interpret(binary->left, left, false, frame, interpreter);
			bool l = ConvertNumerical<bool>((Value*)left, binary->left->type->kind);

			if (l)
			{
				*(bool*)output = true;
				break;
			}

			char right[binary->right->type->size];
			Interpret(binary->right, right, false, frame, interpreter);
			bool r = ConvertNumerical<bool>((Value*)right, binary->right->type->kind);

			*(bool*)output = r;

			Print("% % % = %\n", l, binary->op, r, *(bool*)output);
		} break;

		case AST_EXPRESSION_BINARY_COMPARE_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_LESS:
		case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER:
		case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL:
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

			char left[binary->left->type->size];
			char right[binary->right->type->size];

			Interpret(binary->left,  left,  false, frame, interpreter);
			Interpret(binary->right, right, false, frame, interpreter);

			Type* dominant = GetDominantType(binary->left->type, binary->right->type);

			if (IsPointer(dominant) && (IsInteger(binary->left->type) || IsInteger(binary->right->type))
				&& (binary->kind == AST_EXPRESSION_BINARY_ADD || binary->kind == AST_EXPRESSION_BINARY_SUBTRACT))
			{
				uint64 l = ConvertNumerical<uint64>((Value*)left,  binary->left->type->kind);
				uint64 r = ConvertNumerical<uint64>((Value*)right, binary->right->type->kind);

				if (IsInteger(binary->left->type))
				{
					l *= dominant->subtype->size;
				}
				else
				{
					r *= dominant->subtype->size;
				}

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_ADD:      *(uint64*)output = l + r; break;
					case AST_EXPRESSION_BINARY_SUBTRACT: *(uint64*)output = l - r; break;

					default:
						Assert();
						Unreachable();
				}

				Print("% % % = %\n", l, binary->op, r, *(uint64*)output);
			}
			else if (IsPointer(binary->left->type) && IsPointer(binary->right->type) && binary->kind == AST_EXPRESSION_BINARY_SUBTRACT)
			{
				uint64 l = *(uint64*)left;
				uint64 r = *(uint64*)right;
				Assert(binary->left->type == binary->right->type);

				*(int64*)output = (l - r) / binary->left->type->size;

				Print("% % % = %\n", l, binary->op, r, *(int64*)output);
			}
			else if (IsFloat(dominant))
			{
				float64 l = ConvertNumerical<float64>((Value*)left,  binary->left->type->kind);
				float64 r = ConvertNumerical<float64>((Value*)right, binary->right->type->kind);
				float64 f;

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_LESS:             *(bool*)output = l <  r; break;
					case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    *(bool*)output = l <= r; break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER:          *(bool*)output = l >  r; break;
					case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: *(bool*)output = l >= r; break;
					case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            *(bool*)output = l == r; break;
					case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        *(bool*)output = l != r; break;
					case AST_EXPRESSION_BINARY_ADD:         f = l +  r; break;
					case AST_EXPRESSION_BINARY_SUBTRACT:    f = l -  r; break;
					case AST_EXPRESSION_BINARY_MULTIPLY:    f = l *  r; break;
					case AST_EXPRESSION_BINARY_DIVIDE:      f = l /  r; break;
					case AST_EXPRESSION_BINARY_EXPONENTIAL: f = Pow(l, r); break;
					default: Assert();
				}


				if (binary->type == &type_bool)
				{
					Print("% % % = %\n", l, binary->op, r, *(bool*)output);
				}
				else
				{
					Print("% % % = %\n", l, binary->op, r, f);
				}

				if (binary->type != &type_bool)
				{
					Convert(&type_float64, (Value*)&f, binary->type, (Value*)output);
				}
			}
			else if (IsInteger(dominant) || IsPointer(dominant))
			{
				bool is_signed = IsSignedInteger(dominant);

				if (is_signed && binary->kind != AST_EXPRESSION_BINARY_LEFT_SHIFT && binary->kind != AST_EXPRESSION_BINARY_RIGHT_SHIFT)
				{
					int64 l = ConvertNumerical<int64>((Value*)left,  binary->left->type->kind);
					int64 r = ConvertNumerical<int64>((Value*)right, binary->right->type->kind);
					int64 n;

					switch (binary->kind)
					{
						case AST_EXPRESSION_BINARY_COMPARE_LESS:             n = l <  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    n = l <= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER:          n = l >  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: n = l >= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = l == r; break;
						case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = l != r; break;
						case AST_EXPRESSION_BINARY_ADD:         n = l +  r; break;
						case AST_EXPRESSION_BINARY_SUBTRACT:    n = l -  r; break;
						case AST_EXPRESSION_BINARY_MULTIPLY:    n = l *  r; break;
						case AST_EXPRESSION_BINARY_DIVIDE:      n = l /  r; break;
						case AST_EXPRESSION_BINARY_MODULO:      n = l %  r; break;
						case AST_EXPRESSION_BINARY_EXPONENTIAL: n = Pow(l, r) + 0.5; break;
						case AST_EXPRESSION_BINARY_BITWISE_OR:  n = l |  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_XOR: n = l ^  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_AND: n = l &  r; break;
						default: Assert();
					}

					Convert(&type_int64, (Value*)&n, binary->type, (Value*)output); // Not necessary?

					if (binary->type == &type_bool)
					{
						Print("% % % = %\n", l, binary->op, r, (bool)n);
					}
					else
					{
						Print("% % % = %\n", l, binary->op, r, n);
					}
				}
				else
				{
					uint64 l = ConvertNumerical<uint64>((Value*)left,  binary->left->type->kind);
					uint64 r = ConvertNumerical<uint64>((Value*)right, binary->right->type->kind);
					uint64 n;

					switch (binary->kind)
					{
						case AST_EXPRESSION_BINARY_COMPARE_LESS:             n = l <  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_LESS_OR_EQUAL:    n = l <= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER:          n = l >  r; break;
						case AST_EXPRESSION_BINARY_COMPARE_GREATER_OR_EQUAL: n = l >= r; break;
						case AST_EXPRESSION_BINARY_COMPARE_EQUAL:            n = l == r; break;
						case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL:        n = l != r; break;
						case AST_EXPRESSION_BINARY_ADD:         n = l +  r; break;
						case AST_EXPRESSION_BINARY_SUBTRACT:    n = l -  r; break;
						case AST_EXPRESSION_BINARY_MULTIPLY:    n = l *  r; break;
						case AST_EXPRESSION_BINARY_DIVIDE:      n = l /  r; break;
						case AST_EXPRESSION_BINARY_MODULO:      n = l %  r; break;
						case AST_EXPRESSION_BINARY_EXPONENTIAL: n = Pow(l, r) + 0.5; break;
						case AST_EXPRESSION_BINARY_BITWISE_OR:  n = l |  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_XOR: n = l ^  r; break;
						case AST_EXPRESSION_BINARY_BITWISE_AND: n = l &  r; break;
						case AST_EXPRESSION_BINARY_LEFT_SHIFT:  n = l << r; break;
						case AST_EXPRESSION_BINARY_RIGHT_SHIFT: n = l >> r; break;
						default: Assert();
					}

					Convert(&type_uint64, (Value*)&n, binary->type, (Value*)output); // Not necessary?

					if (binary->type == &type_bool)
					{
						Print("% % % = %\n", l, binary->op, r, (bool)n);
					}
					else
					{
						Print("% % % = %\n", l, binary->op, r, n);
					}
				}
			}
			else
			{
				Assert(binary->left->type == binary->right->type);

				bool b;

				switch (binary->kind)
				{
					case AST_EXPRESSION_BINARY_COMPARE_EQUAL:     b =  CompareMemory(left, right, binary->left->type->size); break;
					case AST_EXPRESSION_BINARY_COMPARE_NOT_EQUAL: b = !CompareMemory(left, right, binary->left->type->size); break;
					default: Assert();
				}

				*(bool*)output = b;

				Print("% % % = %\n", binary->left, binary->op, binary->right, b);
			}
		} break;

		case AST_EXPRESSION_UNARY_BITWISE_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			int64 n = 0;
			Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
			n = ~n;
			Convert(unary->subexpression->type, (Value*)&n, unary->type, (Value*)output);
		} break;

		case AST_EXPRESSION_UNARY_NOT:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			int64 n = 0;
			Interpret(unary->subexpression, (char*)&n, false, frame, interpreter);
			*(bool*)output = !n;
		} break;

		case AST_EXPRESSION_UNARY_MINUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			char data[unary->subexpression->type->size];
			Interpret(unary->subexpression, data, false, frame, interpreter);

			switch (unary->subexpression->type->kind)
			{
				case TYPE_BASETYPE_BOOL:
					// @TestMe: I don't actually think this is semantically allowed?
					// Maybe make -bool evaluate to an int?
					*(bool*)output = -*(bool*)data;
					break;

				case TYPE_BASETYPE_INT8:
				case TYPE_BASETYPE_UINT8:
					*(int8*)output  = -*(int8*)data;
					break;

				case TYPE_BASETYPE_INT16:
				case TYPE_BASETYPE_UINT16:
					*(int16*)output = -*(int16*)data;
					break;

				case TYPE_BASETYPE_INT32:
				case TYPE_BASETYPE_UINT32:
					*(int32*)output = -*(int32*)data;
					break;

				case TYPE_BASETYPE_INT64:
				case TYPE_BASETYPE_UINT64:
					*(int64*)output = -*(int64*)data;
					break;

				case TYPE_BASETYPE_FLOAT16:
					*(float16*)output = -*(float16*)data;
					break;

				case TYPE_BASETYPE_FLOAT32:
					*(float32*)output = -*(float32*)data;
					break;

				case TYPE_BASETYPE_FLOAT64:
					*(float64*)output = -*(float64*)data;
					break;

				default:
					Assert();
			}
		} break;

		case AST_EXPRESSION_UNARY_PLUS:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			char data[unary->subexpression->type->size];
			Interpret(unary->subexpression, data, false, frame, interpreter);

			switch (unary->subexpression->type->kind)
			{
				case TYPE_BASETYPE_BOOL: *(bool*)output = *(bool*)data; break;

				case TYPE_BASETYPE_UINT8:  *(int8*)output  = *(int8*)data;  break;
				case TYPE_BASETYPE_UINT16: *(int16*)output = *(int16*)data; break;
				case TYPE_BASETYPE_UINT32: *(uint32*)output = *(uint32*)data; break;
				case TYPE_BASETYPE_UINT64: *(uint64*)output = *(uint64*)data; break;

				case TYPE_BASETYPE_INT8:  *(int8*)output  = Abs(*(int8*)data);  break;
				case TYPE_BASETYPE_INT16: *(int16*)output = Abs(*(int16*)data); break;
				case TYPE_BASETYPE_INT32: *(int32*)output = Abs(*(int32*)data); break;
				case TYPE_BASETYPE_INT64: *(int64*)output = Abs(*(int64*)data); break;

				case TYPE_BASETYPE_FLOAT16: Assert();
				case TYPE_BASETYPE_FLOAT32: *(float32*)output = Abs(*(float32*)data); break;
				case TYPE_BASETYPE_FLOAT64: *(float64*)output = Abs(*(float64*)data); break;

				default:
					Assert();
			}
		} break;

		case AST_EXPRESSION_UNARY_REFERENCE_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			char* ref;
			Interpret(unary->subexpression, (char*)&ref, false, frame, interpreter);

			if (allow_referential)
			{
				*(char**)output = ref;
			}
			else
			{
				CopyMemory(output, ref, unary->type->size);
			}
		} break;

		case AST_EXPRESSION_UNARY_ADDRESS_OF:
		{
			Ast_Expression_Unary* unary = (Ast_Expression_Unary*)expression;
			Interpret(unary->subexpression, output, true, frame, interpreter);
		} break;

		case AST_EXPRESSION_TERMINAL_LITERAL:
		{
			Ast_Expression_Literal* literal = (Ast_Expression_Literal*)expression;

			switch (literal->token->kind)
			{
				case TOKEN_LITERAL_STRING:
				{
					String string = literal->token->literal_string;
					CopyMemory(output, string.data, string.length);
				} break;

				case TOKEN_LITERAL_INT:
				case TOKEN_LITERAL_INT8:
				case TOKEN_LITERAL_INT16:
				case TOKEN_LITERAL_INT32:
				case TOKEN_LITERAL_INT64:
				case TOKEN_LITERAL_UINT:
				case TOKEN_LITERAL_UINT8:
				case TOKEN_LITERAL_UINT16:
				case TOKEN_LITERAL_UINT32:
				case TOKEN_LITERAL_UINT64:
				{
					CopyMemory(output, (char*)&literal->token->literal_int, literal->type->size);
				} break;

				case TOKEN_LITERAL_FLOAT:
				{
					*(float32*)output = (float32)literal->token->literal_float;
				} break;

				case TOKEN_LITERAL_FLOAT16:
				{
					Assert();
				} break;

				case TOKEN_LITERAL_FLOAT32:
				{
					*(float32*)output = (float32)literal->token->literal_float;
				} break;

				case TOKEN_LITERAL_FLOAT64:
				{
					*(float64*)output = (float64)literal->token->literal_float;
				} break;

				case TOKEN_TRUE:  *(bool*)output = true;  break;
				case TOKEN_FALSE: *(bool*)output = false; break;
				case TOKEN_NULL: *(char**)output = null;  break;

				default:
					Assert();
					Unreachable();
			}
		} break;

		case AST_EXPRESSION_TUPLE:
		{
			Ast_Expression_Tuple* tuple = (Ast_Expression_Tuple*)expression;

			for (uint32 i = 0; i < tuple->elements.count; i++)
			{
				Ast_Expression* element = tuple->elements[i];
				Interpret(element, output, false, frame, interpreter);
				output += element->type->size;
			}
		} break;

		case AST_EXPRESSION_CALL:
		{
			// @Todo @Bug: Make this work for "external" functions, or function pointers.
			//        ...  Maybe make them not work in compile time?
			Ast_Expression_Call* call = (Ast_Expression_Call*)expression;

			if (call->function->kind == AST_EXPRESSION_TERMINAL_FUNCTION)
			{
				Ast_Expression_Function* function_expression = (Ast_Expression_Function*)call->function;
				Ast_Function* function = function_expression->function;

				char uncasted_arguments[call->parameters->type->size];
				Interpret(call->parameters, uncasted_arguments, false, frame, interpreter);

				char arguments[function->type->input->size];
				Convert(call->parameters->type, (Value*)uncasted_arguments, function->type->input, (Value*)arguments);
				Interpret(function, arguments, output, interpreter);
			}
			else
			{
				Assert(call->function->kind == AST_EXPRESSION_TERMINAL_INTRINSIC);

				Ast_Expression_Intrinsic* intrinsic_expression = (Ast_Expression_Intrinsic*)call->function;
				IntrinsicID intrinsic = intrinsic_expression->intrinsic;

				char uncasted_arguments[call->parameters->type->size];

				Interpret(
					call->parameters,
					uncasted_arguments,
					false,
					frame,
					interpreter
				);

				char arguments[intrinsic_expression->type->input->size];

				Convert(
					call->parameters->type,
					(Value*)uncasted_arguments,
					intrinsic_expression->type->input,
					(Value*)arguments
				);

				switch (intrinsic)
				{
					case INTRINSIC_SYSTEM_CALL:
						*(int64*)output =
							SystemCall(
								((int64*)arguments)[0],
								((int64*)arguments)[1],
								((int64*)arguments)[2],
								((int64*)arguments)[3],
								((int64*)arguments)[4],
								((int64*)arguments)[5],
								((int64*)arguments)[6]
							);
						break;

					default:
						Assert();
				}
			}
		} break;

		default:
			Assert();
			Unreachable();
	}
}

void Interpret(Ast_Code* code, char* output, StackFrame* frame, Interpreter* interpreter)
{
	uint32 defer_count = 0;
	for (Ast_Statement* statement = code->statements;
		statement < code->statements.End() && !frame->do_return && !frame->do_break;
		statement++)
	{
		switch (statement->kind)
		{
			case AST_STATEMENT_EXPRESSION:
			{
				Ast_Expression* expression = statement->expression;

				char data[expression->type->size];
				Interpret(expression, data, false, frame, interpreter);
			} break;

			case AST_STATEMENT_VARIABLE_DECLARATION:
			{
				Ast_Variable* variable = &statement->variable_declaration;
				char* variable_data = StackFrameGetVariable(frame, variable);
				ZeroMemory(StackFrameGetVariable(frame, variable), variable->type->size);

				if (variable->assignment)
				{
					char data[variable->assignment->type->size];
					Interpret(variable->assignment, data, false, frame, interpreter);
					Convert(variable->assignment->type, (Value*)data, variable->type, (Value*)StackFrameGetVariable(frame, variable));
				}
			} break;

			case AST_STATEMENT_ASSIGNMENT:
			{
				Ast_Assignment* assignment = &statement->assignment;
				char* reference;
				Interpret(assignment->left, (char*)&reference, true, frame, interpreter);
				char data[assignment->right->type->size];
				Interpret(assignment->right, data, false, frame, interpreter);
				Convert(assignment->right->type, (Value*)data, assignment->left->type, (Value*)reference);
			} break;

			case AST_STATEMENT_ASSIGNMENT_ADD:
			case AST_STATEMENT_ASSIGNMENT_SUBTRACT:
			case AST_STATEMENT_ASSIGNMENT_MULTIPLY:
			case AST_STATEMENT_ASSIGNMENT_DIVIDE:
			case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL:
			{
				Ast_Assignment* assignment = &statement->assignment;
				char* reference;
				Interpret(assignment->left, (char*)&reference, true, frame, interpreter);

				if (IsSignedInteger(assignment->left->type))
				{
					int64 left = ConvertNumerical<int64>((Value*)reference, assignment->left->type->kind);

					char right_data[assignment->right->type->size];
					Interpret(assignment->right, right_data, false, frame, interpreter);
					int64 right = ConvertNumerical<int64>((Value*)right_data, assignment->right->type->kind);

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:         left += right; break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT:    left -= right; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY:    left *= right; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:      left /= right; break;
						case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL: left  = Pow(left, right) + 0.5; break;
						default: Unreachable();
					}

					Convert(&type_int64, (Value*)&left, assignment->left->type, (Value*)reference);
				}
				else if (IsUnsignedInteger(assignment->left->type) || IsPointer(assignment->left->type))
				{
					uint64 left = ConvertNumerical<uint64>((Value*)reference, assignment->left->type->kind);

					char right_data[assignment->right->type->size];
					Interpret(assignment->right, right_data, false, frame, interpreter);
					uint64 right = ConvertNumerical<uint64>((Value*)right_data, assignment->right->type->kind);

					if (!IsPointer(assignment->right->type))
					{
						right *= assignment->left->type->size;
					}

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:         left += right; break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT:    left -= right; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY:    left *= right; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:      left /= right; break;
						case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL: left  = Pow(left, right) + 0.5; break;
						default: Unreachable();
					}

					Convert(&type_uint64, (Value*)&left, assignment->left->type, (Value*)reference);
				}
				else if (IsFloat(assignment->left->type))
				{
					float64 left = ConvertNumerical<float64>((Value*)reference, assignment->left->type->kind);

					char right_data[assignment->right->type->size];
					Interpret(assignment->right, right_data, false, frame, interpreter);
					float64 right = ConvertNumerical<float64>((Value*)right_data, assignment->right->type->kind);

					switch (statement->kind)
					{
						case AST_STATEMENT_ASSIGNMENT_ADD:         left += right; break;
						case AST_STATEMENT_ASSIGNMENT_SUBTRACT:    left -= right; break;
						case AST_STATEMENT_ASSIGNMENT_MULTIPLY:    left *= right; break;
						case AST_STATEMENT_ASSIGNMENT_DIVIDE:      left /= right; break;
						case AST_STATEMENT_ASSIGNMENT_EXPONENTIAL: left  = Pow(left, right); break;
						default: Unreachable();
					}

					Convert(&type_float64, (Value*)&left, assignment->left->type, (Value*)reference);
				}
				else Assert();
			} break;

			case AST_STATEMENT_BRANCH_BLOCK:
			{
				Ast_BranchBlock* block = &statement->branch_block;
				Ast_Branch* branch = block->branches;

				Assert();

				// while (branch)
				// {
				// 	if (branch->condition)
				// 	{
				// 		bool is_if = branch->kind == AST_BRANCH_IF;
				// 		uint64 loop_count = 0;
				// 		char data[branch->condition->type->size];

				// 		while (!frame->do_break && !frame->do_return)
				// 		{
				// 			Interpret(branch->condition, data, false, frame, interpreter);
				// 			bool passed = ConvertNumerical<bool>((Value*)data, branch->condition->type->kind);

				// 			if (passed)
				// 			{
				// 				loop_count++;
				// 				Interpret(&branch->code, output, frame, interpreter);
				// 			}

				// 			if (!passed || is_if)
				// 			{
				// 				break;
				// 			}
				// 		}

				// 		if (!is_if)
				// 		{
				// 			frame->do_break = false;
				// 		}

				// 		branch = &block->branches[loop_count ? branch->then_branch_index : branch->else_branch_index];
				// 	}
				// 	else
				// 	{
				// 		Interpret(&branch->code, output, frame, interpreter);
				// 		break;
				// 	}
				// }

			} break;

			case AST_STATEMENT_CLAIM:
				continue;

			case AST_STATEMENT_DEFER:
			{
				defer_count++;
			} break;

			case AST_STATEMENT_RETURN:
			{
				Ast_Expression* expression = statement->ret.expression;
				frame->do_return = true;

				if (expression)
				{
					char data[expression->type->size];
					Interpret(expression, data, false, frame, interpreter);
					Convert(expression->type, (Value*)data, frame->function->return_type, (Value*)output);
				}

			} break;

			case AST_STATEMENT_BREAK:
			{
				frame->do_break = true;
			} break;

			case AST_STATEMENT_INCREMENT:
			{
				Ast_Increment* inc = &statement->increment;
				char* ref;
				Interpret(inc->expression, (char*)&ref, true, frame, interpreter);
				Type* type = inc->expression->type;

				switch (type->kind)
				{
					case TYPE_SPECIFIER_POINTER:
						*(char**)ref += type->subtype->size; break;

					case TYPE_BASETYPE_INT8:
					case TYPE_BASETYPE_UINT8:
						++*(int8*)ref; break;

					case TYPE_BASETYPE_INT16:
					case TYPE_BASETYPE_UINT16:
						++*(int16*)ref; break;

					case TYPE_BASETYPE_INT32:
					case TYPE_BASETYPE_UINT32:
						++*(uint32*)ref; break;

					case TYPE_BASETYPE_INT64:
					case TYPE_BASETYPE_UINT64:
						++*(uint64*)ref; break;

					case TYPE_BASETYPE_FLOAT16:
						Assert();

					case TYPE_BASETYPE_FLOAT32:
						++*(float32*)ref; break;

					case TYPE_BASETYPE_FLOAT64:
						++*(float64*)ref; break;

					default: Assert();
				}
			} break;

			case AST_STATEMENT_DECREMENT:
			{
				Ast_Increment* dec = &statement->increment;
				char* ref;
				Interpret(dec->expression, (char*)&ref, true, frame, interpreter);
				Type* type = dec->expression->type;

				switch (type->kind)
				{
					case TYPE_SPECIFIER_POINTER:
						*(char**)ref -= type->subtype->size; break;

					case TYPE_BASETYPE_INT8:
					case TYPE_BASETYPE_UINT8:
						--*(int8*)ref; break;

					case TYPE_BASETYPE_INT16:
					case TYPE_BASETYPE_UINT16:
						--*(int16*)ref; break;

					case TYPE_BASETYPE_INT32:
					case TYPE_BASETYPE_UINT32:
						--*(uint32*)ref; break;

					case TYPE_BASETYPE_INT64:
					case TYPE_BASETYPE_UINT64:
						--*(uint64*)ref; break;

					case TYPE_BASETYPE_FLOAT16:
						Assert();

					case TYPE_BASETYPE_FLOAT32:
						--*(float32*)ref; break;

					case TYPE_BASETYPE_FLOAT64:
						--*(float64*)ref; break;

					default: Assert();
				}
			} break;
		}
	}

	for (uint32 i = 0; i < defer_count; i++)
	{
		Ast_Defer* defer = code->defers[i];

		// Gross!
		bool temp_do_return = frame->do_return;
		bool temp_do_break  = frame->do_break;
		frame->do_return = false;
		frame->do_break  = false;
		Interpret(&defer->code, output, frame, interpreter);
		frame->do_return = temp_do_return;
		frame->do_break  = temp_do_break;
	}
}

void Interpret(Ast_Function* function, char* input, char* output, Interpreter* interpreter)
{
	Print("Interpreting function: %\n", function->name_token);
	StackFrame frame = CreateStackFrame(function, interpreter);

	if (function->parameters.count)
	{
		// @Note: Assuming The front of the stack is the input parameters.
		// @Warn: This might not always be the case.
		CopyMemory(frame.data, input, function->type->input->size);
	}

	for (Ast_Variable** variable = function->code.scope.variables.Begin(); variable < function->code.scope.variables.End(); variable++)
	{
		Print("&% = %\n", (*variable)->name, StackFrameGetVariable(&frame, *variable));
	}

	BufferFlush(&unix_output_buffer);

	Interpret(&function->code, output, &frame, interpreter);
}

static uint64 GetFreeSpace(MemoryBlock* block)
{
	return block->data + block->size - block->head;
}

static uint64 GetUsedSpace(MemoryBlock* block)
{
	return block->head - block->data;
}

StackFrame CreateStackFrame(Ast_Function* function, Interpreter* interpreter)
{
	StackFrame frame;
	ZeroMemory(&frame);

	uint64 size = CalculateStackFrameSize(function);

	if (!interpreter->block || size > GetFreeSpace(interpreter->block))
	{
		interpreter->block = CreateMemoryBlock(size, interpreter->block);
	}

	frame.data = interpreter->block->head;
	frame.function = function;
	interpreter->block->head += size;

	ZeroMemory(frame.data, size);
	return frame;
}

Interpreter* CreateInterpreter(Ast_Module* module)
{
	Interpreter* interpreter = StackAllocate<Interpreter>(&module->stack);
	ZeroMemory(interpreter);
	interpreter->block = CreateMemoryBlock(0x10000);
	return interpreter;
}

MemoryBlock* CreateMemoryBlock(uint64 min_size, MemoryBlock* prev)
{
	uint64 size = 0x1000;
	uint64 header_size = sizeof(MemoryBlock);

	if (prev)
	{
		size = prev->size * 2;
	}

	if (size - header_size <= min_size)
	{
		size = NextPow2(min_size + header_size);
	}

	MemoryBlock* block = (MemoryBlock*)GetPage(size);
	block->size = size - header_size;
	block->head = block->data;
	block->prev = prev;
	block->next = null;

	if (prev)
	{
		MemoryBlock* pn = prev->next;
		prev->next = block;
		pn->prev = block;
		block->next = pn;
	}

	return block;
}


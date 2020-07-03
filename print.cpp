#include "print.h"
#include "token.h"
#include <tgmath.h>

void Write(OutputBuffer* buffer, u8  n)
{
	Write(buffer, (u64)n);
}

void Write(OutputBuffer* buffer, u16 n)
{
	Write(buffer, (u64)n);
}

void Write(OutputBuffer* buffer, u32 n)
{
	Write(buffer, (u64)n);
}

// 3 5 10 20
void Write(OutputBuffer* buffer, u64 n)
{
	constexpr int max = 20; // ceil(log10(pow(2, sizeof(n)*8-1)))
	char digits[max];
	int count = 0;
	do
	{
		digits[max - count - 1] = '0' + n % 10;
	} while (++count < max && (n /= 10));
	buffer->Write(digits + (max - count), count);
}

void Write(OutputBuffer* buffer, s8  n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)abs(n));
}

void Write(OutputBuffer* buffer, s16 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)abs(n));
}

void Write(OutputBuffer* buffer, s32 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)abs(n));
}

void Write(OutputBuffer* buffer, s64 n)
{
	if (n < 0) buffer->Write('-');
	Write(buffer, (u64)llabs(n));
}

void Write(OutputBuffer* buffer, f32 n);
void Write(OutputBuffer* buffer, f64 n);

void Write(OutputBuffer* buffer, Token_Kind kind)
{
	Write(buffer, ToString(kind));
}

void Write(OutputBuffer* buffer, Token& token)
{
	if (token.kind == TOKEN_IDENTIFIER)
	{
		Write(buffer, token.info.string);
	}
	else if (token.kind == TOKEN_STRING_LITERAL)
	{
		buffer->Write('"');
		Write(buffer, token.info.span);
		buffer->Write('"');
	}
	else if (token.kind == TOKEN_INTEGER_LITERAL)
	{
		Write(buffer, token.info.integer.value);
	}
	else if (token.kind == TOKEN_FLOAT_LITERAL)
	{
		// TODO: Implement f64 print function.
		//Write(buffer, token.info.floating_point.value);
	}
	else
	{
		Write(buffer, token.kind);
	}
}

void Write(OutputBuffer* buffer, Token* token)
{
	if (!token) Write(buffer, "null");
	else Write(buffer, *token);
}

void Write(OutputBuffer* buffer, Span<Token> span)
{
	for (u64 i = 0; i < span.Length(); i++)
	{
		if (i != 0) Write(buffer, " ");
		Write(buffer, span[i]);
	}
}

void Write(OutputBuffer* buffer, SourceLocation loc)
{
	Write(buffer, loc.line + 1);
	buffer->Write(':');
	Write(buffer, loc.offset + 1);
}


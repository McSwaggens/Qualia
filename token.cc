#include "token.h"
#include "print.h"

static void Print(OutputBuffer* buffer, TokenKind kind) {
	buffer->Write(ToString(kind));
}

void Token::Print(OutputBuffer* buffer) const {
	switch (kind) {
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_CASUAL:
		case TOKEN_IDENTIFIER_FORMAL: {
			buffer->Write(identifier_string);
		} break;

		case TOKEN_LITERAL_STRING: {
			::Print(buffer, "\"%\"", literal_string);
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
		case TOKEN_LITERAL_UINT64: {
			::Print(buffer, literal_int);
		} break;

		case TOKEN_LITERAL_FLOAT:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64: {
			::Print(buffer, literal_float);
		} break;

		default: {
			::Print(buffer, kind);
		} break;
	}
}

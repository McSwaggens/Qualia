#include "print.h"
#include "math.h"

static void Print(OutputBuffer* buffer, char c) { buffer->Write(c); }

// 3 5 10 20
// Lut for n < 256?
static void Print(OutputBuffer* buffer, u64 n) {
	const int max = 20; // ceil(log10(pow(2, sizeof(n)*8-1)))
	char digits[max];
	int count = 0;

	do {
		digits[max - count - 1] = '0' + n % 10;
	} while (++count < max && (n /= 10));

	buffer->Write(digits + (max - count), count);
}

static void Print(OutputBuffer* buffer, s8  n) { Print(buffer, (s64)n); }
static void Print(OutputBuffer* buffer, s16 n) { Print(buffer, (s64)n); }
static void Print(OutputBuffer* buffer, s32 n) { Print(buffer, (s64)n); }
static void Print(OutputBuffer* buffer, u8  n) { Print(buffer, (u64)n); }
static void Print(OutputBuffer* buffer, u16 n) { Print(buffer, (u64)n); }
static void Print(OutputBuffer* buffer, u32 n) { Print(buffer, (u64)n); }
static void Print(OutputBuffer* buffer, unsigned long int n) { Print(buffer, (u64)n); }

static void Print(OutputBuffer* buffer, s64 n) {
	if (n < 0) { buffer->Write('-'); n = -n; }
	Print(buffer, (u64)n);
}

void Hex::Print(OutputBuffer* buffer) const {
	const u8 length_table[65] = {
		16, 16, 16, 16,
		15, 15, 15, 15,
		14, 14, 14, 14,
		13, 13, 13, 13,
		12, 12, 12, 12,
		11, 11, 11, 11,
		10, 10, 10, 10,
		9,  9,  9,  9,
		8,  8,  8,  8,
		7,  7,  7,  7,
		6,  6,  6,  6,
		5,  5,  5,  5,
		4,  4,  4,  4,
		3,  3,  3,  3,
		2,  2,  2,  2,
		1,  1,  1,  1, 1
	};

	char character_buffer[17];

	u64 digits = length_table[Clz64(value)];
	u64 k = digits << 2;

	for (u64 i = 0; i < digits; i++) {
		k -= 4;
		character_buffer[i] = "0123456789ABCDEF"[(value >> k) & 0xF];
	}

	character_buffer[digits] = 'h';
	buffer->Write(character_buffer, digits+1);
}

void Bin::Print(OutputBuffer* buffer) const {
	if (!value) {
		buffer->Write("0b");
		return;
	}

	const u32 table[16] = {
		0x30303030,
		0x31303030,
		0x30313030,
		0x31313030,
		0x30303130,
		0x31303130,
		0x30313130,
		0x31313130,
		0x30303031,
		0x31303031,
		0x30313031,
		0x31313031,
		0x30303131,
		0x31303131,
		0x30313131,
		0x31313131,
	};

	char character_buffer[65];
	s64 lz = Clz64(value);

	for (s64 i = 0; i < 16; i++) {
		((u32*)character_buffer)[i] = table[(value >> (60-(i*4))) & 0x0f];
	}

	character_buffer[64] = 'b';
	buffer->Write(character_buffer+lz, 65-lz);
}

static void Print(OutputBuffer* buffer, void* p) { Hex((u64)p).Print(buffer); }

static void Print(OutputBuffer* buffer, bool b) {
	if (b) buffer->Write("true");
	else   buffer->Write("false");
}

static void Print(OutputBuffer* buffer, String str) {
	str.Print(buffer);
}

void String::Print(OutputBuffer* buffer) const {
	if (!data) COLD {
		buffer->Write("<null-string>");
		return;
	}

	buffer->Write(data, length);
}

static void Print(OutputBuffer* buffer, float32 f) {
	Print(buffer, (float64)f);
}

static void Print(OutputBuffer* buffer, float64 f) {
	// Extract IEEE-754 fields from the raw bits directly: the Float64 bitfield
	// struct in math.h is laid out LSB-first, so its named fields don't match
	// the actual sign/exponent/mantissa positions on this platform.
	Float64 bits; bits.fp = f;
	u64  raw      = (u64)bits.i;
	u64  exponent = (raw >> 52) & 0x7FF;
	u64  mantissa = raw & ((1ull << 52) - 1);
	bool negative = (raw >> 63) & 1;

	if (exponent == 0x7FF) { // inf / nan
		if      (mantissa) buffer->Write("nan");
		else if (negative) buffer->Write("-inf");
		else               buffer->Write("inf");
		return;
	}

	if (negative) { buffer->Write('-'); f = -f; }

	// @Todo: Guard |f| >= 2^63 (integer part overflows u64).
	// @Todo: Shortest round-trip (Grisu2/Ryu) if we ever need to serialize literals back to source.
	u64 integer_part = (u64)f;
	Print(buffer, integer_part);
	buffer->Write('.');

	const int N = 9;
	u64 scaled = (u64)((f - (float64)integer_part) * 1e9 + 0.5); // round to nearest

	char digits[N]; // emit all N fractional digits, leading zeros included
	for (int i = N-1; i >= 0; i--) {
		digits[i] = '0' + scaled % 10;
		scaled /= 10;
	}

	int last = N-1;
	while (last > 0 && digits[last] == '0') last--; // trim trailing zeros, keep at least one
	buffer->Write(digits, last+1);
}

template<typename T>
void Array<T>::Print(OutputBuffer* buffer) const {
	buffer->Write("{ ");

	for (u64 i = 0; i < length; i++) {
		if (i != 0) buffer->Write(", ");
		::Print(buffer, data[i]);
	}

	buffer->Write(" }");
}

template<typename T>
void List<T>::Print(OutputBuffer* buffer) const {
	Array<T>(data, count).Print(buffer);
}

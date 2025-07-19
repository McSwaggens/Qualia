#pragma once

#include "general.h"
#include "string.h"

// ------------ ASCII ------------ //
// 00: Null                 0000_0000
// 01: Start Of Heading     0000_0001
// 02: Start Of Text        0000_0010
// 03: End Of Text          0000_0011
// 04: End Of Transmission  0000_0100
// 05: Enquiry              0000_0101
// 06: Acknowledge          0000_0110
// 07: Bell                 0000_0111
// 08: Backspace            0000_1000
// 09: Horizontal Tab       0000_1001
// 0a: New Line             0000_1010
// 0b: Vertical Tab         0000_1011
// 0c: Form Feed            0000_1100
// 0d: Carriage Return      0000_1101
// 0e: Shift Out            0000_1110
// 0f: Shift In             0000_1111
// 10: Data Link Escape     0001_0000
// 11: Device Control 1     0001_0001
// 12: Device Control 2     0001_0010
// 13: Device Control 3     0001_0011
// 14: Device Control 4     0001_0100
// 15: Negative Acknowledge 0001_0101
// 16: Syncronous Idle      0001_0110
// 17: End Of Transmission  0001_0111
// 18: Cancel               0001_1000
// 19: End of Medium        0001_1001
// 1a: Substitute           0001_1010
// 1b: Escape               0001_1011
// 1c: File   Separator     0001_1100
// 1d: Group  Separator     0001_1101
// 1e: Record Separator     0001_1110
// 1f: Unit   Separator     0001_1111
// 20: Space                0010_0000
// 21: !                    0010_0001
// 22: "                    0010_0010
// 23: #                    0010_0011
// 24: $                    0010_0100
// 25: %                    0010_0101
// 26: &                    0010_0110
// 27: '                    0010_0111
// 28: (                    0010_1000
// 29: )                    0010_1001
// 2a: *                    0010_1010
// 2b: +                    0010_1011
// 2c: ,                    0010_1100
// 2d: -                    0010_1101
// 2e: .                    0010_1110
// 2f: /                    0010_1111
// 30: 0                    0011_0000
// 31: 1                    0011_0001
// 32: 2                    0011_0010
// 33: 3                    0011_0011
// 34: 4                    0011_0100
// 35: 5                    0011_0101
// 36: 6                    0011_0110
// 37: 7                    0011_0111
// 38: 8                    0011_1000
// 39: 9                    0011_1001
// 3a: :                    0011_1010
// 3b: ;                    0011_1011
// 3c: <                    0011_1100
// 3d: =                    0011_1101
// 3e: >                    0011_1110
// 3f: ?                    0011_1111
// 40: @                    0100_0000
// 41: A                    0100_0001
// 42: B                    0100_0010
// 43: C                    0100_0011
// 44: D                    0100_0100
// 45: E                    0100_0101
// 46: F                    0100_0110
// 47: G                    0100_0111
// 48: H                    0100_1000
// 49: I                    0100_1001
// 4a: J                    0100_1010
// 4b: K                    0100_1011
// 4c: L                    0100_1100
// 4d: M                    0100_1101
// 4e: N                    0100_1110
// 4f: O                    0100_1111
// 50: P                    0101_0000
// 51: Q                    0101_0001
// 52: R                    0101_0010
// 53: S                    0101_0011
// 54: T                    0101_0100
// 55: U                    0101_0101
// 56: V                    0101_0110
// 57: W                    0101_0111
// 58: X                    0101_1000
// 59: Y                    0101_1001
// 5a: Z                    0101_1010
// 5b: [                    0101_1011
// 5c: '\'                  0101_1100
// 5d: ]                    0101_1101
// 5e: ^                    0101_1110
// 5f: _                    0101_1111
// 60: `                    0110_0000
// 61: a                    0110_0001
// 62: b                    0110_0010
// 63: c                    0110_0011
// 64: d                    0110_0100
// 65: e                    0110_0101
// 66: f                    0110_0110
// 67: g                    0110_0111
// 68: h                    0110_1000
// 69: i                    0110_1001
// 6a: j                    0110_1010
// 6b: k                    0110_1011
// 6c: l                    0110_1100
// 6d: m                    0110_1101
// 6e: n                    0110_1110
// 6f: o                    0110_1111
// 70: p                    0111_0000
// 71: q                    0111_0001
// 72: r                    0111_0010
// 73: s                    0111_0011
// 74: t                    0111_0100
// 75: u                    0111_0101
// 76: v                    0111_0110
// 77: w                    0111_0111
// 78: x                    0111_1000
// 79: y                    0111_1001
// 7a: z                    0111_1010
// 7b: {                    0111_1011
// 7c: |                    0111_1100
// 7d: }                    0111_1101
// 7e: ~                    0111_1110
// 7f: Delete               0111_1111

typedef u8 AsciiFlags;
static const AsciiFlags ASCII_NONE       = 0x00;
static const AsciiFlags ASCII_BINARY     = 0x02; // 0, 1
static const AsciiFlags ASCII_DECIMAL    = 0x0A; // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
static const AsciiFlags ASCII_HEX_ALPHA  = 0x10; // A, B, C, D, E, F, a, b, c, d, e, f
static const AsciiFlags ASCII_DIGIT_MASK = ASCII_DECIMAL | ASCII_HEX_ALPHA;
static const AsciiFlags ASCII_SYMBOL     = 0x20;
static const AsciiFlags ASCII_WHITESPACE = 0x40;
static const AsciiFlags ASCII_ALPHA      = 0x80;

static const AsciiFlags ASCII_FLAGS_TABLE[256] = {
	[0]    = ASCII_NONE,
	[1]    = ASCII_NONE,
	[2]    = ASCII_NONE,
	[3]    = ASCII_NONE,
	[4]    = ASCII_NONE,
	[5]    = ASCII_NONE,
	[6]    = ASCII_NONE,
	['\a'] = ASCII_NONE,
	['\b'] = ASCII_NONE,
	['\t'] = ASCII_WHITESPACE,
	['\n'] = ASCII_WHITESPACE,
	['\v'] = ASCII_NONE,
	['\f'] = ASCII_NONE,
	['\r'] = ASCII_WHITESPACE,
	[14]   = ASCII_NONE,
	[15]   = ASCII_NONE,
	[16]   = ASCII_NONE,
	[17]   = ASCII_NONE,
	[18]   = ASCII_NONE,
	[19]   = ASCII_NONE,
	[20]   = ASCII_NONE,
	[21]   = ASCII_NONE,
	[22]   = ASCII_NONE,
	[23]   = ASCII_NONE,
	[24]   = ASCII_NONE,
	[25]   = ASCII_NONE,
	[26]   = ASCII_NONE,
	[27]   = ASCII_NONE,
	[28]   = ASCII_NONE,
	[29]   = ASCII_NONE,
	[30]   = ASCII_NONE,
	[31]   = ASCII_NONE,
	[' ']  = ASCII_WHITESPACE,
	['!']  = ASCII_SYMBOL,
	['"']  = ASCII_SYMBOL,
	['#']  = ASCII_SYMBOL,
	['$']  = ASCII_SYMBOL,
	['%']  = ASCII_SYMBOL,
	['&']  = ASCII_SYMBOL,
	['\''] = ASCII_SYMBOL,
	['(']  = ASCII_SYMBOL,
	[')']  = ASCII_SYMBOL,
	['*']  = ASCII_SYMBOL,
	['+']  = ASCII_SYMBOL,
	[',']  = ASCII_SYMBOL,
	['-']  = ASCII_SYMBOL,
	['.']  = ASCII_SYMBOL,
	['/']  = ASCII_SYMBOL,
	['0']  = ASCII_BINARY, // Escape
	['1']  = ASCII_BINARY,
	['2']  = ASCII_DECIMAL,
	['3']  = ASCII_DECIMAL,
	['4']  = ASCII_DECIMAL,
	['5']  = ASCII_DECIMAL,
	['6']  = ASCII_DECIMAL,
	['7']  = ASCII_DECIMAL,
	['8']  = ASCII_DECIMAL,
	['9']  = ASCII_DECIMAL,
	[':']  = ASCII_SYMBOL,
	[';']  = ASCII_SYMBOL,
	['<']  = ASCII_SYMBOL,
	['=']  = ASCII_SYMBOL,
	['>']  = ASCII_SYMBOL,
	['?']  = ASCII_SYMBOL,
	['@']  = ASCII_SYMBOL,
	['A']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['B']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['C']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['D']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['E']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['F']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['G']  = ASCII_ALPHA,
	['H']  = ASCII_ALPHA,
	['I']  = ASCII_ALPHA,
	['J']  = ASCII_ALPHA,
	['K']  = ASCII_ALPHA,
	['L']  = ASCII_ALPHA,
	['M']  = ASCII_ALPHA,
	['N']  = ASCII_ALPHA,
	['O']  = ASCII_ALPHA,
	['P']  = ASCII_ALPHA,
	['Q']  = ASCII_ALPHA,
	['R']  = ASCII_ALPHA,
	['S']  = ASCII_ALPHA,
	['T']  = ASCII_ALPHA,
	['U']  = ASCII_ALPHA,
	['V']  = ASCII_ALPHA,
	['W']  = ASCII_ALPHA,
	['X']  = ASCII_ALPHA,
	['Y']  = ASCII_ALPHA,
	['Z']  = ASCII_ALPHA,
	['[']  = ASCII_SYMBOL,
	['\\'] = ASCII_SYMBOL,
	[']']  = ASCII_SYMBOL,
	['^']  = ASCII_SYMBOL,
	['_']  = ASCII_SYMBOL,
	['`']  = ASCII_SYMBOL,
	['a']  = ASCII_ALPHA | ASCII_HEX_ALPHA, // Escape
	['b']  = ASCII_ALPHA | ASCII_HEX_ALPHA, // Escape
	['c']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['d']  = ASCII_ALPHA | ASCII_HEX_ALPHA,
	['e']  = ASCII_ALPHA | ASCII_HEX_ALPHA, // Escape
	['f']  = ASCII_ALPHA | ASCII_HEX_ALPHA, // Escape
	['g']  = ASCII_ALPHA,
	['h']  = ASCII_ALPHA,
	['i']  = ASCII_ALPHA,
	['j']  = ASCII_ALPHA,
	['k']  = ASCII_ALPHA,
	['l']  = ASCII_ALPHA,
	['m']  = ASCII_ALPHA,
	['n']  = ASCII_ALPHA,
	['o']  = ASCII_ALPHA,
	['p']  = ASCII_ALPHA,
	['q']  = ASCII_ALPHA,
	['r']  = ASCII_ALPHA,
	['s']  = ASCII_ALPHA,
	['t']  = ASCII_ALPHA,
	['u']  = ASCII_ALPHA,
	['v']  = ASCII_ALPHA,
	['w']  = ASCII_ALPHA,
	['x']  = ASCII_ALPHA,
	['y']  = ASCII_ALPHA,
	['z']  = ASCII_ALPHA,
	['{']  = ASCII_SYMBOL,
	['|']  = ASCII_SYMBOL,
	['}']  = ASCII_SYMBOL,
	['~']  = ASCII_SYMBOL,
	[127]  = ASCII_NONE, // Delete
};

static const u8 ASCII_DECODE_TABLE[256] = {
	[0]    = '0',
	[1]    = 0xFF,
	[2]    = 0xFF,
	[3]    = 0xFF,
	[4]    = 0xFF,
	[5]    = 0xFF,
	[6]    = 0xFF,
	['\a'] = 'a',
	['\b'] = 'b',
	['\t'] = 't',
	['\n'] = 'n',
	['\v'] = 'v',
	['\f'] = 'f',
	['\r'] = 'r',
	[14]   = 0xFF,
	[15]   = 0xFF,
	[16]   = 0xFF,
	[17]   = 0xFF,
	[18]   = 0xFF,
	[19]   = 0xFF,
	[20]   = 0xFF,
	[21]   = 0xFF,
	[22]   = 0xFF,
	[23]   = 0xFF,
	[24]   = 0xFF,
	[25]   = 0xFF,
	[26]   = 0xFF,
	[27]   = 0xFF,
	[28]   = 0xFF,
	[29]   = 0xFF,
	[30]   = 0xFF,
	[31]   = 0xFF,
	[' ']  = 0xFF,
	['!']  = 0xFF,
	['"']  = 0xFF,
	['#']  = 0xFF,
	['$']  = 0xFF,
	['%']  = 0xFF,
	['&']  = 0xFF,
	['\''] = 0xFF,
	['(']  = ')',
	[')']  = '(',
	['*']  = 0xFF,
	['+']  = 0xFF,
	[',']  = 0xFF,
	['-']  = 0xFF,
	['.']  = 0xFF,
	['/']  = 0xFF,
	['0']  = 0x00,
	['1']  = 0x01,
	['2']  = 0x02,
	['3']  = 0x03,
	['4']  = 0x04,
	['5']  = 0x05,
	['6']  = 0x06,
	['7']  = 0x07,
	['8']  = 0x08,
	['9']  = 0x09,
	[':']  = 0xFF,
	[';']  = 0xFF,
	['<']  = '>',
	['=']  = 0xFF,
	['>']  = '<',
	['?']  = 0xFF,
	['@']  = 0xFF,
	['A']  = 0x0A,
	['B']  = 0x0B,
	['C']  = 0x0C,
	['D']  = 0x0D,
	['E']  = 0x0E,
	['F']  = 0x0F,
	['G']  = 0xFF,
	['H']  = 0xFF,
	['I']  = 0xFF,
	['J']  = 0xFF,
	['K']  = 0xFF,
	['L']  = 0xFF,
	['M']  = 0xFF,
	['N']  = 0xFF,
	['O']  = 0xFF,
	['P']  = 0xFF,
	['Q']  = 0xFF,
	['R']  = 0xFF,
	['S']  = 0xFF,
	['T']  = 0xFF,
	['U']  = 0xFF,
	['V']  = 0xFF,
	['W']  = 0xFF,
	['X']  = 0xFF,
	['Y']  = 0xFF,
	['Z']  = 0xFF,
	['[']  = ']',
	['\\'] = 0xFF,
	[']']  = '[',
	['^']  = 0xFF,
	['_']  = 0xFF,
	['`']  = 0xFF,
	['a']  = 0x0A,
	['b']  = 0x0B,
	['c']  = 0x0C,
	['d']  = 0x0D,
	['e']  = 0x0E,
	['f']  = 0x0F,
	['g']  = 0xFF,
	['h']  = 0xFF,
	['i']  = 0xFF,
	['j']  = 0xFF,
	['k']  = 0xFF,
	['l']  = 0xFF,
	['m']  = 0xFF,
	['n']  = 0xFF,
	['o']  = 0xFF,
	['p']  = 0xFF,
	['q']  = 0xFF,
	['r']  = 0xFF,
	['s']  = 0xFF,
	['t']  = 0xFF,
	['u']  = 0xFF,
	['v']  = 0xFF,
	['w']  = 0xFF,
	['x']  = 0xFF,
	['y']  = 0xFF,
	['z']  = 0xFF,
	['{']  = '}',
	['|']  = 0xFF,
	['}']  = '{',
	['~']  = 0xFF,
	[127]  = 0xFF,
	0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

static inline bool CheckLowerCaseBit(char c) { return  (c & 0x20); }
static inline bool CheckUpperCaseBit(char c) { return !(c & 0x20); }

static inline bool IsWhiteSpace(char c) { return c == '\t' || c == ' '; }
static inline bool IsPrintable(char c)  { return c >= ' ' && c < 0x7F; }
static inline bool IsLowerCase(char c)  { return c >= 'a' && c <= 'z'; }
static inline bool IsUpperCase(char c)  { return c >= 'A' && c <= 'Z'; }
static inline bool IsAlpha(char c)      { return c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'; }
static inline bool IsHex(char c)        { return c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'; }
static inline bool IsHexAlpha(char c)   { return c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'; }
static inline bool IsDecimal(char c)    { return c >= '0' && c <= '9'; }
static inline bool IsBinary(char c)     { return c >= '0' && c <= '1'; }

static inline AsciiFlags GetAsciiFlags(char c) { return ASCII_FLAGS_TABLE[(u8)c]; }
static inline bool IsAlphaLut(char c)    { return GetAsciiFlags(c) &  ASCII_ALPHA;     }
static inline bool IsHexLut(char c)      { return GetAsciiFlags(c) & (ASCII_HEX_ALPHA|ASCII_DECIMAL); }
static inline bool IsHexAlphaLut(char c) { return GetAsciiFlags(c) &  ASCII_HEX_ALPHA; }
static inline bool IsDecimalLut(char c)  { return GetAsciiFlags(c) &  ASCII_DECIMAL;   }
static inline bool IsBinaryLut(char c)   { return GetAsciiFlags(c) &  ASCII_BINARY;    }

enum Base : u8
{
	BASE2  = 2,
	// octal is for losers.
	BASE10 = 10,
	BASE16 = 16,

	BASE_BINARY      = BASE2,
	BASE_DECIMAL     = BASE10,
	BASE_HEXADECIMAL = BASE16,

	BASE_HEX = BASE_HEXADECIMAL,
};

static String ToString(Base base) {
	switch (base) {
		case BASE_BINARY:      return "Binary";
		case BASE_DECIMAL:     return "Decimal";
		case BASE_HEXADECIMAL: return "Hexadecimal";
	}
}

static inline bool IsDigit(char c, Base base) {
	switch (base) {
		case BASE16: return c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f';
		case BASE10: return c >= '0' && c <= '9';
		case BASE2:  return c >= '0' && c <= '1';
	}
}

static inline u64 DecodeDigitLut(char c) { return ASCII_DECODE_TABLE[(u8)c]; }

static inline u64 DecodeDigit(char c, Base base) {
	switch (base) {
		case BASE16:
			if (c >= 'A' && c <= 'F') return c-'A'+0x0A;
			if (c >= 'a' && c <= 'f') return c-'a'+0x0A;
			return c - '0';

		case BASE10: return c - '0';
		case BASE2:  return c - '0';
	}
}

static inline u64 DecodeDigit_Known(char c) {
	switch (c) {
		case '0': return 0x00;
		case '1': return 0x01;

		case '2': return 0x02;
		case '3': return 0x03;
		case '4': return 0x04;
		case '5': return 0x05;
		case '6': return 0x06;
		case '7': return 0x07;
		case '8': return 0x08;
		case '9': return 0x09;

		case 'A': return 0x0A;
		case 'B': return 0x0B;
		case 'C': return 0x0C;
		case 'D': return 0x0D;
		case 'E': return 0x0E;
		case 'F': return 0x0F;

		case 'a': return 0x0A;
		case 'b': return 0x0B;
		case 'c': return 0x0C;
		case 'd': return 0x0D;
		case 'e': return 0x0E;
		case 'f': return 0x0F;

		default: Unreachable();
	}
}

static inline char GetEscapeCharacter(char c) {
	switch (c) {
		case '0': return '\0';
		case 'a': return '\a';
		case 'b': return '\b';
		case 't': return '\t';
		case 'n': return '\n';
		case 'v': return '\v';
		case 'f': return '\f';
		case 'r': return '\r';
		case '\"': return '\"';
		case '\'': return '\'';
		case '\\': return '\\';
		default: return 0;
	}
}

static inline bool IsEscapeCharacter(char c) {
	switch (c) {
		case '0':  return true;
		case 'a':  return true;
		case 'b':  return true;
		case 't':  return true;
		case 'n':  return true;
		case 'v':  return true;
		case 'f':  return true;
		case 'r':  return true;
		case '"':  return true;
		case '\'': return true;
		case '\\': return true;
		default: return false;
	}
}


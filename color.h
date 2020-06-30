#pragma once

#include "string.h"
#include "print.h"

enum Color
{
	Black   = 0,
	Red     = 1,
	Green   = 2,
	Yellow  = 3,
	Blue    = 4,
	Magenta = 5,
	Cyan    = 6,
	White   = 7,
};

void SetCursorPosition(u32 x, u32 y);
void SetForegroundColor(Color color);
void SetBackgroundColor(Color color);

void ResetColor();    // 0
void SetBold();       // 1
void SetItalic();     // 3 ???
void SetUnderscore(); // 4
void SetBlink();      // 5
void SetReverse();    // 7
void SetConcealed();  // 8
void Clear();


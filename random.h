#pragma once

#include "int.h"

struct RandomState
{
	u64 states[4];
	u8 n;
};

void InitRandom();

u8  Random8();
u16 Random16();
u32 Random32();
u64 Random64();


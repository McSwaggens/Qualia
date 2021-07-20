#include "random.h"

RandomState rs;

void InitRandom()
{
	rs.n = 0;
	// @Todo: Use RDRAND or RDSEED instructions for initial states.
	rs.states[0] = 42;
	rs.states[1] = 1337;
	rs.states[2] = 8086;
	rs.states[3] = 314159;
}

u64 Random()
{
	u64 n = rs.n;
	u64 s = rs.states[n];
	rs.n = (rs.n+1)&(4-1);
	s ^= s << 13;
	s ^= s << 7;
	s ^= s << 17;
	rs.states[n] = s;
	return s;
}

u8  Random8()  { return Random(); }
u16 Random16() { return Random(); }
u32 Random32() { return Random(); }
u64 Random64() { return Random(); }


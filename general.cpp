#include "file_system.h"
#include "general.h"
#include "linux.h"

[[noreturn]]
static void ExitProcess(bool success)
{
	BufferFlush(&unix_output_buffer);
	BufferFlush(&unix_error_buffer);
	SystemCall(LINUX_SYSCALL_EXIT, !success);
	Unreachable();
}

struct RandomState
{
	uint64 states[4];
	uint8 n;
};

static RandomState random_state;

static void InitRandom()
{
	random_state.n = 0;
	// @Todo: Use RDRAND or RDSEED instructions for initial states.
	random_state.states[0] = ReadPerformanceCounter();
	random_state.states[1] = 0x0123456789ABCDEF;
	random_state.states[2] = 0xDEADBEEF0BADBABE;
	random_state.states[3] = 0xFEDCBA9876543210;
}

static uint8  Random8()  { return Random64(); }
static uint16 Random16() { return Random64(); }
static uint32 Random32() { return Random64(); }

static uint64 Random64()
{
	uint64 n = random_state.n;
	uint64 s = random_state.states[n];
	random_state.n = (random_state.n+1)&3;
	s ^= s << 13llu;
	s ^= s >> 7llu;
	s ^= s << 17llu;
	random_state.states[n] = s;
	return s;
}


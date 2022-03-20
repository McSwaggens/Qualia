#pragma once

#include "general.h"

typedef uint8 ThreadID;

struct Thread
{
	bool   alive;
	int32  system_thread_id;
	byte*  stack; // Beginning of allocation (left most in memory)
	uint64 stack_size;
	// Current task? eg. Lexing, Parsing, etc..
	// Dependencies?
};

static const uint64 MAX_THREAD_COUNT = 256;
static Thread threads[MAX_THREAD_COUNT];
static ThreadID thread_count;
static ThreadID thread_id_counter;
static int32 linux_process_id;

static void InitThread(void);
static void KillThread(ThreadID thread);
static ThreadID CreateThread(uint64 stack_size, void (*thread_start_function)(ThreadID id));



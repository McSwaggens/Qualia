#pragma once

#include "general.h"

typedef u8 ThreadID;

struct Thread
{
	bool   alive;
	s32  system_thread_id;
	byte*  stack; // Beginning of allocation (left most in memory)
	u64 stack_size;
	// Current task? eg. Lexing, Parsing, etc..
	// Dependencies?
};

static const u64 MAX_THREAD_COUNT = 256;
static Thread threads[MAX_THREAD_COUNT];
static ThreadID thread_count;
static ThreadID thread_id_counter;
static s32 linux_process_id;

static void InitThread(void);
static void KillThread(ThreadID thread);
static ThreadID CreateThread(u64 stack_size, void (*thread_start_function)(ThreadID id));



#pragma once

#include "general.h"
#include "fixed_buffer.h"
#include "os.h"

using ThreadID = u8;

struct Thread {
	enum Status {
		UNINITIALIZED = 0,
		RUNNING,
		PAUSED,
	};

	volatile Status status;
	OS::ThreadID system_thread_id;
	Array<byte> stack;
	// Current task? eg. Lexing, Parsing, etc..

	static FixedBuffer<Thread, 256> thread_pool;
	static void Init();
	static Thread* Create(u64 stack_size, void (*thread_start_function)(ThreadID id));

	inline byte* GetTopOfStack() { return stack.End(); }

	inline bool IsRunning()     { return status == RUNNING; }
	inline bool IsInitialized() { return status != UNINITIALIZED; }

	void Pause();  // Stop the thread. (RUNNING -> PAUSED)
	void Resume(); // Resume the thread after it's been paused. (PAUSED -> RUNNING)
};


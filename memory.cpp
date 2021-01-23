#include "memory.h"
#include "util.h"
#include "assert.h"
#include <sys/mman.h>
#include <unistd.h>

u64 GetSystemPageSize()
{
	return sysconf(_SC_PAGE_SIZE);
}

//
// On x86 both the page and directory table descriptors' have an 'R' flag.
//        What do you think the 'R' flag does?
//        If you guessed it's the 'R'ead flag, you clearly don't understand how fucking stupid Intel is.
//        It's even more funny because thay call it the 'read/write' flag, when in actuallity it just changes the write permission.
// So... what this means is that if you pass *JUST* the 'PROT_WRITE' flag into mmap, it's actually the same as 'PROT_READ | PROT_WRITE'.
//       PROT_READ doesn't actually do anything...
//       And of course the man pages aren't helpfull at all (as usual) and don't document this at all.
//       Searching this on the internet won't help either. I literally had to infer this from the Intel x86 spec.
//
//                                             x86 was a mistake.
//
void* AllocateVirtualPage(u64 size, bool write, bool execute)
{
	u64 page_size = GetSystemPageSize();
	Assert(size == (size & -page_size), "Invalid page size detected.");
	u32 flags = 1; // Might as well put READ flag on by default, not like it matters anyway... >:(
	if (write)   flags |= 2;
	if (execute) flags |= 4;
	void* page = (void*)SystemCall(9, 0, size, flags, 0x22, -1, 0);
	Assert(page != MAP_FAILED, "Failed to map virtual page.");
	return page;
}

void DeAllocateVirtualPage(void* page, u64 size)
{
	SystemCall(11, (u64)page, size);
}


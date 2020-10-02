#include "memory.h"
#include "util.h"
#include "assert.h"
#include <sys/mman.h>
#include <unistd.h>

u64 GetSystemPageSize()
{
	return sysconf(_SC_PAGE_SIZE);
}

void* AllocateVirtualPage(u64 size)
{
	u64 page_size = GetSystemPageSize();
	// Print("Allocating virtual page: %K\n", size / 1024);
	Assert(size == (size & -page_size), "Invalid page size detected.");
	void* page = mmap(null, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	Assert(page != MAP_FAILED, "Failed to map virtual page.");
	return page;
}

void DeAllocateVirtualPage(void* page, u64 size)
{
	munmap(page, size);
}


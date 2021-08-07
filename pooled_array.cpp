#include "pooled_array.h"
#include "memory.h"
#include "assert.h"

struct Bucket
{
	char* data;
	u64 size;
	Bucket* next;
};

Bucket pool[POOL_SIZE];
Bucket* free_head;
Bucket* head;

static Bucket* TakeFreeBucket()
{
	Bucket* bucket = free_head;
	free_head = free_head->next;
	return bucket;
}

static Bucket* TakeHeadBucket()
{
	Bucket* bucket = head;
	head = head->next;
	return bucket;
}

static void SetHeadBucket(Bucket* bucket)
{
	bucket->next = head;
	head = bucket;
}

static void FreeBucket(Bucket* bucket)
{
	bucket->next = free_head;
	free_head = bucket;
}

Pool_Allocation ExpandArray(char* p, u64 size)
{
	Pool_Allocation result;

	if (!p)
	{
		if (head)
		{
			Bucket* bucket = TakeHeadBucket();
			result.data = bucket->data;
			result.size = bucket->size;
			FreeBucket(bucket);
		}
		else
		{
			result.data = AllocateVirtualPage(BUCKET_SIZE);
			result.size = BUCKET_SIZE;
		}
	}
	else
	{
		Bucket* bucket = TakeFreeBucket();
		bucket->data = p;
		bucket->size = size;
		SetHeadBucket(bucket);

		// @Todo: Make sure allocated size is larger than input size.
		u64 new_size = NextPow2(size + BUCKET_SIZE);
		result.data = AllocateVirtualPage(new_size);
		result.size = new_size;

		CopyMemory((char*)result.data, p, size);
	}

	return result;
}

void ReleaseArray(char* p, u64 size)
{
	if (size >= 256)
	{
		Bucket* bucket = TakeFreeBucket();
		bucket->data = p;
		bucket->size = size;
		SetHeadBucket(bucket);
	}

	// Forget about this block of memory, it's not worth it.
}

void InitArrayPool()
{
	for (u32 i = 0; i < POOL_SIZE; i++)
	{
		Bucket* bucket = pool + i;
		bucket->data = null;
		bucket->size = 0;
		bucket->next = bucket + 1;
	}

	pool[POOL_SIZE-1].next = null;
	free_head = &pool[0];
}


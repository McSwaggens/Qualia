// Radix trie for binary data interning. Deduplicates identical byte sequences
// by storing them once and returning pointers to the canonical copy.

#include "binary.h"
#include "fixed_array.h"
#include "alloc.h"

struct BinaryNode {
	u64 size;
	char* data; // Full data (shared with ancestors)
	FixedArray<BinaryNode*, 256> children;
};

static BinaryNode root_node = {
	.size = 0,
	.data = null,
	.children = { },
};

static u64 FindDisagreement(const byte* a, const byte* b, u64 size) {
	for (u64 i = 0; i < size; i++)
		if (a[i] != b[i])
			return i;
	return size;
}

static Stack binstack;
Binary Binary::LARGE_ZERO;

static void InitEvalSystem() {
	binstack = Stack::Create(1llu<<32);
	Binary::LARGE_ZERO = Binary::Create(Array<const byte>(BIG_ARRAY_OF_ZEROES, sizeof(BIG_ARRAY_OF_ZEROES)));
}

// Find or insert binary data, returning a canonical Binary handle.
Binary Binary::Create(Array<const byte> data) {
	if (data.length * 8 <= Binary::INLINE_BITS) {
		u64 value = 0;
		CopyMemory((byte*)&value, data.data, data.length);
		return Binary(value, data.length * 8);
	}

	u64 head = 0;
	u64 size = data.length;

	BinaryNode* node = &root_node;
	BinaryNode** pnode = null;  // Pointer to the slot holding 'node' (for splits)

	while (true) {
		// Compare remaining bytes against current node
		u64 common_after_head = Min(node->size, size) - head;
		head += FindDisagreement(data.data + head, node->data + head, common_after_head);

		if (head == node->size) {
			// Matched entire node prefix
			if (head == size)
				return Binary((byte*)node->data, size * 8);  // Exact match

			// Traverse to child, or create new leaf
			BinaryNode*& child = node->children[data[head]];
			if (child) {
				pnode = &child;
				node = child;
				continue;
			}

			BinaryNode* leaf = binstack.Allocate<BinaryNode>();
			*leaf = {
				.size = size,
				.data = (char*)binstack.CopyAllocMemory(data, size),
			};
			child = leaf;
			node = leaf;
			break;
		}

		// Disagreement within node - split required
		Assert(head < node->size);
		Assert(pnode);

		BinaryNode* split = binstack.Allocate<BinaryNode>();
		*split = { .size = head, .data = node->data, };
		split->children[node->data[head]] = node; // Old node as child

		if (head == size) {
			// New data is a prefix of the existing node
			*pnode = split;
			node = split;
		} else {
			BinaryNode* leaf = binstack.Allocate<BinaryNode>();
			*leaf = { .size = size, .data = (char*)binstack.CopyAllocMemory(data, size), };
			split->children[data[head]] = leaf;
			*pnode = split;
			node = leaf;
		}
		break;
	}

	return Binary((byte*)node->data, size * 8);
}

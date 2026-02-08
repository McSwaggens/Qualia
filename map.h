#pragma once

#include "general.h"
#include "list.h"
#include "memory.h"
#include "sort.h"
#include "optional.h"

template<typename Key, typename Value>
struct Map {
	Key*   keys   = null;
	Value* values = null;
	u32 count     = 0;
	u32 capacity  = 0;

	Map() = default;

	u32 NextCapacity(u32 n) {
		if (n == 0) return 0;
		if (n <= 8) return 8;
		return NextPow2(n);
	}

	void ChangeCapacity(u32 new_capacity) {
		new_capacity = NextCapacity(new_capacity);
		keys   = ReAlloc(keys,   capacity, new_capacity);
		values = ReAlloc(values, capacity, new_capacity);
		capacity = new_capacity;
	}

	void AssureCapacity(u32 new_capacity) {
		if (capacity >= new_capacity)
			return;

		ChangeCapacity(new_capacity);
	}

	void InsertIndex(u32 index, Key key, Value value) {
		AssureCapacity(count+1);

		Move(keys   + index+1, keys   + index, count - index);
		Move(values + index+1, values + index, count - index);

		keys[index] = key;
		values[index] = value;

		count += 1;
	}

	Optional<u32> GetKeyIndex(Key key) {
		u32 index = BinarySearch(Array<Key>(keys, count), key) - keys;

		if (index == count)
			return OptNone;

		if (keys[index] != key)
			return OptNone;

		return index;
	}

	bool DoesKeyExist(Key key) {
		return GetKeyIndex(key) != count;
	}

	void Add(Key key, Value value) {
		u32 index = GetKeyIndex(key);
		if (index != count && keys[index] == key)
			return;

		Insert(index, key, value);
	}

	void Remove(Key key) {
		u32 index = GetKeyIndex(key);
		if (keys[index] != key)
			return;

		Move(keys   + index, keys   + index + 1, count - (index + 1));
		Move(values + index, values + index + 1, count - (index + 1));

		count -= 1;
	}

	Key GetKey(u32 index) {
		Assert(index < count);
		return keys[index];
	}

	Value GetValue(u32 index) {
		Assert(index < count);
		return values[index];
	}

	Optional<Value&> TryGet(Key key) {
		u32 index = GetKeyIndex(key);
		if (keys[index] != key)
			return OptNone;

		return values[index];
	}

	struct GetOrAddResult { bool was_inserted; Value* value; };
	GetOrAddResult GetOrAdd(Key key, Value value = { }) {
		u32 index = BinarySearch(Array<Key>(keys, count), key) - keys;

		if (index < count && keys[index] == key)
			return { false, &values[index] };

		InsertIndex(index, key, value);
		return { true, &values[index] };
	}

	List<Value> CopyValues() {
		List<Value> result;
		result.Add(values);
		return result;
	}

	List<Key> CopyKeys() {
		List<Key> result;
		result.Add(keys);
		return result;
	}

	void Free() {
		Free(keys,   count);
		Free(values, count);
		count    = 0;
		capacity = 0;
	}
};

template<typename Key, typename Value>
static inline void Free(Map<Key, Value> map) {
	map.Free();
}

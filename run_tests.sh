#!/bin/bash

passed=0
failed=0
errors=""

for f in tests/*.q; do
    printf "%-40s" "$f"
    output=$(./qualia "$f" 2>&1 > /dev/null)
    if [ $? -eq 0 ]; then
        echo "PASS"
        passed=$((passed + 1))
    else
        echo "FAIL"
        echo "$output" | sed 's/^/    /'
        echo "    ----------------------------------------"
        failed=$((failed + 1))
        errors="$errors  $f\n"
    fi
done

echo ""
echo "Results: $passed passed, $failed failed"

if [ $failed -ne 0 ]; then
    echo ""
    printf "Failed:\n$errors"
    exit 1
fi

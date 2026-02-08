#!/bin/bash

passed=0
failed=0
errors=""

for f in tests/*.q; do
    printf "%-40s" "$f"
    output=$(./qualia "$f" 2>&1)
    if [ $? -eq 0 ]; then
        echo "✅ PASS"
        passed=$((passed + 1))
    else
        echo "❌ FAIL"
        echo "$output"
        echo "----------------------------------------"
        failed=$((failed + 1))
        errors="$errors$f\n"
    fi
done

echo ""
if [ $failed -eq 0 ]; then
    echo "Results: $passed ✅ passed, $failed ❌ failed 🎉"
else
    echo "Results: $passed ✅ passed, $failed ❌ failed"
fi

if [ $failed -ne 0 ]; then
    echo ""
    printf "Failed:\n$errors"
    exit 1
fi

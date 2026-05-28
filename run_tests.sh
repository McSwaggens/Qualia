#!/bin/bash

passed=0
failed=0
errors=""

# Run C++ tests
for f in tests/*; do
    # Skip .q files, .cc files, and non-executables
    [[ -d "$f" ]] && continue
    [[ "$f" == *.q ]] && continue
    [[ "$f" == *.cc ]] && continue
    [[ ! -x "$f" ]] && continue

    printf "%-40s" "$f.cc"
    "$f" > /tmp/test_output_$$ 2>&1
    if [ $? -eq 0 ]; then
        echo "✅ PASS"
        passed=$((passed + 1))
    else
        echo "❌ FAIL"
        cat /tmp/test_output_$$
        echo "----------------------------------------"
        failed=$((failed + 1))
        errors="$errors$f\n"
    fi
    rm -f /tmp/test_output_$$
done

# Run .q language tests
for f in tests/*.q; do
    printf "%-40s" "$f"
    ./qualia "$f" > /tmp/test_output_$$ 2>&1
    if [ $? -eq 0 ]; then
        echo "✅ PASS"
        passed=$((passed + 1))
    else
        echo "❌ FAIL"
        cat /tmp/test_output_$$
        echo "----------------------------------------"
        failed=$((failed + 1))
        errors="$errors$f\n"
    fi
    rm -f /tmp/test_output_$$
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

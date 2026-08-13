#!/usr/bin/env bash
# REPL test runner: feeds each tests/repl/*.in session into the REPL and
# compares against the .expected file. Tests live in their own process, so
# each .in starts from a fresh session.
set -u

cd "$(dirname "$0")/../.."

BIN=./bin/phos
DIR=tests/repl
pass=0
fail=0

for input in "$DIR"/*.in; do
    base="${input%.in}"
    expected="$base.expected"

    if [ ! -f "$expected" ]; then
        echo "MISSING EXPECTED: $expected"
        fail=$((fail + 1))
        continue
    fi

    # Ensure a trailing newline so 'quit' never glues to the last line.
    # Run from inside the test dir so relative :load paths resolve.
    { cat "$input"; printf '\n:quit\n'; } | (cd "$DIR" && "$OLDPWD/$BIN" -repl) > /tmp/opencode/repl.out 2>&1

    if diff -u "$expected" /tmp/opencode/repl.out > /tmp/opencode/repl.diff 2>&1; then
        echo "ok   $base"
        pass=$((pass + 1))
    else
        echo "FAIL $base"
        cat /tmp/opencode/repl.diff
        fail=$((fail + 1))
    fi
done

echo
echo "repl tests: $pass passed, $fail failed"
[ "$fail" -eq 0 ]

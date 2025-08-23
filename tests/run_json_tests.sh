#!/usr/bin/env bash
set -euo pipefail

BIN="${BIN:-./build/c2math}"
OUT="tests/actual-json"
EXP="tests/expected-json"
CLANG_FLAGS=${CLANG_FLAGS:-'-- -x c -std=c11'}

mkdir -p "$OUT"

fail=0
for src in tests/fr_tests/*.c; do
  base=$(basename "$src" .c)

  # Allow per-test extra flags: put them in tests/fr_tests/<name>.flags (optional)
  extra_flags=""
  [ -f "tests/fr_tests/$base.flags" ] && extra_flags=" $(cat "tests/fr_tests/$base.flags")"

  # Produce fresh JSON
  $BIN --dump-json "$src" $CLANG_FLAGS$extra_flags \
    | jq -S . > "$OUT/$base.json"

  # Compare
  if [ ! -f "$EXP/$base.json" ]; then
    echo "[NEW] No golden for $base.json — consider blessing it:"
    echo "      cp \"$OUT/$base.json\" \"$EXP/$base.json\""
    fail=1
    continue
  fi

  if ! diff -u "$EXP/$base.json" "$OUT/$base.json"; then
    echo "[FAIL] $base"
    fail=1
  else
    echo "[OK]   $base"
  fi
done

exit $fail


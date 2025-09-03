#!/usr/bin/env bash
set -euo pipefail

# Usage: from repo root (after building), run:
#   bash tests/fr009/run_fr009_golden.sh
#
# You can override tool paths with env vars:
#   C2AST=/path/to/c2ast  C2MATH=/path/to/c2math  bash tests/fr009/run_fr009_golden.sh

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
C2MATH_BIN="${BIN:-$DIR/../../build/c2math}" 

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
tdir="${root}/tests/fr009"
work="${tdir}/_work"
mkdir -p "${work}"

input_c="${tdir}/input.c"
expected_c="${tdir}/expected.c"
json_out="${work}/out.json"
emit_c="${work}/out.c"

# 1) C → JSON via c2ast
"${C2MATH_BIN}" --dump-json "${input_c}" -- -x c -std=c11 > "${json_out}"

# 2) JSON → C via c2math
"${C2MATH_BIN}" --read-json "${json_out}" --emit-c "${emit_c}"

# 3) Compare with golden
if diff -u "${expected_c}" "${emit_c}"; then
  echo "FR-009 golden: OK"
else
  echo "FR-009 golden: FAILED"
  exit 1
fi

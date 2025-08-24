#!/usr/bin/env bash
set -euo pipefail

# --- Config (override via env) ---
DIR="$(cd -- "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN="${BIN:-$DIR/../build/c2math}"         # path to your built binary
EXP_DIR="${EXP_DIR:-$DIR/expected-json}"   # golden JSONs (inputs)
OUT_DIR="${OUT_DIR:-$DIR/roundtrip-json}"  # fresh outputs here
JQ="${JQ:-jq}"                             # jq executable

# --- Preconditions ---
if ! command -v "$JQ" >/dev/null 2>&1; then
  echo "ERROR: jq not found. Install with: sudo apt install jq" >&2
  exit 2
fi
if [[ ! -x "$BIN" ]]; then
  echo "ERROR: binary not found or not executable: $BIN" >&2
  exit 2
fi

mkdir -p "$OUT_DIR"

# Collect files (fail nicely if none)
shopt -s nullglob
files=("$EXP_DIR"/*.json)
if (( ${#files[@]} == 0 )); then
  echo "ERROR: No JSON files found in $EXP_DIR" >&2
  exit 1
fi

fail=0
for f in "${files[@]}"; do
  base="$(basename "$f")"
  out="$OUT_DIR/$base"

  # Import → export → canonicalize (sorted) for stable diffs
  if ! "$BIN" --read-json "$f" --dump-json | "$JQ" -S . > "$out"; then
    echo "[FAIL] $base (import/export failed)"
    fail=1
    continue
  fi

  # Compare canonicalized forms (handles whitespace/ordering)
  if ! diff -u <("$JQ" -S . "$f") <("$JQ" -S . "$out"); then
    echo "[DIFF] $base"
    fail=1
  else
    echo "[OK]   $base"
  fi
done

if (( fail )); then
  echo "Round-trip failed for one or more files."
  exit 1
else
  echo "Round-trip OK for ${#files[@]} files."
fi


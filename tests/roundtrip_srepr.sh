# scripts/roundtrip_srepr.sh
#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "usage: $0 /path/to/c2math file1.c [file2.c ...]" >&2; exit 2
fi

C2M="$1"; shift
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

norm() {
  # canonicalize JSON: drop version/types/exprs, sort decls by name, emit stable compact json
  # uses jq if available; else uses python as a fallback
  if command -v jq >/dev/null 2>&1; then
    jq -S '
      {decls: (.decls | map({name,kind,srepr}) | sort_by(.name))}
    ' -c
  else
    python3 - "$@" <<'PY'
import sys, json
data=json.load(sys.stdin)
decls=sorted(
  ({'name':d.get('name'), 'kind':d.get('kind'), 'srepr':d.get('srepr')}
   for d in data.get('decls',[]) if isinstance(d,dict)),
  key=lambda d: (d.get('name') or '')
)
json.dump({'decls':decls}, sys.stdout, separators=(',',':'), sort_keys=True)
PY
  fi
}

FAILED=0
for SRC in "$@"; do
  base="$(basename "${SRC%.*}")"
  s1="$TMP/${base}_1.json"
  s2="$TMP/${base}_2.json"
  n1="$TMP/${base}_1.norm.json"
  n2="$TMP/${base}_2.norm.json"

  # C → srepr
  "$C2M" --dump-srepr "$SRC" -- -x c -std=c11 > "$s1"
  # srepr → IR → srepr
  "$C2M" --read-srepr "$s1" --dump-srepr -- -x c -std=c11 > "$s2"

  norm <"$s1" >"$n1"
  norm <"$s2" >"$n2"

  if ! diff -u "$n1" "$n2"; then
    echo "[FAIL] srepr roundtrip differs for $SRC" >&2
    FAILED=1
  else
    echo "[OK]   $SRC"
  fi
done

exit $FAILED

# From repo root
mkdir -p tests/expected-json tests/actual-json

BIN=./build/c2math
CLANG_FLAGS='-- -x c -std=c11'   # tweak includes as needed

for src in tests/fr_tests/*.c; do
  base=$(basename "$src" .c)
  $BIN --dump-json "$src" $CLANG_FLAGS | jq -S . > "tests/expected-json/$base.json"
done


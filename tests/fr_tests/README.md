# FR Smoke Tests (C → IR → SymPy → IR → C)

This folder contains small, header-free C11 snippets designed to exercise **FR-001..FR-008**:

- FR-001 Parse translation unit
- FR-002 Build symbol table
- FR-003 AST → IR lowering
- FR-004 IR serialization (JSON)
- FR-005 IR deserialization (JSON → IR)
- FR-006 IR → SymPy emitter (prefer JSON `srepr` form)
- FR-007 SymPy → IR ingestor
- FR-008 C code generator

## File index (by focus)

**Core (MVP-friendly)**
- `01_const.c` — return literal (Integer).  
- `02_arith.c` — arithmetic and precedence.  
- `03_assign.c` — declarations, assignments, compound-assign.  
- `04_calls.c` — internal call + static function.  
- `05_casts.c` — C-style cast (double→int).  
- `06_conditional_expr.c` — ternary `?:` (encode as `Piecewise`).  
- `11_arrays.c` — array indexing (read-only).  
- `13_strings.c` — string literal + char comparison.

**Optional early (maps to FR-011 later)**
- `07_if_else.c` — structured `if/else`.  
- `08_while.c` — `while` loop.  
- `09_for.c` — `for` loop (desugar to while).  
- `10_switch.c` — `switch/case` (encode as `Piecewise`).  
- `12_pointers_basic.c` — pointer deref, side effects.

## Suggested checks per stage

- **AST sanity (FR-001):** Confirm functions, params, and statements are discovered with correct source ranges.  
- **Symbol table (FR-002):** `inc` is internal (static), `use_inc` resolves its call to `inc`. Shadowing not present here.  
- **IR lowering (FR-003):** Ensure node kinds exist for literals, binops, decls, return, call, cast, conditional (`?:`), subscript, deref.  
- **IR JSON (FR-004/FR-005):** Round-trip each file IR → JSON → IR with a byte-for-byte stable JSON (field-order stable).  
- **SymPy emission (FR-006):** 
  - Map `+,-,*,/` → `Add`, `Mul`, `Pow`/rational division, or keep `Div` semantics as needed.  
  - Ternary and if/else → `Piecewise`.  
  - Functions → `Lambda((args...), body)`.  
  - String literals → `String("...")`; array subscripts → `IndexedBase`/`Indexed` or function `get(a,i)`.  
- **SymPy ingestion (FR-007):** Prefer `srepr` JSON; verify you reject constructs you don’t support with clear diagnostics.  
- **Codegen (FR-008):** Re-emit C; build with any C11 compiler to ensure syntactic correctness.

## Notes
- Tests avoid headers to reduce include-path friction.  
- `12_pointers_basic.c` introduces mutation; you may carry side-effects as IR annotations initially.

Happy hacking!

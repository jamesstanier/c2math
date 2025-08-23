# c2math

A research project to translate C code into an intermediate representation (IR) for symbolic mathematics and code generation.

## Features (so far)
- FR-001: Parse C → Clang AST
- FR-002: Build symbol table (scopes, identifiers, typedefs, tags, linkage)
- FR-003: Lower AST → IR (with holes for unsupported constructs)

## Roadmap
- FR-004: JSON export of IR
- FR-005: JSON import
- FR-006–FR-013: type system, structs/unions, control flow, SymPy interop

## Build
```bash
mkdir build && cd build
cmake ..
make

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
```

## License

This project is dual-licensed:

- Open Source: [GPL v3.0](./LICENSE)
- Commercial: [Commercial License](./LICENSE-COMMERCIAL) (contact me for details)

If you are building an open-source project, you are free to use this under the terms of the GPL v3.0.
For closed-source, proprietary use, please [contact me](mailto:j.stanier766@gmail.com).


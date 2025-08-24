#!/usr/bin/env python3
import argparse
import json
import sys
from typing import Any, Dict, List, Tuple

# SymPy imports
from sympy import sympify, srepr  # type: ignore

def check_srepr(s: str) -> Tuple[bool, str]:
    try:
        obj = sympify(s)
        # Basic sanity: it should be a SymPy "Basic" subclass (which sympify returns).
        # Optionally round-trip its srepr just to ensure it is representable again.
        _ = srepr(obj)
        return True, ""
    except Exception as e:
        return False, f"{type(e).__name__}: {e}"

def try_eval_lambda(s: str) -> Tuple[bool, str]:
    """If the object is a Lambda, try calling it with 1s for each variable."""
    try:
        obj = sympify(s)
        # Lambda detection without importing Lambda directly
        if obj.func.__name__ != "Lambda":
            return True, ""  # not a lambda, nothing to do
        # Try to call the lambda with simple numeric args
        # For Lambda((x, y), ...) .variables exists; for Lambda(x, ...) it's a single var
        vars_attr = getattr(obj, "variables", None)
        if vars_attr is None:
            return True, ""  # unexpected, but don't fail the whole test
        n = len(tuple(vars_attr))
        if n == 0:
            # zero-arg lambda: just evaluate it
            _ = obj()
            return True, ""
        # Use 1 for each arg; this is just a smoke test
        args = (1,) * n
        _ = obj(*args)
        return True, ""
    except Exception as e:
        return False, f"{type(e).__name__}: {e}"

def main() -> int:
    ap = argparse.ArgumentParser(description="Validate a c2math SymPy srepr JSON with SymPy.")
    ap.add_argument("json_path", help="Path to sympy srepr JSON (produced by --dump-srepr).")
    ap.add_argument("--no-eval", action="store_true", help="Skip attempting to eval Lambda decls.")
    args = ap.parse_args()

    with open(args.json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    total = 0
    ok = 0
    failures: List[str] = []

    def handle_entry(kind: str, key: str, s: str) -> None:
        nonlocal total, ok
        total += 1
        good, err = check_srepr(s)
        label = f"{kind}:{key}"
        if not good:
            failures.append(f"[{label}] sympify failed: {err}\n  srepr: {s}")
            return
        if not args.no_eval and kind == "decl":
            egood, eerr = try_eval_lambda(s)
            if not egood:
                failures.append(f"[{label}] lambda eval failed: {eerr}\n  srepr: {s}")
                return
        ok += 1

    # types (list of srepr strings)
    for i, s in enumerate(data.get("types", [])):
        if isinstance(s, str):
            handle_entry("type", str(i), s)

    # exprs (list of srepr strings)
    for i, s in enumerate(data.get("exprs", [])):
        if isinstance(s, str):
            handle_entry("expr", str(i), s)

    # decls (list of dicts with "name", "srepr")
    for i, d in enumerate(data.get("decls", [])):
        if isinstance(d, dict):
            name = str(d.get("name", f"#{i}"))
            s = d.get("srepr")
            if isinstance(s, str):
                handle_entry("decl", name, s)

    print(f"SymPy srepr validation: {ok}/{total} entries passed.", file=sys.stdout)
    if failures:
        print("\nFailures:", file=sys.stdout)
        for msg in failures:
            print(" - " + msg, file=sys.stdout)
        return 1
    return 0

if __name__ == "__main__":
    sys.exit(main())

import json
from sympy import sympify

def load_srepr_module(path):
    """
    Load a c2math srepr JSON into SymPy objects.

    Returns a dict with keys:
      - types: list of SymPy objects
      - exprs: list of SymPy objects
      - decls: dict mapping decl name -> SymPy object
    """
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)

    types = [sympify(s) for s in data.get("types", []) if isinstance(s, str)]
    exprs = [sympify(s) for s in data.get("exprs", []) if isinstance(s, str)]

    decls = {}
    for d in data.get("decls", []):
        if isinstance(d, dict) and "name" in d and "srepr" in d:
            decls[d["name"]] = sympify(d["srepr"])

    return {"types": types, "exprs": exprs, "decls": decls}

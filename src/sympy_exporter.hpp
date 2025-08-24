
#pragma once
#include <ostream>
#include <string>
#include <vector>
#include <cstdint>
#include <sstream>
#include "ir.hpp"

// FR-006: IR → SymPy emitter (srepr JSON)
// This header provides a tiny printer that converts the FR-003 IR into
// strings matching SymPy's srepr() for a small core of objects:
// - Types-as-symbols (e.g. Symbol('int'), Symbol('float'), ...)
// - Expressions: Integer/Float/Str/VarRef, Add/Sub/Mul/Div, Call
// - Functions as Lambda forms: Lambda((params...), body)
// The output format is a deterministic JSON object with three arrays:
//   { "ir_version":"...",
//     "types":[ <srepr string per TypeId> ],
//     "exprs":[ <srepr string per ExprId> ],
//     "decls":[ {"name":..., "kind":..., "srepr":... }, ... ] }
//
// NOTES:
// • We emit "Str('...')" for string literals which matches SymPy's srepr.
// • Subtraction is encoded as Add(lhs, Mul(Integer(-1), rhs)) which is how
//   SymPy represents subtraction in srepr.
// • Division is encoded as Mul(lhs, Pow(rhs, Integer(-1))).
// • Calls are emitted as Function('f')(args...) when callee is a VarRef('f').
//   Otherwise we conservatively emit Function('call')(callee, *args).
//
// This emitter intentionally has no SymPy/Python dependency; it only formats
// strings which a downstream tool can evaluate in Python if desired.

namespace c2sympy {

// ---------- tiny JSON string escaper (reused) ----------
inline void write_escaped(std::ostream& os, const std::string& s) {
  os << '"';
  for (unsigned char c : s) {
    switch (c) {
      case '\"': os << "\\\""; break;
      case '\\': os << "\\\\"; break;
      case '\b': os << "\\b"; break;
      case '\f': os << "\\f"; break;
      case '\n': os << "\\n"; break;
      case '\r': os << "\\r"; break;
      case '\t': os << "\\t"; break;
      default:
        if (c < 0x20) {
          static const char* hex = "0123456789abcdef";
          os << "\\u00" << hex[(c>>4)&0xF] << hex[c&0xF];
        } else {
          os << c;
        }
    }
  }
  os << '"';
}

inline std::string sym_symbol(const std::string& name) {
  std::ostringstream o; o << "Symbol('";
  for (char c : name) {
    if (c == '\\' || c=='\'' ) o << '\\';
    o << c;
  }
  o << "')";
  return o.str();
}

inline std::string sym_integer(long long v) {
  std::ostringstream o; o << "Integer(" << v << ")"; return o.str();
}
inline std::string sym_float(double v) {
  std::ostringstream o; 
  // Print with enough precision but keep it simple
  o.setf(std::ios::fixed); o.precision(17);
  o << "Float(" << v << ")";
  return o.str();
}
inline std::string sym_str(const std::string& s) {
  std::ostringstream o; o << "Str('";
  for (char c : s) {
    if (c=='\\' || c=='\'') o << '\\';
    o << c;
  }
  o << "')";
  return o.str();
}

// ---------- Types as Symbols ----------
inline std::string repr_type(const c2ir::Module& M, c2ir::TypeId t);

inline std::string repr_type_kind(c2ir::TypeKind k) {
  using TK=c2ir::TypeKind;
  switch (k) {
    case TK::Void:   return "Symbol('void')";
    case TK::Bool:   return "Symbol('bool')";
    case TK::Char:   return "Symbol('char')";
    case TK::Int:    return "Symbol('int')";
    case TK::UInt:   return "Symbol('uint')";
    case TK::Long:   return "Symbol('long')";
    case TK::ULong:  return "Symbol('ulong')";
    case TK::Float:  return "Symbol('float')";
    case TK::Double: return "Symbol('double')";
    default:         return "Symbol('unknown')";
  }
}

inline std::string repr_type(const c2ir::Module& M, c2ir::TypeId t) {
  if (t<0 || (size_t)t>=M.types.size()) return "Symbol('invalid_type')";
  const auto& T = M.types[t];
  using TK=c2ir::TypeKind;
  switch (T.kind) {
    case TK::Pointer: {
      std::ostringstream o;
      o << "Function('ptr')(" << repr_type(M, T.pointee) << ")";
      return o.str();
    }
    case TK::Function: {
      std::ostringstream o;
      o << "Function('fn_type')(" << repr_type(M, T.ret);
      for (auto p : T.params) { o << ", " << repr_type(M, p); }
      if (T.varargs) o << ", Symbol('varargs')";
      o << ")";
      return o.str();
    }
    default:
      return repr_type_kind(T.kind);
  }
}

// ---------- Expressions ----------
inline std::string repr_expr(const c2ir::Module& M, c2ir::ExprId e);

inline std::string repr_binop(c2ir::BinOp b,
                              const std::string& a,
                              const std::string& bstr) {
  using BO=c2ir::BinOp;
  switch (b) {
    case BO::Add: return "Add(" + a + ", " + bstr + ")";
    case BO::Sub: return "Add(" + a + ", Mul(Integer(-1), " + bstr + "))";
    case BO::Mul: return "Mul(" + a + ", " + bstr + ")";
    case BO::Div: return "Mul(" + a + ", Pow(" + bstr + ", Integer(-1)))";
  }
  return "Symbol('binop_unknown')";
}

inline std::string repr_call(const c2ir::Module& M, const c2ir::Expr& e) {
  // If callee is a VarRef, emit Function('name')(args...)
  std::string callee = "Symbol('callee')";
  if (e.callee>=0 && (size_t)e.callee<M.exprs.size()) {
    const auto& C = M.exprs[e.callee];
    if (C.kind==c2ir::ExprKind::VarRef) {
      callee = "Function('" + C.str_val + "')";
    } else {
      // Fallback: include the callee as first arg to a generic 'call'
      std::ostringstream o;
      o << "Function('call')(" << repr_expr(M, e.callee);
      for (auto a : e.args) o << ", " << repr_expr(M, a);
      o << ")";
      return o.str();
    }
  }
  std::ostringstream o;
  o << callee << "(";
  for (size_t i=0;i<e.args.size();++i) {
    if (i) o << ", ";
    o << repr_expr(M, e.args[i]);
  }
  o << ")";
  return o.str();
}

inline std::string repr_expr(const c2ir::Module& M, c2ir::ExprId eid) {
  if (eid<0 || (size_t)eid>=M.exprs.size()) return "Symbol('invalid_expr')";
  const auto& E = M.exprs[eid];
  using EK=c2ir::ExprKind;
  switch (E.kind) {
    case EK::IntLit:    return sym_integer(E.int_val);
    case EK::FloatLit:  return sym_float(E.float_val);
    case EK::StringLit: return sym_str(E.str_val);
    case EK::VarRef:    return sym_symbol(E.str_val);
    case EK::BinOp:     return repr_binop(E.binop, repr_expr(M, E.lhs), repr_expr(M, E.rhs));
    case EK::Call:      return repr_call(M, E);
    case EK::Hole:      return sym_symbol(std::string("HOLE:") + E.str_val);
  }
  return "Symbol('expr_unknown')";
}

// ---------- Statements (only Return is mapped to an expression) ----------
inline c2ir::ExprId find_return_expr(const c2ir::Module& M, c2ir::StmtId sid) {
  if (sid<0 || (size_t)sid>=M.stmts.size()) return c2ir::kInvalidExpr;
  const auto& S = M.stmts[sid];
  using SK=c2ir::StmtKind;
  switch (S.kind) {
    case SK::Return:   return S.ret_expr;
    case SK::ExprStmt: return S.expr;
    case SK::Compound:
      for (auto s : S.items) {
        auto e = find_return_expr(M, s);
        if (e != c2ir::kInvalidExpr) return e;
      }
      return c2ir::kInvalidExpr;
    case SK::If: {
      return c2ir::kInvalidExpr;
    }
    default:
      return c2ir::kInvalidExpr;
  }
}

// ---------- Decls to srepr (functions as Lambda) ----------
inline std::string repr_decl_srepr(const c2ir::Module& M, const c2ir::Decl& D) {
  using DK=c2ir::DeclKind;
  if (D.kind==DK::Hole) return sym_symbol(std::string("HOLE:") + D.reason);
  if (D.kind==DK::Typedef) {
    std::ostringstream o;
    o << "Eq(" << sym_symbol(D.name) << ", " << repr_type(M, D.type) << ")";
    return o.str();
  }
  if (D.kind==DK::Var) {
    // No initializer in IR.decl; just expose the symbol with its type
    std::ostringstream o;
    o << "Symbol('" << D.name << "')";
    return o.str();
  }
  // Function
  std::ostringstream args;
  args << "(";
  for (size_t i=0;i<D.params.size();++i) {
    if (i) args << ", ";
    args << sym_symbol(D.params[i].name);
  }
  args << ")";
  // Extract a return expression if present
  std::string body = "Symbol('unit')";
  auto ret = find_return_expr(M, D.body);
  if (ret!=c2ir::kInvalidExpr) body = repr_expr(M, ret);
  else if (D.body==c2ir::kInvalidStmt) body = "Symbol('prototype')";
  std::ostringstream o;
  o << "Lambda(" << args.str() << ", " << body << ")";
  return o.str();
}

// ---------- Entry: write srepr JSON ----------
inline void write_module_srepr_json(const c2ir::Module& M, std::ostream& os) {
  os << '{';
  os << "\"ir_version\":"; write_escaped(os, M.ir_version); // reuse escape via ADL
  os << ",\"types\":[";
  for (size_t i=0;i<M.types.size();++i) {
    if (i) os << ',';
    write_escaped(os, repr_type(M, (c2ir::TypeId)i));
  }
  os << "],\"exprs\":[";
  for (size_t i=0;i<M.exprs.size();++i) {
    if (i) os << ',';
    write_escaped(os, repr_expr(M, (c2ir::ExprId)i));
  }
  os << "],\"decls\":[";
  for (size_t i=0;i<M.decls.size();++i) {
    const auto& D = M.decls[i];
    if (i) os << ',';
    os << '{';
    os << "\"name\":"; write_escaped(os, D.name);
    os << ",\"kind\":\"";
    switch (D.kind) {
      case c2ir::DeclKind::Var: os << "var"; break;
      case c2ir::DeclKind::Func: os << "func"; break;
      case c2ir::DeclKind::Typedef: os << "typedef"; break;
      case c2ir::DeclKind::Hole: os << "hole"; break;
    }
    os << "\",\"srepr\":";
    write_escaped(os, repr_decl_srepr(M, D));
    os << '}';
  }
  os << "]}";
}

} // namespace c2sympy

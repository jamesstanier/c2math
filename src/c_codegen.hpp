/*
 * Copyright (c) 2025 James G. Stanier
 *
 * This file is part of c2math.
 *
 * This software is dual-licensed under:
 *   1. The GNU General Public License v3.0 (GPLv3)
 *   2. A commercial license (contact j.stanier766(at)gmail.com for details)
 *
 * You may use this file under the terms of the GPLv3 as published by
 * the Free Software Foundation. For proprietary/commercial use,
 * please see the LICENSE-COMMERCIAL file or contact the copyright holder.
 */

 #pragma once
// FR-008: IR → C code generator (subset MVP)
// ------------------------------------------------------------
// Generates valid C11 source from your IR. This initial cut
// focuses on what FR-006/FR-007 already support today:
//   - Function decls/defs (DeclKind::Func)
//   - Bodies with Compound + Return
//   - Exprs: IntLit, FloatLit, VarRef, BinOp(Add/Sub/Mul/Div), Call
//
// Design goals per FR-008:
//  - Pretty-printed, deterministic output (stable ordering).
//  - Idempotent on IR where possible (format-only diffs OK). :contentReference[oaicite:1]{index=1}
//
// Extends in future FRs:
//  - FR-009: richer types (qualifiers, pointers, arrays, fn types).
//  - FR-010: full expression set (assignments, casts, ?:, etc.).
//  - FR-011: statements/control flow (if/switch/loops/decl-stmts).
//
// Usage:
//   #include "c_codegen.hpp"
//   c2c::Options co;
//   co.emit_prototypes = true;
//   c2c::write_module_c(M, std::cout, co);
//
// CLI note (per FR-017): a --emit=c switch is expected; you can wire it
// as an alias to the function below. :contentReference[oaicite:2]{index=2}

#include <cassert>
#include <cstddef>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include "ir.hpp"

namespace c2c {

struct Options {
  bool emit_prototypes = false;   // also emit prototypes before defs
  bool single_function = false;   // restrict output to one function
  std::string only_func;          // the function name when single_function=true
};

// ---------- small helpers for ID validity ----------
template <typename T>
static inline bool id_valid(const std::vector<T>& v, int id) {
  return id >= 0 && static_cast<std::size_t>(id) < v.size();
}

template <typename T>
static inline const T* try_get(const std::vector<T>& v, int id) {
  return id_valid(v, id) ? &v[static_cast<std::size_t>(id)] : nullptr;
}

// ---------- Type printing (MVP) ----------
static inline std::string type_to_c(const c2ir::Type& T) {
  using TK = c2ir::TypeKind;
  switch (T.kind) {
    case TK::Void:   return "void";
    case TK::Int:    return "int";
    case TK::Float:  return "float";
    case TK::Double: return "double";
    // TK::Function and others will be rendered via declarator contexts in later FRs.
    default:         return "int"; // safe default for MVP
  }
}

static inline std::string typeid_to_c(const c2ir::Module& M, c2ir::TypeId id) {
  if (!id_valid(M.types, id)) return "int";
  return type_to_c(M.types[static_cast<std::size_t>(id)]);
}

// ---------- Expression printing with precedence ----------
enum Prec : int {
  P_Primary = 100,   // literals, identifiers, calls
  P_Mul     = 80,    // * /
  P_Add     = 70,    // + -
  P_Top     = 0
};

static inline void write_expr_rec(const c2ir::Module& M, c2ir::ExprId id,
                                  std::ostream& os, int parent_prec);

static inline void write_call(const c2ir::Module& M, const c2ir::Expr& E,
                              std::ostream& os) {
  const c2ir::Expr* cal = try_get(M.exprs, E.callee);
  if (cal && cal->kind == c2ir::ExprKind::VarRef) {
    os << cal->str_val;
  } else {
    os << "(";
    write_expr_rec(M, E.callee, os, P_Primary);
    os << ")";
  }
  os << "(";
  for (std::size_t i = 0; i < E.args.size(); ++i) {
    if (i) os << ", ";
    write_expr_rec(M, E.args[i], os, P_Top);
  }
  os << ")";
}

static inline void write_float(std::ostream& os, double v) {
  std::ostringstream ss;
  ss.setf(std::ios::fmtflags(0), std::ios::floatfield);
  ss.precision(17);
  ss << v;
  os << ss.str();
}

static inline void write_binop(const c2ir::Module& M, const c2ir::Expr& E,
                               std::ostream& os, int parent_prec) {
  using B = c2ir::BinOp;
  const char* op = "?";
  int prec = P_Add;
  switch (E.binop) {
    case B::Add: op = "+"; prec = P_Add; break;
    case B::Sub: op = "-"; prec = P_Add; break;
    case B::Mul: op = "*"; prec = P_Mul; break;
    case B::Div: op = "/"; prec = P_Mul; break;
    default:     op = "/*op*/"; prec = P_Add; break;
  }
  const bool need_paren = prec < parent_prec;
  if (need_paren) os << "(";
  write_expr_rec(M, E.lhs, os, prec);
  os << " " << op << " ";
  write_expr_rec(M, E.rhs, os, prec + 1); // left-assoc: tighten rhs minimally
  if (need_paren) os << ")";
}

static inline void write_expr_rec(const c2ir::Module& M, c2ir::ExprId id,
                                  std::ostream& os, int parent_prec) {
  const c2ir::Expr* pE = try_get(M.exprs, id);
  if (!pE) { os << "/*HOLE:bad-expr-id*/"; return; }
  const c2ir::Expr& E = *pE;
  using EK = c2ir::ExprKind;
  switch (E.kind) {
    case EK::IntLit:   os << E.int_val; return;
    case EK::FloatLit: write_float(os, E.float_val); return;
    case EK::VarRef:   os << E.str_val; return;
    case EK::BinOp:    write_binop(M, E, os, parent_prec); return;
    case EK::Call:     write_call(M, E, os); return;
    default:           os << "/*HOLE:expr*/"; return;
  }
}

// ---------- Statement & function printing ----------
static inline void pad(std::ostream& os, int n) {
  for (int i = 0; i < n; ++i) os << ' ';
}

static inline void write_stmt(const c2ir::Module& M, c2ir::StmtId sid,
                              std::ostream& os, int indent = 0) {
  const c2ir::Stmt* pS = try_get(M.stmts, sid);
  if (!pS) { pad(os, indent); os << "/*HOLE:bad-stmt-id*/;\n"; return; }
  const c2ir::Stmt& S = *pS;
  using SK = c2ir::StmtKind;

  switch (S.kind) {
    case SK::Return:
      pad(os, indent); os << "return ";
      write_expr_rec(M, S.ret_expr, os, P_Top);
      os << ";\n";
      break;

    case SK::Compound:
      pad(os, indent); os << "{\n";
      for (auto child : S.items) {
        write_stmt(M, child, os, indent + 2);
      }
      pad(os, indent); os << "}\n";
      break;

    default:
      pad(os, indent); os << "/*HOLE:stmt*/;\n";
      break;
  }
}

static inline void write_func_decl(const c2ir::Module& M, const c2ir::Decl& D,
                                   std::ostream& os, bool with_body) {
  assert(D.kind == c2ir::DeclKind::Func);

  // Return type (default int if unknown)
  std::string ret = "int";
  if (id_valid(M.types, D.type)) {
    const auto& FT = M.types[static_cast<std::size_t>(D.type)];
    if (FT.kind == c2ir::TypeKind::Function && id_valid(M.types, FT.ret)) {
      ret = typeid_to_c(M, FT.ret);
    }
  }

  // Storage class by linkage
  if (D.linkage == c2ir::Linkage::Internal) os << "static ";

  // Signature
  os << ret << " " << D.name << "(";
  if (!D.params.empty()) {
    for (std::size_t i = 0; i < D.params.size(); ++i) {
      if (i) os << ", ";
      const auto& P = D.params[i];
      const std::string PT = typeid_to_c(M, P.type);
      os << PT << " " << P.name;
    }
  } else {
    os << "void";
  }
  os << ")";

  // Body or prototype
  if (with_body) {
    os << " ";
    if (id_valid(M.stmts, D.body)) {
      write_stmt(M, D.body, os, 0);
    } else {
      os << "{ /* no body */ }\n";
    }
  } else {
    os << ";\n";
  }
}

static inline void write_module_c(const c2ir::Module& M, std::ostream& os,
                                  const Options& opts = {}) {
  os << "/* Generated by c2math (FR-008) */\n";

  // (Optional) prototypes first
  if (opts.emit_prototypes) {
    for (const auto& D : M.decls) {
      if (D.kind != c2ir::DeclKind::Func) continue;
      if (opts.single_function && D.name != opts.only_func) continue;
      write_func_decl(M, D, os, /*with_body*/false);
    }
    os << "\n";
  }

  // Definitions
  for (const auto& D : M.decls) {
    if (D.kind != c2ir::DeclKind::Func) continue;
    if (opts.single_function && D.name != opts.only_func) continue;
    write_func_decl(M, D, os, /*with_body*/true);
    os << "\n";
  }
}

} // namespace c2c

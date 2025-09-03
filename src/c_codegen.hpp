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
// FR-008/FR-009: IR → C code generator (types, decls)
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
  bool emit_prototypes = false;
  bool single_function = false;
  std::string only_func;
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

// ---------- Type & declarator printing (FR-009: richer types) ----------
static inline std::string base_type_to_c(const c2ir::Type& T) {
  using TK = c2ir::TypeKind;
  switch (T.kind) {
    case TK::Void:   return "void";
    case TK::Bool:   return "bool";
    case TK::Char:   return "char";
    case TK::Int:    return "int";
    case TK::UInt:   return "unsigned int";
    case TK::Long:   return "long";
    case TK::ULong:  return "unsigned long";
    case TK::Float:  return "float";
    case TK::Double: return "double";
    case TK::Record: return (T.is_union ? std::string("union ") : std::string("struct ")) + (T.tag.empty()?std::string(""):T.tag);
    case TK::Enum:   return std::string("enum ") + (T.enum_tag.empty()?std::string(""):T.enum_tag);
    default:         return "int"; // fallback
  }
}

// Build a split declarator: base  left name right
// Example for "int (*fp)(float)": base="int", left="*", right="(float)"
struct DeclPieces {
  std::string base;
  std::string left;
  std::string right;
};

static inline c2ir::TypeKind get_kind(const c2ir::Module& M, c2ir::TypeId id){
  if (!id_valid(M.types, id)) return c2ir::TypeKind::Int;
  return M.types[static_cast<std::size_t>(id)].kind;
}

static inline void append_cv(std::string& s, uint8_t quals){
  if (quals & 1) { if(!s.empty()) s += ' '; s += "const"; }
  if (quals & 2) { if(!s.empty()) s += ' '; s += "volatile"; }
}

static inline DeclPieces build_decl_pieces(const c2ir::Module& M,
                                           c2ir::TypeId ty,
                                           const std::vector<c2ir::Param>* outer_params,
                                           bool is_outermost) {
  DeclPieces out;
  if (!id_valid(M.types, ty)) { out.base = "int"; return out; }
  const auto& T = M.types[static_cast<std::size_t>(ty)];
  using TK = c2ir::TypeKind;
  switch (T.kind) {
    case TK::Pointer: {
      auto inner = build_decl_pieces(M, T.pointee, outer_params, false);
      inner.left += "*";
      return inner;
    }
    case TK::Function: {
      // Start from return type pieces
      auto inner = build_decl_pieces(M, T.ret, outer_params, false);

      // Render parameter list (names only for outermost decl if provided)
      std::ostringstream ps;
      ps << "(";
      for (std::size_t i = 0; i < T.params.size(); ++i) {
        if (i) ps << ", ";
        const auto pty = T.params[i];
        std::string pname;
        if (is_outermost && outer_params && i < outer_params->size()) {
          pname = (*outer_params)[i].name;
        }
        // Recursively render each parameter declarator
        DeclPieces pp = build_decl_pieces(M, pty, nullptr, false);
        // Stitch param: base + ' ' + left+name+right (paren if needed)
        ps << pp.base;
        std::string mid = pp.left;
        if (!pname.empty()) {
          if (!mid.empty()) mid += pname;
          else               mid  = pname;
        }
        if (!mid.empty()) {
          if (!pp.left.empty() && !pp.right.empty() && pp.right.front() == '(') {
            ps << " (" << mid << ")";
          } else {
            ps << " " << mid;
          }
        }
        ps << pp.right;
      }
      if (T.varargs) {
        if (!T.params.empty()) ps << ", ";
        ps << "...";
      }
      ps << ")";
      inner.right += ps.str();
      return inner;
    }
    case TK::Qualified: {
      // Apply cv to either the pointer level (if present) or to the base token.
      auto inner = build_decl_pieces(M, T.base, outer_params, false);
      auto apply_cv = [&](uint8_t q, std::string& target) {
        if (q & 1) { target += (target.empty() || target.back()=='*' ? " const" : " const"); }
        if (q & 2) { target += (target.empty() || target.back()=='*' ? " volatile" : " volatile"); }
      };
      if (!inner.left.empty() && get_kind(M, T.base) == TK::Pointer) {
        apply_cv(T.quals, inner.left);     // e.g., int * const
      } else {
        std::string cv; append_cv(cv, T.quals);
        if (!cv.empty()) {
          if (inner.base.empty()) inner.base = cv;
          else inner.base = cv + " " + inner.base;  // e.g., const int *
        }
      }
      return inner;
    }
    case TK::Array: {
      auto inner = build_decl_pieces(M, T.elem, outer_params, false);
      std::ostringstream s;
      s << "[";
      if (T.count >= 0) s << T.count;     // flexible/incomplete => "[]"
      s << "]";
      inner.right += s.str();
      return inner;
    }
    default:
      out.base = base_type_to_c(T);
      return out;
  }
}

static inline std::string render_declarator(const c2ir::Module& M,
                                            c2ir::TypeId ty,
                                            const std::string& name,
                                            const std::vector<c2ir::Param>* outer_params = nullptr) {
  DeclPieces dp = build_decl_pieces(M, ty, outer_params, /*is_outermost*/true);
  std::ostringstream os;
  os << dp.base;
  std::string mid = dp.left + name;
  if (!mid.empty()) {
    const bool needs_paren = !dp.left.empty() && !dp.right.empty() &&
                             (dp.right.front() == '(' || dp.right.front() == '[');
    if (needs_paren) {
      os << " (" << mid << ")";
    } else {
      os << " " << mid;
    }
  }
  os << dp.right;
  return os.str();
}

// Back-compat helper for simple contexts that just need a leaf type name.
static inline std::string typeid_to_c(const c2ir::Module& M, c2ir::TypeId id) {
  if (!id_valid(M.types, id)) return "int";
  const auto& T = M.types[static_cast<std::size_t>(id)];
  return base_type_to_c(T);
}

// ---------- Expression printing with precedence ----------
enum Prec : int {
  P_Primary = 100,   // literals, identifiers, calls
  P_Mul     = 80,    // * /
  P_Add     = 70,    // + -
  P_Top     = 0
};

// Top-level typedefs and variables
static inline void write_typedef_decl(const c2ir::Module& M, const c2ir::Decl& D,
                                      std::ostream& os) {
  std::string decl = render_declarator(M, D.type, D.name);
  os << "typedef " << decl << ";\n";
}

static inline void write_var_decl_top(const c2ir::Module& M, const c2ir::Decl& D,
                                      std::ostream& os) {
  if (D.linkage == c2ir::Linkage::Internal) os << "static ";
  else if (D.linkage == c2ir::Linkage::External) os << "/*extern*/ ";
  std::string decl = render_declarator(M, D.type, D.name);
  os << decl << ";\n";
}

static inline void write_record_defn(const c2ir::Module& M, const c2ir::Type& T, std::ostream& os) {
  os << (T.is_union ? "union " : "struct ") << (T.tag.empty()? "" : T.tag) << " {\n";
  for (const auto& f : T.fields) {
    os << "  " << render_declarator(M, f.type, f.name);
    if (f.bit_width >= 0) os << " : " << f.bit_width;
    os << ";\n";
  }
  os << "};\n";
}
static inline void write_enum_defn(const c2ir::Module& M, const c2ir::Type& T, std::ostream& os) {
  os << "enum " << (T.enum_tag.empty()? "" : T.enum_tag) << " {\n";
  for (size_t i=0;i<T.enumerators.size();++i) {
    const auto& e = T.enumerators[i];
    os << "  " << e.name;
    if (e.has_value) os << " = " << e.value;
    if (i+1<T.enumerators.size()) os << ",";
    os << "\n";
  }
  os << "};\n";
}

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

  // Storage class by linkage
  if (D.linkage == c2ir::Linkage::Internal) os << "static ";

  // Full declarator from the function type (includes return type and params).
  std::string sig = render_declarator(M, D.type, D.name, &D.params);
  os << sig;

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
                                  const Options& opts) {
  os << "/* Generated by c2math (FR-008 + FR-009 types) */\n\n";

  // 0) Sorted enum/record definitions for deterministic output
  std::vector<const c2ir::Type*> enums, recs;
  for (const auto& T : M.types) {
    if (T.kind == c2ir::TypeKind::Enum && !T.enumerators.empty()) {
      enums.push_back(&T);
    } else if (T.kind == c2ir::TypeKind::Record && T.is_complete) {
      recs.push_back(&T);
    }
  }
  std::sort(enums.begin(), enums.end(),
            [](const c2ir::Type* a, const c2ir::Type* b){ return a->enum_tag < b->enum_tag; });
  std::sort(recs.begin(), recs.end(),
            [](const c2ir::Type* a, const c2ir::Type* b){ return a->tag < b->tag; });

  for (auto* T : enums) { write_enum_defn(M, *T, os); os << "\n"; }
  for (auto* T : recs)  { write_record_defn(M, *T, os); os << "\n"; }

  // 1) typedefs
  for (const auto& D : M.decls) {
    if (D.kind == c2ir::DeclKind::Typedef && !opts.single_function) write_typedef_decl(M, D, os);
  }
  if (!M.decls.empty()) os << "\n";

  // 2) top-level vars
  for (const auto& D : M.decls) {
    if (D.kind == c2ir::DeclKind::Var && !opts.single_function) write_var_decl_top(M, D, os);
  }
  if (!M.decls.empty()) os << "\n";

  // 3) optional prototypes
  if (opts.emit_prototypes) {
    for (const auto& D : M.decls) {
      if (D.kind == c2ir::DeclKind::Func && (!opts.single_function || D.name == opts.only_func)) {
        write_func_decl(M, D, os, /*with_body*/false);
      }
    }
    os << "\n";
  }

  // 4) defs
  for (const auto& D : M.decls) {
    if (D.kind == c2ir::DeclKind::Func && (!opts.single_function || D.name == opts.only_func)) {
      write_func_decl(M, D, os, /*with_body*/true);
      os << "\n";
    }
  }
}

} // namespace c2c

/*
 * Copyright (c) 2025 James G. Stanier
 *
 * This file is part of c2math.
 *
 * This software is dual-licensed under:
 *   1. The GNU General Public License v3.0 (GPLv3)
 *   2. A commercial license (contact j.stanier766@gmail.com for details)
 *
 * You may use this file under the terms of the GPLv3 as published by
 * the Free Software Foundation. For proprietary/commercial use,
 * please see the LICENSE-COMMERCIAL file or contact the copyright holder.
 */

#pragma once
#include <ostream>
#include <string>
#include <vector>
#include <cstdint>
#include "ir.hpp"

namespace c2json {

// ---------- tiny JSON helpers (header-only) ----------
inline void write_escaped(std::ostream& os, const std::string& s) {
  os << '"';
  for (unsigned char c : s) {
    switch (c) {
      case '\"': os << "\\\""; break;
      case '\\': os << "\\\\"; break;
      case '\b': os << "\\b";  break;
      case '\f': os << "\\f";  break;
      case '\n': os << "\\n";  break;
      case '\r': os << "\\r";  break;
      case '\t': os << "\\t";  break;
      default:
        if (c < 0x20) { // control chars → \u00XX
          static const char* hex = "0123456789abcdef";
          os << "\\u00" << hex[(c>>4)&0xF] << hex[c&0xF];
        } else {
          os << c;
        }
    }
  }
  os << '"';
}

inline const char* type_kind_str(c2ir::TypeKind k) {
  using TK = c2ir::TypeKind;
  switch (k) {
    case TK::Void: return "void";
    case TK::Bool: return "bool";
    case TK::Char: return "char";
    case TK::Int: return "int";
    case TK::UInt: return "uint";
    case TK::Long: return "long";
    case TK::ULong: return "ulong";
    case TK::Float: return "float";
    case TK::Double: return "double";
    case TK::Pointer: return "pointer";
    case TK::Function: return "function";
  }
  return "unknown";
}

inline const char* decl_kind_str(c2ir::DeclKind k) {
  using DK = c2ir::DeclKind;
  switch (k) {
    case DK::Var: return "var";
    case DK::Func: return "func";
    case DK::Typedef: return "typedef";
    case DK::Hole: return "hole";
  }
  return "unknown";
}

inline const char* stmt_kind_str(c2ir::StmtKind k) {
  using SK = c2ir::StmtKind;
  switch (k) {
    case SK::Compound: return "compound";
    case SK::Return: return "return";
    case SK::ExprStmt: return "expr_stmt";
    case SK::DeclStmt: return "decl_stmt";
    case SK::If: return "if";
    case SK::Hole: return "hole";
  }
  return "unknown";
}

inline const char* expr_kind_str(c2ir::ExprKind k) {
  using EK = c2ir::ExprKind;
  switch (k) {
    case EK::IntLit: return "int_lit";
    case EK::FloatLit: return "float_lit";
    case EK::StringLit: return "string_lit";
    case EK::VarRef: return "var_ref";
    case EK::BinOp: return "binop";
    case EK::Call: return "call";
    case EK::Hole: return "hole";
  }
  return "unknown";
}

inline const char* binop_str(c2ir::BinOp b) {
  using BO = c2ir::BinOp;
  switch (b) {
    case BO::Add: return "+";
    case BO::Sub: return "-";
    case BO::Mul: return "*";
    case BO::Div: return "/";
  }
  return "?";
}

inline const char* linkage_str(c2ir::Linkage l) {
  using L = c2ir::Linkage;
  switch (l) {
    case L::External: return "external";
    case L::Internal: return "internal";
    case L::None: return "none";
  }
  return "none";
}

// ---------- writers (deterministic: vector order == id order) ----------
inline void write_types(const c2ir::Module& M, std::ostream& os) {
  os << "\"types\":[";
  for (size_t i=0;i<M.types.size();++i) {
    const auto& t = M.types[i];
    if (i) os << ',';
    os << '{';
    os << "\"id\":" << (int)i
       << ",\"kind\":\"" << type_kind_str(t.kind) << '"';
    if (t.kind == c2ir::TypeKind::Pointer) {
      os << ",\"pointee\":" << t.pointee;
    } else if (t.kind == c2ir::TypeKind::Function) {
      os << ",\"ret\":" << t.ret << ",\"params\":[";
      for (size_t j=0;j<t.params.size();++j) {
        if (j) os << ',';
        os << t.params[j];
      }
      os << "],\"varargs\":" << (t.varargs ? "true":"false");
    }
    os << '}';
  }
  os << "]";
}

inline void write_decls(const c2ir::Module& M, std::ostream& os) {
  os << "\"decls\":[";
  for (size_t i=0;i<M.decls.size();++i) {
    const auto& d = M.decls[i];
    if (i) os << ',';
    os << '{';
    os << "\"id\":" << (int)i
       << ",\"kind\":\"" << decl_kind_str(d.kind) << '"';
    os << ",\"name\":"; write_escaped(os, d.name);
    if (d.kind != c2ir::DeclKind::Hole) {
      os << ",\"type\":" << d.type
         << ",\"linkage\":\"" << linkage_str(d.linkage) << '"';
    }
    if (d.kind == c2ir::DeclKind::Func) {
      os << ",\"params\":[";
      for (size_t j=0;j<d.params.size();++j) {
        if (j) os << ',';
        os << "{\"name\":"; write_escaped(os, d.params[j].name);
        os << ",\"type\":" << d.params[j].type << "}";
      }
      os << "],\"body\":";
      if (d.body == c2ir::kInvalidStmt) os << "null"; else os << d.body;
    }
    if (d.kind == c2ir::DeclKind::Hole) {
      os << ",\"reason\":"; write_escaped(os, d.reason);
    }
    os << '}';
  }
  os << "]";
}

inline void write_stmts(const c2ir::Module& M, std::ostream& os) {
  os << "\"stmts\":[";
  for (size_t i=0;i<M.stmts.size();++i) {
    const auto& s = M.stmts[i];
    if (i) os << ',';
    os << '{';
    os << "\"id\":" << (int)i
       << ",\"kind\":\"" << stmt_kind_str(s.kind) << '"';
    switch (s.kind) {
      case c2ir::StmtKind::Compound:
        os << ",\"items\":[";
        for (size_t j=0;j<s.items.size();++j) { if(j) os<<','; os<< s.items[j]; }
        os << "]";
        break;
      case c2ir::StmtKind::Return:
        os << ",\"ret\":" << s.ret_expr;
        break;
      case c2ir::StmtKind::ExprStmt:
        os << ",\"expr\":" << s.expr;
        break;
      case c2ir::StmtKind::DeclStmt:
        os << ",\"name\":"; write_escaped(os, s.decl_name);
        os << ",\"type\":" << s.decl_type;
        os << ",\"init\":" << (s.init==c2ir::kInvalidExpr ? -1 : s.init);
        break;
      case c2ir::StmtKind::If:
        os << ",\"cond\":" << s.cond
           << ",\"then\":" << (s.then_branch==c2ir::kInvalidStmt? -1 : s.then_branch)
           << ",\"else\":" << (s.else_branch==c2ir::kInvalidStmt? -1 : s.else_branch);
        break;
      case c2ir::StmtKind::Hole:
        os << ",\"reason\":"; write_escaped(os, s.reason);
        break;
    }
    os << '}';
  }
  os << "]";
}

inline void write_exprs(const c2ir::Module& M, std::ostream& os) {
  os << "\"exprs\":[";
  for (size_t i=0;i<M.exprs.size();++i) {
    const auto& e = M.exprs[i];
    if (i) os << ',';
    os << '{';
    os << "\"id\":" << (int)i
       << ",\"kind\":\"" << expr_kind_str(e.kind) << '"'
       << ",\"type\":" << e.ty;
    switch (e.kind) {
      case c2ir::ExprKind::IntLit:
        os << ",\"value\":" << e.int_val; break;
      case c2ir::ExprKind::FloatLit:
        os << ",\"value\":" << e.float_val; break;
      case c2ir::ExprKind::StringLit:
      case c2ir::ExprKind::VarRef:
      case c2ir::ExprKind::Hole:
        os << ",\"value\":"; write_escaped(os, e.str_val);
        if (e.kind==c2ir::ExprKind::VarRef) os << ",\"sym\":" << e.sym_id;
        break;
      case c2ir::ExprKind::BinOp:
        os << ",\"op\":"; write_escaped(os, binop_str(e.binop));
        os << ",\"lhs\":" << e.lhs << ",\"rhs\":" << e.rhs; break;
      case c2ir::ExprKind::Call:
        os << ",\"callee\":" << e.callee << ",\"args\":[";
        for (size_t j=0;j<e.args.size();++j) { if(j) os<<','; os<< e.args[j]; }
        os << "]"; break;
    }
    os << '}';
  }
  os << "]";
}

// ---------- entry point ----------
inline void write_module_json(const c2ir::Module& M, std::ostream& os) {
  // Deterministic ordering: fields in fixed order; vectors serialized by ID order (index).
  os << '{';
  os << "\"ir_version\":"; write_escaped(os, M.ir_version); os << ',';
  write_types(M, os); os << ',';
  write_decls(M, os); os << ',';
  write_stmts(M, os); os << ',';
  write_exprs(M, os);
  os << '}';
}

} // namespace c2json

#pragma once
#include <string>
#include <vector>
#include <optional>
#include <cstdint>
#include <utility>
#include <cassert>
#include <iostream>

namespace c2ir {

// ---- IR version (stable prefix for FR-004/005) ------------------------------
inline constexpr const char* IR_VERSION = "1.0"; // schema v1.0  :contentReference[oaicite:1]{index=1}

// ---- Basic indexed handles ---------------------------------------------------
using TypeId = int32_t;
using ExprId = int32_t;
using StmtId = int32_t;
using DeclId = int32_t;

static inline constexpr TypeId  kInvalidType = -1;
static inline constexpr ExprId  kInvalidExpr = -1;
static inline constexpr StmtId  kInvalidStmt = -1;
static inline constexpr DeclId  kInvalidDecl = -1;

// ---- Types -------------------------------------------------------------------
enum class TypeKind {
  Void, Bool, Char, Int, UInt, Long, ULong, Float, Double,
  Pointer, Function
};

struct Type {
  TypeKind kind{};
  // For Pointer:
  TypeId pointee = kInvalidType;
  // For Function:
  TypeId ret = kInvalidType;
  std::vector<TypeId> params;
  bool varargs = false;
};

// ---- Expressions (subset; holes allowed per FR-016) --------------------------
enum class BinOp { Add, Sub, Mul, Div };

enum class ExprKind {
  IntLit, FloatLit, StringLit,
  VarRef,
  BinOp, Call,
  Hole
};

struct Expr {
  ExprKind kind{};
  TypeId ty = kInvalidType;

  // payloads
  long long int_val = 0;
  double float_val = 0.0;
  std::string str_val;            // StringLit, VarRef (name), Hole(reason)
  BinOp binop{};
  ExprId lhs = kInvalidExpr, rhs = kInvalidExpr;    // BinOp
  ExprId callee = kInvalidExpr;                     // Call
  std::vector<ExprId> args;                         // Call
  // Optionally, link back to a symbol-table id if you want (FR-002)
  int32_t sym_id = -1; // for VarRef, optional
};

// ---- Statements --------------------------------------------------------------
enum class StmtKind {
  Compound, Return, ExprStmt, DeclStmt, If, Hole
};

struct Stmt {
  StmtKind kind{};
  // Compound:
  std::vector<StmtId> items;
  // Return:
  ExprId ret_expr = kInvalidExpr;
  // ExprStmt:
  ExprId expr = kInvalidExpr;
  // DeclStmt (single var for simplicity here):
  std::string decl_name;
  TypeId decl_type = kInvalidType;
  ExprId init = kInvalidExpr;   // may be kInvalidExpr
  // If:
  ExprId cond = kInvalidExpr;
  StmtId then_branch = kInvalidStmt;
  StmtId else_branch = kInvalidStmt; // kInvalidStmt if none
  // Hole:
  std::string reason;
};

// ---- Declarations ------------------------------------------------------------
enum class Linkage { External, Internal, None }; // from FR-002 mapping

enum class DeclKind {
  Var, Func, Typedef, Hole
};

struct Param {
  std::string name;
  TypeId type = kInvalidType;
};

struct Decl {
  DeclKind kind{};
  Linkage linkage = Linkage::None;
  std::string name;
  TypeId type = kInvalidType;      // Var: object type; Func: function type; Typedef: aliased type
  // Func:
  std::vector<Param> params;
  StmtId body = kInvalidStmt;      // kInvalidStmt for prototypes
  // Typedef:
  // use 'type' as the aliased target; 'name' is the typedef name
  // Hole:
  std::string reason;
};

// ---- Module container --------------------------------------------------------
struct Module {
  std::string ir_version = IR_VERSION;
  std::vector<Type> types;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<Decl> decls;

  // factories (return stable IDs)
  TypeId make(const Type& t)   { types.push_back(t);  return (TypeId)types.size()-1; }
  ExprId make(const Expr& e)   { exprs.push_back(e);  return (ExprId)exprs.size()-1; }
  StmtId make(const Stmt& s)   { stmts.push_back(s);  return (StmtId)stmts.size()-1; }
  DeclId make(const Decl& d)   { decls.push_back(d);  return (DeclId)decls.size()-1; }

  // convenience creators
  TypeId makeBuiltin(TypeKind k) {
    Type t; t.kind = k; return make(t);
  }

  // debugging-only dumper (FR-003 visibility; FR-004 will add JSON)  :contentReference[oaicite:2]{index=2}
  void dump(std::ostream& os) const {
    os << "IR Module v" << ir_version << "\n";
    os << "Types(" << types.size() << ")\n";
    for (size_t i=0;i<types.size();++i) {
      const auto& t = types[i];
      os << "  t#" << i << ": ";
      switch (t.kind) {
        case TypeKind::Void: os << "void"; break;
        case TypeKind::Bool: os << "bool"; break;
        case TypeKind::Char: os << "char"; break;
        case TypeKind::Int: os << "int"; break;
        case TypeKind::UInt: os << "uint"; break;
        case TypeKind::Long: os << "long"; break;
        case TypeKind::ULong: os << "ulong"; break;
        case TypeKind::Float: os << "float"; break;
        case TypeKind::Double: os << "double"; break;
        case TypeKind::Pointer: os << "ptr(t#" << t.pointee << ")"; break;
        case TypeKind::Function:
          os << "fn(ret=t#" << t.ret << ", params=[";
          for (size_t j=0;j<t.params.size();++j) { if (j) os << ","; os << "t#" << t.params[j]; }
          os << "], varargs=" << (t.varargs?"true":"false") << ")";
          break;
      }
      os << "\n";
    }
    os << "Decls(" << decls.size() << ")\n";
    for (size_t i=0;i<decls.size();++i) {
      const auto& d = decls[i];
      os << "  d#" << i << ": ";
      switch (d.kind) {
        case DeclKind::Var:
          os << "var " << d.name << " : t#" << d.type << " link=" << (int)d.linkage;
          break;
        case DeclKind::Func:
          os << "func " << d.name << " : t#" << d.type << " params=[";
          for (size_t j=0;j<d.params.size();++j) { if (j) os << ","; os << d.params[j].name << ":t#" << d.params[j].type; }
          os << "] body=s#" << d.body;
          break;
        case DeclKind::Typedef:
          os << "typedef " << d.name << " = t#" << d.type;
          break;
        case DeclKind::Hole:
          os << "hole '" << d.reason << "'";
          break;
      }
      os << "\n";
    }
    os << "Stmts(" << stmts.size() << ")\n";
    for (size_t i=0;i<stmts.size();++i) {
      const auto& s = stmts[i];
      os << "  s#" << i << ": ";
      switch (s.kind) {
        case StmtKind::Compound:
          os << "{ "; for (size_t j=0;j<s.items.size();++j){ if(j) os<<", "; os<<"s#"<<s.items[j]; } os<<" }"; break;
        case StmtKind::Return: os << "return e#" << s.ret_expr; break;
        case StmtKind::ExprStmt: os << "expr e#" << s.expr; break;
        case StmtKind::DeclStmt: os << "decl " << s.decl_name << ":t#" << s.decl_type << " init=e#" << s.init; break;
        case StmtKind::If: os << "if(e#" << s.cond << ") then s#" << s.then_branch << " else s#" << s.else_branch; break;
        case StmtKind::Hole: os << "hole '" << s.reason << "'"; break;
      }
      os << "\n";
    }
    os << "Exprs(" << exprs.size() << ")\n";
    for (size_t i=0;i<exprs.size();++i) {
      const auto& e = exprs[i];
      os << "  e#" << i << ": ";
      switch (e.kind) {
        case ExprKind::IntLit: os << "int " << e.int_val; break;
        case ExprKind::FloatLit: os << "float " << e.float_val; break;
        case ExprKind::StringLit: os << "string \"" << e.str_val << "\""; break;
        case ExprKind::VarRef: os << "var " << e.str_val << " (sym="<< e.sym_id <<")"; break;
        case ExprKind::BinOp:
          os << "binop(" << (e.binop==BinOp::Add?"+":e.binop==BinOp::Sub?"-":e.binop==BinOp::Mul?"*":"/")
             << ", e#" << e.lhs << ", e#" << e.rhs << ")";
          break;
        case ExprKind::Call:
          os << "call callee=e#" << e.callee << " args=[";
          for (size_t j=0;j<e.args.size();++j){ if(j) os<<","; os<<"e#"<<e.args[j]; }
          os << "]";
          break;
        case ExprKind::Hole: os << "hole '" << e.str_val << "'"; break;
      }
      os << " : t#" << e.ty << "\n";
    }
  }
};

} // namespace c2ir

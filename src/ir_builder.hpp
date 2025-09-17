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
#include "ir.hpp"
#include "symbol_table.hpp"

#include "clang/AST/AST.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Linkage.h"
#include "llvm/Support/Casting.h"

namespace c2ir {

class IRBuilder : public clang::RecursiveASTVisitor<IRBuilder> {
public:
  IRBuilder(clang::ASTContext& Ctx, c2ast::SymTab* ST, Module& M)
    : Ctx(Ctx), ST(ST), M(M) {
    // Pre-create common builtin types
    tVoid  = M.makeBuiltin(TypeKind::Void);
    tInt   = M.makeBuiltin(TypeKind::Int);
    tChar  = M.makeBuiltin(TypeKind::Char);
    tFloat = M.makeBuiltin(TypeKind::Float);
    tDouble= M.makeBuiltin(TypeKind::Double);
    tBool  = M.makeBuiltin(TypeKind::Bool);
  }

  bool lower(clang::TranslationUnitDecl* TU) {
    return TraverseDecl(TU);
  }

  // ---- Decls (top-level) ----------------------------------------------------
  bool VisitVarDecl(clang::VarDecl* VD) {
    if (!VD->isFileVarDecl()) return true; // locals handled in DeclStmt

    Decl D;
    D.kind = DeclKind::Var;
    D.name = VD->getNameAsString();
    D.linkage = mapLinkage(VD);
    D.type = mapType(VD->getType());

    if (const clang::Expr* Init = VD->getAnyInitializer()) {
      // We map the expr to ensure it’s valid IR, but we don’t attach it to Decl v1.
      (void)mapExpr(Init);
    }
    M.make(D);
    return true;
  }

  bool VisitTypedefNameDecl(clang::TypedefNameDecl* TD) {
    Decl D;
    D.kind = DeclKind::Typedef;
    D.name = TD->getNameAsString();
    D.type = mapType(TD->getUnderlyingType());
    M.make(D);
    return true;
  }

  // Ensure standalone tag definitions are captured even if never referenced.
  bool VisitRecordDecl(clang::RecordDecl* RD) {
    // Only materialize complete definitions (skip forward decls).
    if (!RD->isCompleteDefinition()) return true;
    // Map this tag into a Type (with fields/bitfields).
    clang::QualType QT(RD->getTypeForDecl(), 0);
    (void)mapType(QT);  // inserts a Record type into M.types
    return true;
  }

  bool VisitEnumDecl(clang::EnumDecl* ED) {
    // Enums are “complete” when defined; still harmless to gate.
    if (!ED->isCompleteDefinition()) return true;
    clang::QualType QT(ED->getTypeForDecl(), 0);
    (void)mapType(QT);  // inserts an Enum type into M.types
    return true;
  }

  bool TraverseFunctionDecl(clang::FunctionDecl* FD) {
    // Create func decl
    Decl D;
    D.kind = DeclKind::Func;
    D.name = FD->getNameAsString();
    D.linkage = mapLinkage(FD);
    D.type = mapType(FD->getType()); // function type

    // Params
    for (clang::ParmVarDecl* P : FD->parameters()) {
      Param p; p.name = P->getNameAsString(); p.type = mapType(P->getType());
      D.params.push_back(p);
    }

    // Body (if definition)
    if (FD->hasBody()) {
      const clang::Stmt* Body = FD->getBody();
      StmtId bodyId = mapStmt(Body);
      D.body = bodyId;
    }

    M.make(D);
    // Recurse so children are visited as usual
    return clang::RecursiveASTVisitor<IRBuilder>::TraverseFunctionDecl(FD);
  }

  // ---- Stmts ----------------------------------------------------------------
  StmtId mapStmt(const clang::Stmt* S) {
    if (!S) return kInvalidStmt;

    if (const auto* CS = llvm::dyn_cast<clang::CompoundStmt>(S)) {
      Stmt out; out.kind = StmtKind::Compound;
      for (const clang::Stmt* child : CS->body()) {
        out.items.push_back(mapStmt(child));
      }
      return M.make(out);
    }

    if (const auto* RS = llvm::dyn_cast<clang::ReturnStmt>(S)) {
      Stmt out; out.kind = StmtKind::Return;
      out.ret_expr = mapExpr(RS->getRetValue());
      return M.make(out);
    }

    if (const auto* DS = llvm::dyn_cast<clang::DeclStmt>(S)) {
      // Emit one DeclStmt per VarDecl; return the last id
      StmtId lastId = kInvalidStmt;
      for (auto it = DS->decl_begin(); it != DS->decl_end(); ++it) {
        if (const auto* VD = llvm::dyn_cast<clang::VarDecl>(*it)) {
          Stmt d; d.kind = StmtKind::DeclStmt;
          d.decl_name = VD->getNameAsString();
          d.decl_type = mapType(VD->getType());
          d.init = VD->hasInit() ? mapExpr(VD->getInit()) : kInvalidExpr;
          lastId = M.make(d);
        }
      }
      if (lastId == kInvalidStmt) {
        Stmt hole; hole.kind = StmtKind::Hole; hole.reason = "declstmt_nonvar";
        return M.make(hole);
      }
      return lastId;
    }

    if (const auto* IS = llvm::dyn_cast<clang::IfStmt>(S)) {
      Stmt out; out.kind = StmtKind::If;
      out.cond = mapExpr(IS->getCond());
      out.then_branch = mapStmt(IS->getThen());
      out.else_branch = mapStmt(IS->getElse());
      return M.make(out);
    }

    // Generic expression statement
    if (llvm::isa<clang::Expr>(S)) {
      const auto* ES = llvm::cast<clang::Expr>(S);
      Stmt out; out.kind = StmtKind::ExprStmt;
      out.expr = mapExpr(ES);
      return M.make(out);
    }

    // Unsupported stmt → hole (FR-016)
    Stmt hole; hole.kind = StmtKind::Hole; hole.reason = "stmt_unsupported";
    return M.make(hole);
  }

  // ---- Exprs ----------------------------------------------------------------
  ExprId mapExpr(const clang::Expr* E) {
    if (!E) return kInvalidExpr;

    // strip implicit casts and parens
    const clang::Expr* X = E->IgnoreParenImpCasts();

    if (const auto* IL = llvm::dyn_cast<clang::IntegerLiteral>(X)) {
      Expr out; out.kind = ExprKind::IntLit; out.int_val = (long long)IL->getValue().getSExtValue();
      out.ty = mapType(X->getType());
      return M.make(out);
    }

    if (const auto* FL = llvm::dyn_cast<clang::FloatingLiteral>(X)) {
      Expr out; out.kind = ExprKind::FloatLit; out.float_val = FL->getValueAsApproximateDouble();
      out.ty = mapType(X->getType());
      return M.make(out);
    }

    if (const auto* SL = llvm::dyn_cast<clang::StringLiteral>(X)) {
      Expr out; out.kind = ExprKind::StringLit; out.str_val = SL->getString().str();
      out.ty = mapType(X->getType());
      return M.make(out);
    }

    if (const auto* DRE = llvm::dyn_cast<clang::DeclRefExpr>(X)) {
      Expr out; out.kind = ExprKind::VarRef; out.str_val = DRE->getNameInfo().getAsString();
      out.ty = mapType(X->getType());

      // Try to attach symbol id from FR-002 (optional, best-effort)
      if (ST) {
        for (const auto& u : ST->uses) {
          if (u.spelled == out.str_val) { out.sym_id = (int)u.resolved_symbol_id; break; }
        }
      }
      return M.make(out);
    }

    if (const auto* BO = llvm::dyn_cast<clang::BinaryOperator>(X)) {
      if (BO->isAdditiveOp() || BO->isMultiplicativeOp()) {
        Expr out; out.kind = ExprKind::BinOp;
        switch (BO->getOpcode()) {
          case clang::BO_Add: out.binop = BinOp::Add; break;
          case clang::BO_Sub: out.binop = BinOp::Sub; break;
          case clang::BO_Mul: out.binop = BinOp::Mul; break;
          default:            out.binop = BinOp::Div; break;
        }
        out.lhs = mapExpr(BO->getLHS());
        out.rhs = mapExpr(BO->getRHS());
        out.ty = mapType(X->getType());
        return M.make(out);
      }
      // Assignment & other side-effecting ops → represent as holes for v1
      Expr h; h.kind = ExprKind::Hole; h.str_val = "assign_or_side_effect_expr";
      h.ty = mapType(X->getType());
      return M.make(h);
    }

    if (const auto* CE = llvm::dyn_cast<clang::CallExpr>(X)) {
      Expr out; out.kind = ExprKind::Call;
      out.callee = mapExpr(CE->getCallee());
      for (const clang::Expr* arg : CE->arguments()) out.args.push_back(mapExpr(arg));
      out.ty = mapType(X->getType());
      return M.make(out);
    }

    // MemberExpr, designated inits, etc. → hole for now (FR-013 later)
    Expr h; h.kind = ExprKind::Hole; h.str_val = "expr_unsupported";
    h.ty = mapType(X->getType());
    return M.make(h);
  }

private:
  clang::ASTContext& Ctx;
  c2ast::SymTab* ST; // may be null
  Module& M;

  // cached builtin types
  TypeId tVoid{}, tInt{}, tChar{}, tFloat{}, tDouble{}, tBool{};

  // ---- Type mapper (subset; extend per FR-009)
  TypeId mapType(clang::QualType QT) {
    // 1) Peel off top-level cv qualifiers
    bool topConst    = QT.isConstQualified();
    bool topVolatile = QT.isVolatileQualified();
    clang::QualType Unqual = QT.getUnqualifiedType();

    // 2) Enums must be handled *before* canonicalization (so enum tag isn’t lost)
    if (Unqual->isEnumeralType()) {
      const auto* ET = Unqual->getAs<clang::EnumType>();
      const auto* ED = ET->getDecl();
      Type T;
      T.kind = TypeKind::Enum;
      T.enum_tag = ED->getNameAsString();
      if (!ED->getIntegerType().isNull()) {
        T.enum_underlying = mapType(ED->getIntegerType());
      }
      for (const auto* ECD : ED->enumerators()) {
        Type::Enumerator en;
        en.name = ECD->getNameAsString();
        en.value = (long long)ECD->getInitVal().getSExtValue();
        en.has_value = true;
        T.enumerators.push_back(en);
      }
      TypeId inner = M.make(T);
      if (topConst || topVolatile) {
        Type Q;
        Q.kind = TypeKind::Qualified;
        Q.base = inner;
        Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
        return M.make(Q);
      }
      return inner;
    }

    // 3) Now canonicalize the unqualified type
    QT = Unqual.getCanonicalType();

    // 4) Map other kinds (array, pointer, function, record)
    // Array
    if (QT->isArrayType()) {
      const auto* AT = llvm::dyn_cast<clang::ArrayType>(QT.getTypePtr());
      Type T;
      T.kind = TypeKind::Array;
      T.elem = mapType(AT->getElementType()); // recurse, child quals handled in recursive calls
      if (const auto* CAT = llvm::dyn_cast<clang::ConstantArrayType>(AT)) {
        T.count = (long long)CAT->getSize().getSExtValue();
      } else {
        T.count = -1;  // flexible / incomplete
      }
      TypeId inner = M.make(T);
      if (topConst || topVolatile) {
        Type Q; Q.kind = TypeKind::Qualified; Q.base = inner;
        Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
        return M.make(Q);
      }
      return inner;
    }

    // Pointer
    if (QT->isPointerType()) {
      Type T;
      T.kind = TypeKind::Pointer;
      T.pointee = mapType(QT->getPointeeType());
      TypeId inner = M.make(T);
      if (topConst || topVolatile) {
        Type Q; Q.kind = TypeKind::Qualified; Q.base = inner;
        Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
        return M.make(Q);
      }
      return inner;
    }

    // Function
    if (const clang::FunctionType* FT = QT->getAs<clang::FunctionType>()) {
      const auto* FPT = llvm::dyn_cast<clang::FunctionProtoType>(FT);
      Type T;
      T.kind = TypeKind::Function;
      T.ret = mapType(FT->getReturnType());
      if (FPT) {
        for (clang::QualType P : FPT->param_types()) {
          T.params.push_back(mapType(P));
        }
        T.varargs = FPT->isVariadic();
      }
      TypeId inner = M.make(T);
      if (topConst || topVolatile) {
        Type Q; Q.kind = TypeKind::Qualified; Q.base = inner;
        Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
        return M.make(Q);
      }
      return inner;
    }

    // Record (struct / union)
    if (QT->isRecordType()) {
      const auto* RT = QT->getAs<clang::RecordType>();
      const auto* RD = RT->getDecl();
      Type T;
      T.kind = TypeKind::Record;
      T.is_union = RD->isUnion();
      T.tag = RD->getNameAsString();
      T.is_complete = RD->isCompleteDefinition();
      if (T.is_complete) {
        for (const auto* Fld : RD->fields()) {
          Type::Field f;
          f.name = Fld->getNameAsString();
          f.type = mapType(Fld->getType());
          if (Fld->isBitField())
            f.bit_width = Fld->getBitWidthValue(Ctx);
          T.fields.push_back(f);
        }
      }
      TypeId inner = M.make(T);
      if (topConst || topVolatile) {
        Type Q; Q.kind = TypeKind::Qualified; Q.base = inner;
        Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
        return M.make(Q);
      }
      return inner;
    }

    // 5) Builtins fallback
    TypeId inner;
    if (QT->isVoidType()) inner = tVoid;
    else if (QT->isBooleanType()) inner = tBool;
    else if (QT->isCharType()) inner = tChar;
    else if (QT->isIntegerType()) {
      if (QT->isUnsignedIntegerType()) inner = M.makeBuiltin(TypeKind::UInt);
      else inner = tInt;
    }
    else if (QT->isSpecificBuiltinType(clang::BuiltinType::Float)) inner = tFloat;
    else if (QT->isSpecificBuiltinType(clang::BuiltinType::Double)) inner = tDouble;
    else inner = tInt;

    if (topConst || topVolatile) {
      Type Q;
      Q.kind = TypeKind::Qualified;
      Q.base = inner;
      Q.quals = (topConst ? 1 : 0) | (topVolatile ? 2 : 0);
      return M.make(Q);
    }
    return inner;
  }

  // Map Clang linkage → our IR linkage using modern API (works across versions)
  Linkage mapLinkage(const clang::Decl* D) const {
    const auto* ND = llvm::dyn_cast<clang::NamedDecl>(D);
    clang::Linkage L = ND ? ND->getFormalLinkage() : clang::Linkage::None;

    if (clang::isExternallyVisible(L)) return Linkage::External;
    if (L == clang::Linkage::None)     return Linkage::None;
    return Linkage::Internal;
  }
};

} // namespace c2ir

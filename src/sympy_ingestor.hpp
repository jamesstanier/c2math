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
// FR-007: SymPy srepr JSON → IR ingestor
// - No Python required. Parses the subset of srepr emitted by FR-006.
// - Builds functions as DeclKind::Func with a single Return statement body.
//
// Supported srepr heads: Integer, Float, Symbol, Add, Mul, Pow,
//                        Function('name')(args...), Lambda(...).
//
// This is intentionally conservative: it assumes all params/returns are `int`
// unless you extend your srepr JSON with extra type metadata.
//
// Fits the current IR in src/ir.hpp (Type/Expr/Stmt/Decl + Module::make).

#include <cctype>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>
#include <optional>
#include <istream>
#include <sstream>
#include <limits>

#include "ir.hpp"

namespace c2sympy_ingest {

// ---------- srepr tokeniser ----------
struct Tok {
  enum Kind { Id, LParen, RParen, Comma, String, Integer, Float, End } kind;
  std::string text;
  size_t pos = 0;
};
struct Lex {
  std::string s;
  size_t i = 0;
  explicit Lex(std::string str) : s(std::move(str)) {}
  static bool isIdStart(char c){ return std::isalpha((unsigned char)c) || c=='_'; }
  static bool isId(char c){ return std::isalnum((unsigned char)c) || c=='_'; }
  void skipws(){ while (i < s.size() && std::isspace((unsigned char)s[i])) ++i; }
  bool eof() const { return i >= s.size(); }
  char peek() const { return eof() ? '\0' : s[i]; }
  char get() { return eof() ? '\0' : s[i++]; }

  std::optional<std::string> read_quoted(char q){
    std::string out;
    while (!eof()){
      char c = get();
      if (c == q) return out;
      if (c == '\\'){
        if (eof()) return std::nullopt;
        char n = get();
        switch(n){
          case 'n': out.push_back('\n'); break;
          case 'r': out.push_back('\r'); break;
          case 't': out.push_back('\t'); break;
          case '\\': out.push_back('\\'); break;
          case '\'': out.push_back('\''); break;
          case '"': out.push_back('"'); break;
          default: out.push_back(n); break;
        }
      } else out.push_back(c);
    }
    return std::nullopt;
  }

  Tok next(){
    skipws();
    if (eof()) return {Tok::End, "", i};
    char c = peek();
    if (c=='('){ ++i; return {Tok::LParen, "(", i-1}; }
    if (c==')'){ ++i; return {Tok::RParen, ")", i-1}; }
    if (c==','){ ++i; return {Tok::Comma, ",", i-1}; }
    if (c=='\'' || c=='"'){
      char q = get();
      auto so = read_quoted(q);
      if (!so) return {Tok::End, "", i};
      return {Tok::String, *so, i};
    }
    // float or int literal (accept simple forms only; optional leading '-')
    if (c== '-' || std::isdigit((unsigned char)c)){
      size_t j = i;
      if (s[j] == '-') ++j;  // <-- allow a leading minus
      bool dot = false;
      while (j < s.size()){
        char d = s[j];
        if (std::isdigit((unsigned char)d)){ ++j; continue; }
        if (d=='.' && !dot){ dot = true; ++j; continue; }
        break;
      }
      std::string t = s.substr(i, j-i);
      i = j;
      return { dot ? Tok::Float : Tok::Integer, t, j };
    }
    if (isIdStart(c)){
      size_t j = i+1;
      while (j < s.size() && isId(s[j])) ++j;
      std::string t = s.substr(i, j-i);
      i = j;
      return {Tok::Id, t, j};
    }
    // unknown char → skip
    ++i;
    return next();
  }
};

// ---------- srepr AST ----------
struct SNode {
  enum K { K_Int, K_Float, K_Symbol, K_Add, K_Mul, K_Pow, K_Call, K_Lambda, K_Unknown } k;
  std::string name;           // Symbol/Function name
  std::vector<SNode> xs;      // children
};

struct Parser {
  Lex L;
  Tok t;
  explicit Parser(std::string s) : L(std::move(s)) { t = L.next(); }

  bool accept(Tok::Kind k){ if (t.kind==k){ t = L.next(); return true; } return false; }
  bool expect(Tok::Kind k){ if (!accept(k)) return false; return true; }

  std::optional<SNode> parse(){
    auto n = head();
    return n;
  }

  std::optional<SNode> head(){
    if (t.kind != Tok::Id) return std::nullopt;
    std::string tag = t.text; accept(Tok::Id);
    if (!expect(Tok::LParen)) return std::nullopt;

    auto read_args = [&]() -> std::vector<SNode>{
      std::vector<SNode> out;
      if (t.kind != Tok::RParen){
        while (true){
          auto a = head();
          if (!a) return {};
          out.push_back(*a);
          if (accept(Tok::Comma)) continue;
          break;
        }
      }
      expect(Tok::RParen);
      return out;
    };

    if (tag=="Integer"){
      if (t.kind!=Tok::Integer) return std::nullopt;
      SNode n; n.k=SNode::K_Int; n.name=t.text; accept(Tok::Integer); expect(Tok::RParen); return n;
    }
    if (tag=="Float"){
      if (!(t.kind==Tok::Float || t.kind==Tok::Integer)) return std::nullopt;
      SNode n; n.k=SNode::K_Float; n.name=t.text; accept(t.kind); expect(Tok::RParen); return n;
    }
    if (tag=="Symbol"){
      if (t.kind!=Tok::String) return std::nullopt;
      SNode n; n.k=SNode::K_Symbol; n.name=t.text; accept(Tok::String); expect(Tok::RParen); return n;
    }
    if (tag=="Function"){
      if (t.kind!=Tok::String) return std::nullopt;
      std::string nm = t.text; accept(Tok::String); expect(Tok::RParen);
      // Immediate call form: Function('f')(args...)
      if (t.kind==Tok::LParen){
        accept(Tok::LParen);
        std::vector<SNode> args;
        if (t.kind!=Tok::RParen){
          while (true){
            auto a = head(); if (!a) return std::nullopt;
            args.push_back(*a);
            if (accept(Tok::Comma)) continue;
            break;
          }
        }
        expect(Tok::RParen);
        SNode n; n.k=SNode::K_Call; n.name=nm; n.xs=std::move(args); return n;
      }
      SNode n; n.k=SNode::K_Symbol; n.name=nm; return n; // bare Function becomes Symbol
    }
    if (tag=="Add" || tag=="Mul"){
      auto args = read_args();
      SNode n; n.k = (tag=="Add")?SNode::K_Add:SNode::K_Mul; n.xs = std::move(args); return n;
    }
    if (tag=="Pow"){
      auto a = head(); if (!a) return std::nullopt;
      expect(Tok::Comma);
      auto b = head(); if (!b) return std::nullopt;
      expect(Tok::RParen);
      SNode n; n.k=SNode::K_Pow; n.xs.push_back(*a); n.xs.push_back(*b); return n;
    }
    if (tag=="Lambda"){
      // params: (Symbol('a'), Symbol('b'))  OR  Symbol('x')
      std::vector<std::string> params;
      if (accept(Tok::LParen)){
        if (t.kind!=Tok::RParen){
          while (true){
            auto p = head(); if (!p || p->k!=SNode::K_Symbol) return std::nullopt;
            params.push_back(p->name);
            if (accept(Tok::Comma)) continue;
            break;
          }
        }
        expect(Tok::RParen);
      }else{
        auto p = head(); if (!p || p->k!=SNode::K_Symbol) return std::nullopt;
        params.push_back(p->name);
      }
      expect(Tok::Comma);
      auto body = head(); if (!body) return std::nullopt;
      expect(Tok::RParen);
      // encode Lambda as: xs[0..n-1]=param symbols, xs[n]=body
      SNode n; n.k=SNode::K_Lambda;
      n.xs.reserve(params.size()+1);
      for (auto& nm: params){ SNode s; s.k=SNode::K_Symbol; s.name = nm; n.xs.push_back(std::move(s)); }
      n.xs.push_back(std::move(*body));
      return n;
    }

    // Unknown head: skip to matching ')'
    int depth=1;
    while (depth>0 && t.kind!=Tok::End){
      if (t.kind==Tok::LParen) ++depth;
      else if (t.kind==Tok::RParen) --depth;
      t = L.next();
    }
    SNode n; n.k=SNode::K_Unknown; n.name=tag; return n;
  }
};

// ---------- JSON (minimal) reader for {"decls":[{"name":...,"srepr":...}, ...]} ----------
struct DeclPair { std::string name; std::string srepr; };

inline bool read_decl_pairs(std::istream& in, std::vector<DeclPair>& out, std::string* err){
  std::ostringstream ss; ss << in.rdbuf();
  const std::string s = ss.str();
  size_t p = s.find("\"decls\"");
  if (p==std::string::npos){ if (err) *err="no decls key"; return false; }
  p = s.find('[', p);
  if (p==std::string::npos){ if (err) *err="decls not array"; return false; }
  size_t q = s.find(']', p);
  if (q==std::string::npos){ if (err) *err="unterminated decls array"; return false; }
  std::string_view arr(&s[p+1], q-(p+1));

  size_t i=0;
  auto skip=[&]{ while (i<arr.size() && std::isspace((unsigned char)arr[i])) ++i; };
  auto parse_string=[&](std::string& out)->bool{
    skip();
    if (i>=arr.size() || (arr[i]!='"' && arr[i]!='\'')) return false;
    char q = arr[i++];
    out.clear();
    while (i<arr.size()){
      char c = arr[i++];
      if (c==q) return true;
      if (c=='\\' && i<arr.size()){
        char n = arr[i++];
        switch(n){ case 'n': out.push_back('\n'); break; case 'r': out.push_back('\r'); break;
                   case 't': out.push_back('\t'); break; case '\\': out.push_back('\\'); break;
                   case '\'': out.push_back('\''); break; case '"': out.push_back('"'); break;
                   default: out.push_back(n); break; }
      } else out.push_back(c);
    }
    return false;
  };

  while (i < arr.size()){
    skip();
    if (i>=arr.size()) break;
    if (arr[i]=='{'){
      ++i;
      std::string name, srepr;
      bool got_name=false, got_srepr=false;
      while (i < arr.size() && arr[i] != '}'){
        skip();
        std::string key; if (!parse_string(key)) return false;
        skip(); if (i>=arr.size() || arr[i] != ':') return false; ++i; skip();
        if (key=="name"){ if (!parse_string(name)) return false; got_name=true; }
        else if (key=="srepr"){ if (!parse_string(srepr)) return false; got_srepr=true; }
        else {
          // skip string or simple token
          if (i < arr.size() && (arr[i]=='"'||arr[i]=='\'')){ std::string tmp; if (!parse_string(tmp)) return false; }
          else { while (i < arr.size() && arr[i]!=',' && arr[i]!='}') ++i; }
        }
        skip(); if (i < arr.size() && arr[i]==',') ++i;
      }
      if (i < arr.size() && arr[i]=='}') ++i;
      if (got_name && got_srepr) out.push_back({name, srepr});
    }
    skip(); if (i < arr.size() && arr[i]==',') ++i;
  }
  return true;
}

// ---------- Lower srepr AST → IR ----------
struct Glue {
  c2ir::Module& M;
  // Builtins
  c2ir::TypeId tInt{}, tFloat{}, tDouble{};

  explicit Glue(c2ir::Module& M) : M(M) {
    tInt    = M.makeBuiltin(c2ir::TypeKind::Int);
    tFloat  = M.makeBuiltin(c2ir::TypeKind::Float);
    tDouble = M.makeBuiltin(c2ir::TypeKind::Double);
  }

  c2ir::ExprId makeInt(long long v){
    c2ir::Expr e; e.kind=c2ir::ExprKind::IntLit; e.int_val=v; e.ty=tInt; return M.make(e);
  }
  c2ir::ExprId makeFloat(double d){
    c2ir::Expr e; e.kind=c2ir::ExprKind::FloatLit; e.float_val=d; e.ty=tDouble; return M.make(e);
  }
  c2ir::ExprId makeVar(const std::string& nm){
    c2ir::Expr e; e.kind=c2ir::ExprKind::VarRef; e.str_val=nm; e.ty=tInt; return M.make(e);
  }
  c2ir::ExprId makeBin(c2ir::BinOp op, c2ir::ExprId a, c2ir::ExprId b, c2ir::TypeId ty){
    c2ir::Expr e; e.kind=c2ir::ExprKind::BinOp; e.binop=op; e.lhs=a; e.rhs=b; e.ty=ty; return M.make(e);
  }
  c2ir::ExprId makeCall(c2ir::ExprId callee, const std::vector<c2ir::ExprId>& args, c2ir::TypeId ty){
    c2ir::Expr e; e.kind=c2ir::ExprKind::Call; e.callee=callee; e.args=args; e.ty=ty; return M.make(e);
  }
  c2ir::StmtId makeReturn(c2ir::ExprId e){
    c2ir::Stmt s; s.kind=c2ir::StmtKind::Return; s.ret_expr=e; return M.make(s);
  }
  c2ir::StmtId makeCompound(const std::vector<c2ir::StmtId>& items){
    c2ir::Stmt s; s.kind=c2ir::StmtKind::Compound; s.items=items; return M.make(s);
  }
  c2ir::TypeId makeFnType(c2ir::TypeId ret, const std::vector<c2ir::TypeId>& params, bool varargs=false){
    c2ir::Type T; T.kind=c2ir::TypeKind::Function; T.ret=ret; T.params=params; T.varargs=varargs; return M.make(T);
  }
};


inline std::optional<c2ir::ExprId> lower_expr(Glue& G, const SNode& n){
  using K = SNode::K;

  auto is_neg = [&](const SNode& t, const SNode** inner)->bool{
    if (t.k != K::K_Mul || t.xs.size() != 2) return false;
    if (t.xs[0].k != K::K_Int) return false;
    // Integer(-1) marker from exporter
    if (t.xs[0].name != std::string("-1")) return false;
    *inner = &t.xs[1];
    return true;
  };
  auto is_inv = [&](const SNode& t, const SNode** base)->bool{
    if (t.k != K::K_Pow || t.xs.size()!=2) return false;
    if (t.xs[1].k != K::K_Int) return false;
    if (t.xs[1].name != std::string("-1")) return false;
    *base = &t.xs[0];
    return true;
  };

  switch (n.k){
    case K::K_Int: {
      long long v=0; try{ v = std::stoll(n.name); }catch(...){}
      return G.makeInt(v);
    }
    case K::K_Float: {
      double d=0; try{ d = std::stod(n.name); }catch(...){}
      return G.makeFloat(d);
    }
    case K::K_Symbol:
      return G.makeVar(n.name);

    case K::K_Add: {
      if (n.xs.empty()) return std::nullopt;
      // Start with first term (respecting possible negation)
      c2ir::ExprId acc{};
      bool acc_set = false;
      for (size_t i=0;i<n.xs.size();++i){
        const SNode* inner=nullptr;
        bool neg = is_neg(n.xs[i], &inner);
        const SNode& term = neg ? *inner : n.xs[i];
        auto v = lower_expr(G, term); if (!v) return std::nullopt;
        if (!acc_set){
          acc = *v; acc_set = true;
          if (neg){ // -X as first term => 0 - X
            auto zero = G.makeInt(0);
            acc = G.makeBin(c2ir::BinOp::Sub, zero, acc, G.tInt);
          }
        }else{
          acc = G.makeBin(neg ? c2ir::BinOp::Sub : c2ir::BinOp::Add, acc, *v, G.tInt);
        }
      }
      return acc_set ? std::optional<c2ir::ExprId>(acc) : std::nullopt;
    }

    case K::K_Mul: {
      if (n.xs.empty()) return std::nullopt;
      c2ir::ExprId acc{};
      bool acc_set = false;
      for (size_t i=0;i<n.xs.size();++i){
        const SNode* base=nullptr;
        bool inv = is_inv(n.xs[i], &base);
        const SNode& factor = inv ? *base : n.xs[i];
        auto v = lower_expr(G, factor); if (!v) return std::nullopt;
        if (!acc_set){
          // If first factor is inverse, start from 1 / factor
          if (inv){
            auto one = G.makeInt(1);
            acc = G.makeBin(c2ir::BinOp::Div, one, *v, G.tInt);
          } else {
            acc = *v;
          }
          acc_set = true;
        } else {
          acc = G.makeBin(inv ? c2ir::BinOp::Div : c2ir::BinOp::Mul, acc, *v, G.tInt);
        }
      }
      return acc_set ? std::optional<c2ir::ExprId>(acc) : std::nullopt;
    }

    case K::K_Pow: {
      // Should only appear as an inverse inside Mul; otherwise unsupported for FR-007
      const SNode* base=nullptr;
      if (is_inv(n, &base)){
        // Represent as 1 / base
        auto v = lower_expr(G, *base); if (!v) return std::nullopt;
        auto one = G.makeInt(1);
        return G.makeBin(c2ir::BinOp::Div, one, *v, G.tInt);
      }
      return std::nullopt;
    }

    case K::K_Call: {
      auto cal = G.makeVar(n.name);
      std::vector<c2ir::ExprId> args; args.reserve(n.xs.size());
      for (auto& a : n.xs){ auto v = lower_expr(G, a); if (!v) return std::nullopt; args.push_back(*v); }
      return G.makeCall(cal, args, G.tInt);
    }

    default: return std::nullopt;
  }
}
// Lower a Lambda → DeclKind::Func with Return(body)
inline bool lower_lambda_to_decl(Glue& G, const std::string& name, const SNode& lam){
  if (lam.k != SNode::K_Lambda || lam.xs.size() < 1) return false;
  // Collect params
  std::vector<std::string> pnames;
  pnames.reserve(lam.xs.size()-1);
  for (size_t i=0;i+1<lam.xs.size();++i){
    if (lam.xs[i].k != SNode::K_Symbol) return false;
    pnames.push_back(lam.xs[i].name);
  }
  // Body expr
  auto bodyExpr = lower_expr(G, lam.xs.back()); if (!bodyExpr) return false;

  // Build function type: int (int, int, ...)
  std::vector<c2ir::TypeId> ptypes(pnames.size(), G.tInt);
  c2ir::TypeId fnty = G.makeFnType(G.tInt, ptypes, /*varargs*/false);

  // Build Decl
  c2ir::Decl D;
  D.kind    = c2ir::DeclKind::Func;
  D.linkage = c2ir::Linkage::External;
  D.name    = name;
  D.type    = fnty;
  D.params.reserve(pnames.size());
  for (size_t i=0;i<pnames.size();++i){
    c2ir::Param P; P.name=pnames[i]; P.type=ptypes[i]; D.params.push_back(P);
  }

  // Body: Return(body) wrapped in a Compound
  c2ir::StmtId ret = G.makeReturn(*bodyExpr);
  c2ir::StmtId comp = G.makeCompound({ret});
  D.body = comp;

  (void)G.M.make(D);
  return true;
}

// ---------- Public API ----------
inline bool read_srepr_json_to_ir(std::istream& in, c2ir::Module& M, std::string* err=nullptr){
  std::vector<DeclPair> ds;
  if (!read_decl_pairs(in, ds, err)) return false;
  Glue G{M};
  for (const auto& d : ds){
    Parser P(d.srepr);
    auto n = P.parse();
    if (!n){ if (err) *err = "parse error in srepr for decl '" + d.name + "'"; return false; }
    if (n->k == SNode::K_Lambda){
      if (!lower_lambda_to_decl(G, d.name, *n)){
        if (err) *err = "failed to lower Lambda decl '" + d.name + "'";
        return false;
      }
    } else {
      // Ignore non-Lambda decls in FR-007 (Typedef/Var can be added later)
    }
  }
  return true;
}

} // namespace c2sympy_ingest

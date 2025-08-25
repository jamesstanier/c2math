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
#include <istream>
#include <sstream>
#include <string>
#include <vector>
#include <utility>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <limits>
#include "ir.hpp"

namespace c2json {

// ======== Minimal JSON DOM (enough for our schema) ===========================
struct JValue {
  enum class Kind { Null, Bool, Number, String, Array, Object } kind = Kind::Null;
  bool b = false;
  double num = 0.0;
  std::string str;
  std::vector<JValue> arr;
  std::vector<std::pair<std::string, JValue>> obj;

  const JValue* find(const std::string& key) const {
    if (kind != Kind::Object) return nullptr;
    for (auto& kv : obj) if (kv.first == key) return &kv.second;
    return nullptr;
  }
  bool isNull()   const { return kind == Kind::Null; }
  bool isBool()   const { return kind == Kind::Bool; }
  bool isNum()    const { return kind == Kind::Number; }
  bool isStr()    const { return kind == Kind::String; }
  bool isArr()    const { return kind == Kind::Array; }
  bool isObj()    const { return kind == Kind::Object; }
};

struct JParser {
  const char* p = nullptr;
  const char* e = nullptr;
  std::string* err = nullptr;

  explicit JParser(const std::string& s, std::string* errOut=nullptr) : p(s.c_str()), e(s.c_str()+s.size()), err(errOut) {}

  void ws() { while (p<e && std::isspace(static_cast<unsigned char>(*p))) ++p; }

  bool match(char c){ ws(); if (p<e && *p==c){ ++p; return true; } return false; }
  bool want(char c){ if (!match(c)){ fail(std::string("expected '")+c+"'"); return false; } return true; }

  [[noreturn]] void fail(const std::string& m){ if (err) *err = m; throw std::runtime_error(m); }

  JValue parse() {
    ws();
    JValue v = parseValue();
    ws();
    if (p!=e) fail("trailing characters after JSON value");
    return v;
  }

  JValue parseValue() {
    ws();
    if (p>=e) fail("unexpected end of input");
    char c=*p;
    if (c=='"') return parseString();
    if (c=='{' ) return parseObject();
    if (c=='[' ) return parseArray();
    if (c=='t' || c=='f') return parseBool();
    if (c=='n') return parseNull();
    // number
    return parseNumber();
  }

  JValue parseNull(){
    if (e-p<4 || std::string(p,p+4)!="null") fail("expected 'null'");
    p+=4; return JValue{};
  }
  JValue parseBool(){
    if (e-p>=4 && std::string(p,p+4)=="true") { p+=4; JValue v; v.kind=JValue::Kind::Bool; v.b=true; return v; }
    if (e-p>=5 && std::string(p,p+5)=="false"){ p+=5; JValue v; v.kind=JValue::Kind::Bool; v.b=false; return v; }
    fail("expected 'true' or 'false'"); return {};
  }
  static int hexval(char c){
    if (c>='0'&&c<='9') return c-'0';
    if (c>='a'&&c<='f') return 10+(c-'a');
    if (c>='A'&&c<='F') return 10+(c-'A');
    return -1;
  }
  JValue parseString(){
    want('"');
    std::string out;
    while (p<e){
      char c=*p++;
      if (c=='"') { JValue v; v.kind=JValue::Kind::String; v.str=std::move(out); return v; }
      if (c=='\\'){
        if (p>=e) fail("bad escape");
        char esc=*p++;
        switch(esc){
          case '"': out.push_back('"'); break;
          case '\\': out.push_back('\\'); break;
          case '/': out.push_back('/'); break;
          case 'b': out.push_back('\b'); break;
          case 'f': out.push_back('\f'); break;
          case 'n': out.push_back('\n'); break;
          case 'r': out.push_back('\r'); break;
          case 't': out.push_back('\t'); break;
          case 'u': {
            if (e-p<4) fail("short \\u escape");
            int h1=hexval(p[0]), h2=hexval(p[1]), h3=hexval(p[2]), h4=hexval(p[3]);
            if (h1<0||h2<0||h3<0||h4<0) fail("invalid \\u escape");
            unsigned code = (h1<<12)|(h2<<8)|(h3<<4)|h4;
            p+=4;
            // minimal BMP handling (UTF-8 encode)
            if (code < 0x80) out.push_back(static_cast<char>(code));
            else if (code < 0x800) {
              out.push_back(static_cast<char>(0xC0 | (code>>6)));
              out.push_back(static_cast<char>(0x80 | (code&0x3F)));
            } else {
              out.push_back(static_cast<char>(0xE0 | (code>>12)));
              out.push_back(static_cast<char>(0x80 | ((code>>6)&0x3F)));
              out.push_back(static_cast<char>(0x80 | (code&0x3F)));
            }
          } break;
          default: fail("invalid escape");
        }
      } else {
        out.push_back(c);
      }
    }
    fail("unterminated string"); return {};
  }
  JValue parseNumber(){
    ws();
    const char* s=p;
    if (p<e && (*p=='-'||*p=='+')) ++p;
    bool digits=false;
    while (p<e && std::isdigit(static_cast<unsigned char>(*p))) { ++p; digits=true; }
    if (p<e && *p=='.'){ ++p; while (p<e && std::isdigit(static_cast<unsigned char>(*p))) { ++p; digits=true; } }
    if (p<e && (*p=='e'||*p=='E')){ ++p; if (p<e && (*p=='+'||*p=='-')) ++p; bool any=false; while (p<e && std::isdigit(static_cast<unsigned char>(*p))) { ++p; any=true; } if(!any) fail("bad exponent"); }
    if (!digits) fail("invalid number");
    std::string tok(s,p);
    char* end=nullptr;
    errno=0;
    double val = std::strtod(tok.c_str(), &end);
    if (errno==ERANGE) fail("number out of range");
    if (end!=tok.c_str()+tok.size()) fail("invalid numeric literal");
    JValue v; v.kind=JValue::Kind::Number; v.num=val; return v;
  }
  JValue parseArray(){
    want('[');
    JValue v; v.kind=JValue::Kind::Array;
    ws();
    if (match(']')) return v;
    for(;;){
      v.arr.push_back(parseValue());
      ws();
      if (match(']')) break;
      want(',');
    }
    return v;
  }
  JValue parseObject(){
    want('{');
    JValue v; v.kind=JValue::Kind::Object;
    ws();
    if (match('}')) return v;
    for(;;){
      ws();
      if (p>=e || *p!='"') fail("expected string key");
      JValue key = parseString();
      ws(); want(':');
      JValue val = parseValue();
      v.obj.emplace_back(key.str, std::move(val));
      ws();
      if (match('}')) break;
      want(',');
    }
    return v;
  }
};

// ======== Helpers to coerce/validate ========================================
inline bool to_int(const JValue& v, int& out){ if(!v.isNum()) return false; double x=v.num; if (x>std::numeric_limits<int>::max()||x<std::numeric_limits<int>::min()) return false; out=(int)llround(x); return true; }
inline bool to_ll (const JValue& v, long long& out){ if(!v.isNum()) return false; out=(long long)llround(v.num); return true; }
inline bool to_bool(const JValue& v, bool& out){ if(!v.isBool()) return false; out=v.b; return true; }
inline bool to_str (const JValue& v, std::string& out){ if(!v.isStr()) return false; out=v.str; return true; }

inline c2ir::Linkage parse_linkage(const std::string& s){
  if (s=="external") return c2ir::Linkage::External;
  if (s=="internal") return c2ir::Linkage::Internal;
  return c2ir::Linkage::None;
}

inline c2ir::TypeKind parse_type_kind(const std::string& s){
  using TK=c2ir::TypeKind;
  if (s=="void") return TK::Void;
  if (s=="bool") return TK::Bool;
  if (s=="char") return TK::Char;
  if (s=="int") return TK::Int;
  if (s=="uint") return TK::UInt;
  if (s=="long") return TK::Long;
  if (s=="ulong") return TK::ULong;
  if (s=="float") return TK::Float;
  if (s=="double")return TK::Double;
  if (s=="pointer")return TK::Pointer;
  if (s=="function")return TK::Function;
  // fallback
  return TK::Int;
}

inline c2ir::DeclKind parse_decl_kind(const std::string& s){
  using DK=c2ir::DeclKind;
  if (s=="var") return DK::Var;
  if (s=="func")return DK::Func;
  if (s=="typedef")return DK::Typedef;
  return DK::Hole;
}

inline c2ir::StmtKind parse_stmt_kind(const std::string& s){
  using SK=c2ir::StmtKind;
  if (s=="compound") return SK::Compound;
  if (s=="return")  return SK::Return;
  if (s=="expr_stmt") return SK::ExprStmt;
  if (s=="decl_stmt") return SK::DeclStmt;
  if (s=="if")      return SK::If;
  return SK::Hole;
}

inline c2ir::ExprKind parse_expr_kind(const std::string& s){
  using EK=c2ir::ExprKind;
  if (s=="int_lit") return EK::IntLit;
  if (s=="float_lit") return EK::FloatLit;
  if (s=="string_lit") return EK::StringLit;
  if (s=="var_ref") return EK::VarRef;
  if (s=="binop") return EK::BinOp;
  if (s=="call") return EK::Call;
  return EK::Hole;
}

inline c2ir::BinOp parse_binop(const std::string& s){
  if (s=="+") return c2ir::BinOp::Add;
  if (s=="-") return c2ir::BinOp::Sub;
  if (s=="*") return c2ir::BinOp::Mul;
  return c2ir::BinOp::Div;
}

// ======== Main entry: JSON → Module =========================================
inline bool read_module_json(std::istream& is, c2ir::Module& M, std::string* error_out=nullptr) {
  // 1) load entire stream
  std::ostringstream buf; buf << is.rdbuf();
  std::string s = buf.str();
  if (s.empty()){ if(error_out) *error_out="empty input"; return false; }

  // 2) parse JSON
  std::string perr;
  try {
    JParser P(s, &perr);
    JValue root = P.parse();
    if (!root.isObj()) { if(error_out) *error_out="root is not an object"; return false; }

    // 3) ir_version
    const JValue* jver = root.find("ir_version");
    if (!jver || !jver->isStr()) { if(error_out) *error_out="missing ir_version"; return false; }
    if (jver->str != c2ir::IR_VERSION) {
      if (error_out) *error_out = "ir_version mismatch: got '"+jver->str+"' expected '"+std::string(c2ir::IR_VERSION)+"'";
      return false;
    }

    // clear module (by reassigning a fresh Module with same version)
    M = c2ir::Module{};

    // 4) types
    const JValue* jtypes = root.find("types");
    if (!jtypes || !jtypes->isArr()) { if(error_out) *error_out="missing types[]"; return false; }
    for (size_t i=0;i<jtypes->arr.size();++i){
      const JValue& jt = jtypes->arr[i];
      if (!jt.isObj()) { if(error_out) *error_out="type item not object"; return false; }

      // id (optional consistency check)
      if (const JValue* jid = jt.find("id")) {
        int id=0; if(!to_int(*jid,id) || id!=(int)i){ if(error_out) *error_out="type id mismatch"; return false; }
      }

      // kind
      const JValue* jkind = jt.find("kind"); if(!jkind||!jkind->isStr()){ if(error_out) *error_out="type missing kind"; return false; }
      c2ir::Type T; T.kind = parse_type_kind(jkind->str);

      if (T.kind == c2ir::TypeKind::Pointer) {
        const JValue* jp = jt.find("pointee"); int idx=0; if(!jp||!to_int(*jp,idx)){ if(error_out) *error_out="pointer missing pointee"; return false; }
        T.pointee = idx;
      } else if (T.kind == c2ir::TypeKind::Function) {
        const JValue* jret = jt.find("ret"); int ridx=0; if(!jret||!to_int(*jret,ridx)){ if(error_out) *error_out="function missing ret"; return false; }
        T.ret = ridx;
        if (const JValue* jparams = jt.find("params")){
          if (!jparams->isArr()) { if(error_out) *error_out="function params not array"; return false; }
          for (auto& jp : jparams->arr){ int tid=0; if(!to_int(jp,tid)){ if(error_out) *error_out="param type not int"; return false; } T.params.push_back(tid); }
        }
        if (const JValue* jva = jt.find("varargs")){ bool b; if(!to_bool(*jva,b)){ if(error_out) *error_out="varargs not bool"; return false; } T.varargs=b; }
      }

      // insert
      int made = (int)M.make(T);
      if (made!=(int)i){ if(error_out) *error_out="type insertion index mismatch"; return false; }
    }

    // 5) decls
    const JValue* jdecls = root.find("decls");
    if (!jdecls || !jdecls->isArr()) { if(error_out) *error_out="missing decls[]"; return false; }
    for (size_t i=0;i<jdecls->arr.size();++i){
      const JValue& jd = jdecls->arr[i];
      if (!jd.isObj()){ if(error_out) *error_out="decl item not object"; return false; }

      if (const JValue* jid = jd.find("id")) { int id=0; if(!to_int(*jid,id) || id!=(int)i){ if(error_out) *error_out="decl id mismatch"; return false; } }

      c2ir::Decl D;
      // required: kind, name
      const JValue* jkind = jd.find("kind"); if(!jkind||!jkind->isStr()){ if(error_out) *error_out="decl missing kind"; return false; }
      D.kind = parse_decl_kind(jkind->str);
      const JValue* jname = jd.find("name"); if(!jname||!jname->isStr()){ if(error_out) *error_out="decl missing name"; return false; }
      D.name = jname->str;

      if (D.kind != c2ir::DeclKind::Hole) {
        const JValue* jtype = jd.find("type"); int t=0; if(!jtype||!to_int(*jtype,t)){ if(error_out) *error_out="decl missing type"; return false; }
        D.type=t;
        const JValue* jlink = jd.find("linkage"); if(!jlink||!jlink->isStr()){ if(error_out) *error_out="decl missing linkage"; return false; }
        D.linkage = parse_linkage(jlink->str);
      } else {
        const JValue* jrsn = jd.find("reason"); if (jrsn && jrsn->isStr()) D.reason=jrsn->str;
      }

      if (D.kind == c2ir::DeclKind::Func) {
        const JValue* jparams = jd.find("params");
        if (jparams){
          if (!jparams->isArr()) { if(error_out) *error_out="func params not array"; return false; }
          for (auto& jp : jparams->arr){
            if (!jp.isObj()) { if(error_out) *error_out="param not object"; return false; }
            c2ir::Param P;
            const JValue* jpn = jp.find("name"); if(!jpn||!jpn->isStr()){ if(error_out) *error_out="param missing name"; return false; }
            P.name=jpn->str;
            const JValue* jpt = jp.find("type"); int pt=0; if(!jpt||!to_int(*jpt,pt)){ if(error_out) *error_out="param missing type"; return false; }
            P.type=pt;
            D.params.push_back(P);
          }
        }
        const JValue* jbody = jd.find("body");
        if (jbody){
          if (jbody->isNull()) D.body=c2ir::kInvalidStmt;
          else { int sid=0; if(!to_int(*jbody,sid)) { if(error_out) *error_out="func body not int/null"; return false; } D.body=sid; }
        }
      }

      int made = (int)M.make(D);
      if (made!=(int)i){ if(error_out) *error_out="decl insertion index mismatch"; return false; }
    }

    // 6) stmts
    const JValue* jstmts = root.find("stmts");
    if (!jstmts || !jstmts->isArr()) { if(error_out) *error_out="missing stmts[]"; return false; }
    for (size_t i=0;i<jstmts->arr.size();++i){
      const JValue& js = jstmts->arr[i];
      if (!js.isObj()){ if(error_out) *error_out="stmt item not object"; return false; }
      if (const JValue* jid = js.find("id")) { int id=0; if(!to_int(*jid,id) || id!=(int)i){ if(error_out) *error_out="stmt id mismatch"; return false; } }

      c2ir::Stmt S;
      const JValue* jkind = js.find("kind"); if(!jkind||!jkind->isStr()){ if(error_out) *error_out="stmt missing kind"; return false; }
      S.kind = parse_stmt_kind(jkind->str);

      using SK=c2ir::StmtKind;
      switch (S.kind){
        case SK::Compound: {
          const JValue* jitems = js.find("items"); if(!jitems||!jitems->isArr()){ if(error_out) *error_out="compound missing items[]"; return false; }
          for (auto& it : jitems->arr){ int sid=0; if(!to_int(it,sid)){ if(error_out) *error_out="compound item not int"; return false; } S.items.push_back(sid); }
        } break;
        case SK::Return: {
          const JValue* jr = js.find("ret"); int eid=0; if(!jr||!to_int(*jr,eid)){ if(error_out) *error_out="return missing ret"; return false; } S.ret_expr=eid;
        } break;
        case SK::ExprStmt: {
          const JValue* je = js.find("expr"); int eid=0; if(!je||!to_int(*je,eid)){ if(error_out) *error_out="expr_stmt missing expr"; return false; } S.expr=eid;
        } break;
        case SK::DeclStmt: {
          const JValue* jn = js.find("name"); if(!jn||!jn->isStr()){ if(error_out) *error_out="decl_stmt missing name"; return false; } S.decl_name=jn->str;
          const JValue* jt = js.find("type"); int tid=0; if(!jt||!to_int(*jt,tid)){ if(error_out) *error_out="decl_stmt missing type"; return false; } S.decl_type=tid;
          const JValue* ji = js.find("init"); if(ji){ int eid=0; if(to_int(*ji,eid) && eid>=0) S.init=eid; else S.init=c2ir::kInvalidExpr; }
        } break;
        case SK::If: {
          const JValue* jc = js.find("cond"); int ce=0; if(!jc||!to_int(*jc,ce)){ if(error_out) *error_out="if missing cond"; return false; } S.cond=ce;
          const JValue* jt = js.find("then"); if(!jt||!jt->isNum()){ if(error_out) *error_out="if missing then"; return false; } { int sid=0; to_int(*jt,sid); S.then_branch = (sid<0? c2ir::kInvalidStmt : sid); }
          const JValue* je = js.find("else"); if(je){ int sid=0; if(to_int(*je,sid)) S.else_branch = (sid<0? c2ir::kInvalidStmt : sid); }
        } break;
        case SK::Hole: {
          const JValue* jr = js.find("reason"); if(jr && jr->isStr()) S.reason=jr->str;
        } break;
      }

      int made = (int)M.make(S);
      if (made!=(int)i){ if(error_out) *error_out="stmt insertion index mismatch"; return false; }
    }

    // 7) exprs
    const JValue* jexprs = root.find("exprs");
    if (!jexprs || !jexprs->isArr()) { if(error_out) *error_out="missing exprs[]"; return false; }
    for (size_t i=0;i<jexprs->arr.size();++i){
      const JValue& je = jexprs->arr[i];
      if (!je.isObj()){ if(error_out) *error_out="expr item not object"; return false; }
      if (const JValue* jid = je.find("id")) { int id=0; if(!to_int(*jid,id) || id!=(int)i){ if(error_out) *error_out="expr id mismatch"; return false; } }

      c2ir::Expr E;
      const JValue* jkind = je.find("kind"); if(!jkind||!jkind->isStr()){ if(error_out) *error_out="expr missing kind"; return false; }
      E.kind = parse_expr_kind(jkind->str);
      const JValue* jty = je.find("type"); int tid=0; if(!jty||!to_int(*jty,tid)){ if(error_out) *error_out="expr missing type"; return false; }
      E.ty=tid;

      using EK=c2ir::ExprKind;
      switch (E.kind){
        case EK::IntLit: {
          const JValue* v = je.find("value"); long long n=0; if(!v||!to_ll(*v,n)){ if(error_out) *error_out="int_lit missing value"; return false; } E.int_val = n;
        } break;
        case EK::FloatLit: {
          const JValue* v = je.find("value"); if(!v||!v->isNum()){ if(error_out) *error_out="float_lit missing value"; return false; } E.float_val = v->num;
        } break;
        case EK::StringLit:
        case EK::VarRef:
        case EK::Hole: {
          const JValue* v = je.find("value"); if(!v||!v->isStr()){ if(error_out) *error_out="string/var/hole missing value"; return false; } E.str_val = v->str;
          if (E.kind==EK::VarRef) { const JValue* jsym = je.find("sym"); if(jsym && jsym->isNum()){ int sid=0; to_int(*jsym,sid); E.sym_id=sid; } }
        } break;
        case EK::BinOp: {
          const JValue* jop = je.find("op"); if(!jop||!jop->isStr()){ if(error_out) *error_out="binop missing op"; return false; } E.binop = parse_binop(jop->str);
          const JValue* jl = je.find("lhs"); int l=0; if(!jl||!to_int(*jl,l)){ if(error_out) *error_out="binop missing lhs"; return false; } E.lhs=l;
          const JValue* jr = je.find("rhs"); int r=0; if(!jr||!to_int(*jr,r)){ if(error_out) *error_out="binop missing rhs"; return false; } E.rhs=r;
        } break;
        case EK::Call: {
          const JValue* jc = je.find("callee"); int c=0; if(!jc||!to_int(*jc,c)){ if(error_out) *error_out="call missing callee"; return false; } E.callee=c;
          const JValue* ja = je.find("args"); if(ja){ if(!ja->isArr()){ if(error_out) *error_out="call args not array"; return false; } for (auto& a: ja->arr){ int id=0; if(!to_int(a,id)){ if(error_out) *error_out="call arg not int"; return false; } E.args.push_back(id); } }
        } break;
      }

      int made = (int)M.make(E);
      if (made!=(int)i){ if(error_out) *error_out="expr insertion index mismatch"; return false; }
    }

    return true;
  } catch (const std::exception&) {
    if (error_out) *error_out = perr.empty() ? "parse/import error" : perr;
    return false;
  }
}

} // namespace c2json

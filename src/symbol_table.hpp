#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <optional>
#include <utility>
#include <cassert>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/AST.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Basic/Linkage.h"

namespace c2ast {

// --------------------------- Symbol Table Model ---------------------------

enum class ScopeKind { File, Function, Block };
enum class SymKind {
  Var, Func, Param, Typedef, TagStruct, TagUnion, TagEnum, EnumConst
};

enum class NameSpace {
  IdentOrFunc, // identifiers (vars/params) & functions (ordinary identifiers ns)
  TypedefName, // typedef namespace
  Tag          // struct/union/enum tag namespace
};

enum class Linkage {
  External, Internal, None
};

struct SrcLoc {
  std::string file; unsigned line = 0; unsigned col = 0;
};

struct Symbol {
  unsigned id = 0;
  SymKind kind;
  NameSpace ns;
  std::string name;        // empty for unnamed/anonymous
  unsigned scope_id = 0;   // where it is declared
  Linkage linkage = Linkage::None;
  std::string storage;     // "extern"/"static"/"register"/"auto"/""
  bool is_definition = false;

  // For reference/debugging: pointer string of the Decl
  const void* decl_ptr = nullptr;
  SrcLoc loc;
};

struct Scope {
  unsigned id = 0;
  ScopeKind kind;
  std::optional<unsigned> parent; // none for TU file-scope
  // map per namespace so C’s distinct namespaces don’t collide
  std::unordered_map<std::string, std::vector<unsigned>> byName_ident;
  std::unordered_map<std::string, std::vector<unsigned>> byName_typedef;
  std::unordered_map<std::string, std::vector<unsigned>> byName_tag;
  std::vector<unsigned> symbols; // all symbol ids declared directly in this scope
};

struct UseRef {
  // A resolved *use* of a symbol (identifier or type name reference)
  SrcLoc loc;
  NameSpace ns;
  std::string spelled;          // what appeared in source
  unsigned resolved_symbol_id;  // 0 if unresolved (shouldn’t happen if AST ok)
};

struct SymTab {
  std::vector<Scope> scopes;       // dense scope table by id
  std::vector<Symbol> symbols;     // dense symbol table by id
  std::vector<UseRef> uses;        // resolved identifier and type-name uses
};

// --------------------------- Utilities ---------------------------

inline SrcLoc toSrcLoc(const clang::SourceManager& SM, clang::SourceLocation L) {
  SrcLoc out;
  if (!L.isValid()) return out;
  clang::PresumedLoc P = SM.getPresumedLoc(L);
  if (!P.isValid()) return out;
  out.file = P.getFilename();
  out.line = P.getLine();
  out.col  = P.getColumn();
  return out;
}

/*inline c2ast::Linkage toLinkage(clang::Linkage L) {
  using clang::Linkage;
  switch (L) {
    case clang::ExternalLinkage: return c2ast::Linkage::External;
    case clang::InternalLinkage: return c2ast::Linkage::Internal;
    default: return c2ast::Linkage::None;
  }
}*/

inline c2ast::Linkage toLinkage(clang::Linkage L) {
  // If it's externally visible (External / Module / VisibleNone), call it External.
  if (clang::isExternallyVisible(L))
    return c2ast::Linkage::External;

  // Otherwise, check formal linkage. If it's None, we report None.
  if (clang::getFormalLinkage(L) == clang::Linkage::None)
    return c2ast::Linkage::None;

  // Everything else (Internal, UniqueExternal, ModuleInternal in older trees)
  return c2ast::Linkage::Internal;
}

inline std::string storageToStr(clang::StorageClass SC) {
  using clang::StorageClass;
  switch (SC) {
    case StorageClass::SC_None: return "";
    case StorageClass::SC_Extern: return "extern";
    case StorageClass::SC_Static: return "static";
    case StorageClass::SC_Register: return "register";
    case StorageClass::SC_Auto: return "auto";
    default: return "";
  }
}

// --------------------------- Builder ---------------------------

class SymbolTableBuilder : public clang::RecursiveASTVisitor<SymbolTableBuilder> {
public:
  SymbolTableBuilder(clang::ASTContext& Ctx, SymTab& Out)
    : Ctx(Ctx), SM(Ctx.getSourceManager()), Out(Out) {
    // Create TU file-scope (scope 0)
    Scope file;
    file.id = nextScopeId++;
    file.kind = ScopeKind::File;
    Out.scopes.push_back(file);
    scopeStack.push_back(file.id);
  }

  bool build(clang::TranslationUnitDecl* TU) {
    return TraverseDecl(TU);
  }

  // ---- Scope helpers ----
  unsigned curScope() const { return scopeStack.back(); }

  void pushScope(ScopeKind kind) {
    Scope s;
    s.id = nextScopeId++;
    s.kind = kind;
    s.parent = curScope();
    Out.scopes.push_back(s);
    scopeStack.push_back(s.id);
  }

  void popScope() {
    assert(scopeStack.size() > 1 && "Pop would remove file-scope");
    scopeStack.pop_back();
  }

  // ---- Symbol insertion & name-namespace helpers ----
  unsigned addSymbol(Symbol&& sym) {
    sym.id = nextSymId++;
    Out.symbols.push_back(sym);
    Out.scopes[curScope()].symbols.push_back(sym.id);
    auto& S = Out.scopes[curScope()];
    switch (sym.ns) {
      case NameSpace::IdentOrFunc: S.byName_ident[sym.name].push_back(sym.id); break;
      case NameSpace::TypedefName: S.byName_typedef[sym.name].push_back(sym.id); break;
      case NameSpace::Tag:         S.byName_tag[sym.name].push_back(sym.id); break;
    }
    return sym.id;
  }

  // Lookup visible symbol in C’s namespace rules (walk scope chain)
  std::optional<unsigned> lookupVisible(NameSpace ns, llvm::StringRef name) const {
    for (int i = (int)scopeStack.size() - 1; i >= 0; --i) {
      const Scope& S = Out.scopes[scopeStack[i]];
      switch (ns) {
        case NameSpace::IdentOrFunc: {
          auto it = S.byName_ident.find(name.str());
          if (it != S.byName_ident.end() && !it->second.empty())
            return it->second.back(); // most recent
          break;
        }
        case NameSpace::TypedefName: {
          auto it = S.byName_typedef.find(name.str());
          if (it != S.byName_typedef.end() && !it->second.empty())
            return it->second.back();
          break;
        }
        case NameSpace::Tag: {
          auto it = S.byName_tag.find(name.str());
          if (it != S.byName_tag.end() && !it->second.empty())
            return it->second.back();
          break;
        }
      }
    }
    return std::nullopt;
  }

  // -------------------- RecursiveASTVisitor hooks --------------------

  // File-scope declarations appear under TU. No extra scope needed here;
  // we already created a file-scope (scope 0) in the ctor.

  bool TraverseFunctionDecl(clang::FunctionDecl* FD) {
    // Insert the function symbol in *enclosing* scope (file-scope or block if nested)
    //if (FD->isThisDeclarationADefinition() || FD->isThisDeclarationADemarcation())
    insertFunction(FD); // inside, set: S.is_definition = FD->isThisDeclarationADefinition();

    // Enter function scope and add parameters
    pushScope(ScopeKind::Function);
    for (auto* P : FD->parameters()) insertParam(P);

    // Continue traversal (will visit body, including CompoundStmt blocks)
    bool ok = RecursiveASTVisitor::TraverseFunctionDecl(FD);
    popScope();
    return ok;
  }

  bool TraverseCompoundStmt(clang::CompoundStmt* CS) {
    pushScope(ScopeKind::Block);
    bool ok = RecursiveASTVisitor::TraverseCompoundStmt(CS);
    popScope();
    return ok;
  }

  // Decls

  bool VisitVarDecl(clang::VarDecl* VD) {
    // File-scope, function-scope, or block-scope variable (not parameter)
    Symbol S;
    S.kind = SymKind::Var;
    S.ns = NameSpace::IdentOrFunc;
    S.name = VD->getNameAsString();
    S.scope_id = curScope();
    S.linkage = toLinkage(VD->getLinkageInternal());
    S.storage = storageToStr(VD->getStorageClass());
    S.is_definition = VD->isThisDeclarationADefinition();
    S.decl_ptr = VD;
    S.loc = toSrcLoc(SM, VD->getLocation());
    addSymbol(std::move(S));
    return true;
  }

  bool VisitParmVarDecl(clang::ParmVarDecl* PD) {
    // Parameters are added in insertParam via TraverseFunctionDecl,
    // but keep a Visit hook in case some params appear in unusual places.
    return true;
  }

  bool VisitTypedefNameDecl(clang::TypedefNameDecl* TD) {
    Symbol S;
    S.kind = SymKind::Typedef;
    S.ns = NameSpace::TypedefName;
    S.name = TD->getNameAsString();
    S.scope_id = curScope();
    S.linkage = Linkage::None;
    S.storage = "";
    S.is_definition = true;
    S.decl_ptr = TD;
    S.loc = toSrcLoc(SM, TD->getLocation());
    addSymbol(std::move(S));
    return true;
  }

  bool VisitRecordDecl(clang::RecordDecl* RD) {
    // struct/union tag
    if (auto* TD = llvm::dyn_cast<clang::TagDecl>(RD)) {
      if (TD->getIdentifier()) {
        Symbol S;
        S.kind = RD->isUnion() ? SymKind::TagUnion : SymKind::TagStruct;
        S.ns = NameSpace::Tag;
        S.name = TD->getNameAsString();
        S.scope_id = curScope();
        S.linkage = Linkage::None;
        S.is_definition = RD->isCompleteDefinition();
        S.decl_ptr = RD;
        S.loc = toSrcLoc(SM, RD->getLocation());
        addSymbol(std::move(S));
      }
    }
    return true;
  }

  bool VisitEnumDecl(clang::EnumDecl* ED) {
    if (auto* TD = llvm::dyn_cast<clang::TagDecl>(ED)) {
      if (TD->getIdentifier()) {
        Symbol S;
        S.kind = SymKind::TagEnum;
        S.ns = NameSpace::Tag;
        S.name = TD->getNameAsString();
        S.scope_id = curScope();
        S.linkage = Linkage::None;
        S.is_definition = ED->isCompleteDefinition();
        S.decl_ptr = ED;
        S.loc = toSrcLoc(SM, ED->getLocation());
        addSymbol(std::move(S));
      }
    }
    return true;
  }

  bool VisitEnumConstantDecl(clang::EnumConstantDecl* ECD) {
    Symbol S;
    S.kind = SymKind::EnumConst;
    S.ns = NameSpace::IdentOrFunc; // Enum constants live in ordinary identifiers ns
    S.name = ECD->getNameAsString();
    S.scope_id = curScope();
    S.linkage = Linkage::None;
    S.is_definition = true;
    S.decl_ptr = ECD;
    S.loc = toSrcLoc(SM, ECD->getLocation());
    addSymbol(std::move(S));
    return true;
  }

  // Uses (identifier references)
  bool VisitDeclRefExpr(clang::DeclRefExpr* DRE) {
    clang::ValueDecl* D = DRE->getDecl();
    NameSpace ns = NameSpace::IdentOrFunc;
    std::string spelled = DRE->getNameInfo().getAsString();

    // Resolve via Decl (preferred) → then record as a use tied to the symbol id if present.
    unsigned symId = resolveFromDecl(D, ns, spelled).value_or(0);
    UseRef U{ toSrcLoc(SM, DRE->getLocation()), ns, spelled, symId };
    Out.uses.push_back(std::move(U));
    return true;
  }

  // Uses (typedef / tag types)
  bool VisitTypeLoc(clang::TypeLoc TL) {
    using namespace clang;
    // TypedefType
    if (auto TDTL = TL.getAs<TypedefTypeLoc>()) {
      auto* TD = TDTL.getTypedefNameDecl();
      std::string spelled = TD->getNameAsString();
      unsigned symId = resolveFromDecl(TD, NameSpace::TypedefName, spelled).value_or(0);
      UseRef U{ toSrcLoc(SM, TL.getBeginLoc()), NameSpace::TypedefName, spelled, symId };
      Out.uses.push_back(std::move(U));
    }
    // TagType (struct/union/enum)
    if (auto TTL = TL.getAs<TagTypeLoc>()) {
      auto* TD = TTL.getDecl();
      std::string spelled = TD->getNameAsString();
      NameSpace ns = NameSpace::Tag;
      unsigned symId = resolveFromDecl(TD, ns, spelled).value_or(0);
      UseRef U{ toSrcLoc(SM, TL.getBeginLoc()), ns, spelled, symId };
      Out.uses.push_back(std::move(U));
    }
    return true;
  }

  // -------------------- Dump utility (for debugging FR-002) --------------------

  void dump(llvm::raw_ostream& OS) const {
    OS << "Symbol Table:\n";
    for (const auto& S : Out.scopes) {
      OS << "  scope#" << S.id << " kind=" << scopeKindStr(S.kind);
      if (S.parent) OS << " parent=" << *S.parent;
      OS << "\n";
      for (auto sid : S.symbols) {
        const auto& sym = Out.symbols[sid];
        OS << "    [" << sym.id << "] " << symKindStr(sym.kind)
           << " ns=" << nsStr(sym.ns)
           << " name='" << sym.name << "'"
           << " def=" << (sym.is_definition ? "y" : "n")
           << " storage=" << (sym.storage.empty() ? "-" : sym.storage)
           << " linkage=" << linkStr(sym.linkage)
           << " @ " << sym.loc.file << ":" << sym.loc.line << ":" << sym.loc.col
           << "\n";
      }
    }
    OS << "Resolved uses:\n";
    for (const auto& U : Out.uses) {
      OS << "  " << nsStr(U.ns) << " '" << U.spelled << "' -> sym#" << U.resolved_symbol_id
         << " @ " << U.loc.file << ":" << U.loc.line << ":" << U.loc.col << "\n";
    }
  }

private:
  clang::ASTContext& Ctx;
  const clang::SourceManager& SM;
  SymTab& Out;
  std::vector<unsigned> scopeStack;
  unsigned nextSymId = 0;
  unsigned nextScopeId = 0;

  // Helpers
  void insertFunction(clang::FunctionDecl* FD) {
    Symbol S;
    S.kind = SymKind::Func;
    S.ns = NameSpace::IdentOrFunc;
    S.name = FD->getNameAsString();
    S.scope_id = curScope();
    S.linkage = toLinkage(FD->getLinkageInternal());
    S.storage = storageToStr(FD->getStorageClass());
    S.is_definition = FD->isThisDeclarationADefinition();
    S.decl_ptr = FD;
    S.loc = toSrcLoc(SM, FD->getLocation());
    addSymbol(std::move(S));
  }

  void insertParam(clang::ParmVarDecl* PD) {
    Symbol S;
    S.kind = SymKind::Param;
    S.ns = NameSpace::IdentOrFunc;
    S.name = PD->getNameAsString();
    S.scope_id = curScope();
    S.linkage = Linkage::None;
    S.is_definition = true;
    S.decl_ptr = PD;
    S.loc = toSrcLoc(SM, PD->getLocation());
    addSymbol(std::move(S));
  }

  std::optional<unsigned> resolveFromDecl(clang::Decl* D, NameSpace ns, llvm::StringRef spelled) const {
    // Fast path: try to match by walking visible scopes for spelled name and namespace.
    if (auto id = lookupVisible(ns, spelled)) return id;

    // Fallback: try to match through Decl name and namespace in any scope (rare if names shadow)
    for (const auto& S : Out.symbols) {
      if (S.ns == ns && S.name == spelled && S.decl_ptr == D) return S.id;
    }
    // If still not found, best-effort: match by Decl* only
    for (const auto& S : Out.symbols) {
      if (S.decl_ptr == D) return S.id;
    }
    return std::nullopt;
  }

  static const char* scopeKindStr(ScopeKind k) {
    switch (k) {
      case ScopeKind::File: return "file";
      case ScopeKind::Function: return "function";
      case ScopeKind::Block: return "block";
    }
    return "?";
  }
  static const char* symKindStr(SymKind k) {
    switch (k) {
      case SymKind::Var: return "var";
      case SymKind::Func: return "func";
      case SymKind::Param: return "param";
      case SymKind::Typedef: return "typedef";
      case SymKind::TagStruct: return "tag_struct";
      case SymKind::TagUnion: return "tag_union";
      case SymKind::TagEnum: return "tag_enum";
      case SymKind::EnumConst: return "enum_const";
    }
    return "?";
  }
  static const char* nsStr(NameSpace n) {
    switch (n) {
      case NameSpace::IdentOrFunc: return "ident";
      case NameSpace::TypedefName: return "typedef";
      case NameSpace::Tag: return "tag";
    }
    return "?";
  }
  static const char* linkStr(Linkage l) {
    switch (l) {
      case Linkage::External: return "external";
      case Linkage::Internal: return "internal";
      case Linkage::None: return "none";
    }
    return "?";
  }
};

} // namespace c2ast

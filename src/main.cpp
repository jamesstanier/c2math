#include <sstream>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <vector>
#include <optional>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/WithColor.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Version.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"

#include "symbol_table.hpp"
#include "ir.hpp"
#include "ir_builder.hpp"
#include "json_exporter.hpp"
#include "json_importer.hpp"

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// ---- CLI category & flags ----------------------------------------------------
static cl::OptionCategory C2ASTCat("c2ast options");
static cl::opt<bool> Werror(
    "warnings-as-errors",
    cl::desc("Treat warnings as errors (exit non-zero on any warnings)"),
    cl::cat(C2ASTCat), cl::init(false));

static cl::opt<std::string> StdOpt(
    "std",
    cl::desc("C language standard (e.g., c11, c17). If omitted, use compile_commands.json or default toolchain."),
    cl::value_desc("c-standard"), cl::cat(C2ASTCat));

static cl::opt<bool> DumpSyms(
  "dump-syms",
  cl::desc("Print the built symbol table and resolved uses (FR-002)"),
  cl::cat(C2ASTCat), cl::init(false)
);

static cl::opt<bool> DumpIR(
  "dump-ir",
  cl::desc("Print lowered IR (FR-003)"),
  cl::cat(C2ASTCat), cl::init(false)
);

static cl::opt<bool> DumpJSON(
  "dump-json",
  cl::desc("Print IR as JSON (FR-004)"),
  cl::cat(C2ASTCat), cl::init(false)
);

static cl::opt<std::string> ReadJSON(
  "read-json",
  cl::desc("READ IR from JSON file (FR-005)"),
  cl::value_desc("path"),
  cl::cat(C2ASTCat)
);

// ---- Diagnostic consumer that prints file:line:col and counts severities -----
class CollectingDiagConsumer : public DiagnosticConsumer {
public:
  unsigned NumErrors = 0;
  unsigned NumWarnings = 0;

  void HandleDiagnostic(DiagnosticsEngine::Level Level, const Diagnostic &Info) override {
    SmallString<256> Msg;
    Info.FormatDiagnostic(Msg);

    // Try to render source location
    const auto &SM = Info.getSourceManager();
    if (Info.hasSourceManager() && Info.getLocation().isValid()) {
      PresumedLoc PLoc = SM.getPresumedLoc(Info.getLocation());
      if (PLoc.isValid()) {
        // file:line:col: level: message
        WithColor::note() << PLoc.getFilename() << ":" << PLoc.getLine()
                          << ":" << PLoc.getColumn() << ": ";
      }
    }

    switch (Level) {
      case DiagnosticsEngine::Ignored: break;
      case DiagnosticsEngine::Note:
        llvm::outs() << "note: " << Msg << "\n";
        break;
      case DiagnosticsEngine::Remark:
        llvm::outs() << "remark: " << Msg << "\n";
        break;
      case DiagnosticsEngine::Warning:
        ++NumWarnings;
        llvm::errs() << "warning: " << Msg << "\n";
        break;
      case DiagnosticsEngine::Error:
      case DiagnosticsEngine::Fatal:
        ++NumErrors;
        llvm::errs() << "error: " << Msg << "\n";
        break;
    }
  }
};

// ---- A no-op action that still fully parses the TU (syntax-only) -------------
class ParseOnlyAction : public SyntaxOnlyAction {
public:
  bool BeginSourceFileAction(CompilerInstance &CI) override {
    // You could inspect CI.getLangOpts() here or tweak as needed
    return SyntaxOnlyAction::BeginSourceFileAction(CI);
  }

  void EndSourceFileAction() override {
    // If you need an ASTContext reference, you can access it here:
    // ASTContext &Ctx = getCompilerInstance().getASTContext();
    // For FR-001 we just ensure the TU was parsed successfully.
    ASTContext &Ctx = getCompilerInstance().getASTContext();
    clang::TranslationUnitDecl* TU = Ctx.getTranslationUnitDecl();

    // FR-002
    c2ast::SymTab ST;
    c2ast::SymbolTableBuilder B(Ctx, ST);
    B.build(TU);

    if (DumpSyms) {
      B.dump(llvm::outs());
    }

    // FR-003
    c2ir::Module M;
    c2ir::IRBuilder IRB(Ctx, &ST, M);
    IRB.lower(TU);

    if (DumpIR) {
      std::ostringstream oss;
      M.dump(oss);
      llvm::outs() << oss.str();
    }

    // FR-004
    if (DumpJSON) {
      c2json::write_module_json(M, std::cout);
      std::cout << std::endl;
    }

    SyntaxOnlyAction::EndSourceFileAction();
  }
};

int main(int argc, const char **argv) {
  // Print header for traceability (optional)
  if (!DumpJSON) {
    //llvm::outs() << "c2math (Clang " << getClangFullVersion() << ")\n";
  }

  auto ExpectedParser = CommonOptionsParser::create(argc, argv, C2ASTCat, cl::ZeroOrMore);
  
  if (!ExpectedParser) {
    WithColor::error() << toString(ExpectedParser.takeError()) << "\n";
    return 2;
  }
  CommonOptionsParser &Options = ExpectedParser.get();

  // FR-005
  if (!ReadJSON.getValue().empty()) {
    std::ifstream ifs(ReadJSON.getValue(), std::ios::in | std::ios::binary);
    if (!ifs) {
      WithColor::error() << "cannot open: " << ReadJSON.getValue() << "\n";
      return 1;
    }

    c2ir::Module M;
    std::string err;
    if (!c2json::read_module_json(ifs, M, &err)) {
      WithColor::error() << "import failed: " << err << "\n";
      return 1;
    }

    // Optional outputs after import:
    if (DumpIR) {
      M.dump(std::cout);
    } else if (DumpJSON) {
      c2json::write_module_json(M, std::cout);
      std::cout << '\n';
    } else {
      llvm::outs()  << "Imported IR v" << M.ir_version
                    << " (types=" << M.types.size()
                    << ", decls=" << M.decls.size()
                    << ", stmts=" << M.stmts.size()
                    << ", exprs=" << M.exprs.size() << ")\n";
    }
    return 0;
  }

  // Build the tool from compile_commands.json (respects per-file flags).
  ClangTool Tool(Options.getCompilations(), Options.getSourcePathList());

  // Optional: globally adjust command line (e.g., force -std=)
  if (!StdOpt.empty()) {
    Tool.appendArgumentsAdjuster(getInsertArgumentAdjuster(
        ("-std=" + StdOpt.getValue()).c_str(), ArgumentInsertPosition::BEGIN));
  }

  // Install our diagnostic consumer
  auto Diags = std::make_unique<CollectingDiagConsumer>();
  auto *DiagsPtr = Diags.get();
  Tool.setDiagnosticConsumer(Diags.release());

  // Run parsing action over all input files
  int RunCode = Tool.run(newFrontendActionFactory<ParseOnlyAction>().get());

  // Decide exit status per FR-001 constraints:
  // - Non-zero on fatal parse errors
  // - Optionally treat warnings as errors
  const bool HasErrors = (RunCode != 0) || (DiagsPtr->NumErrors > 0);
  const bool HasWarns = (DiagsPtr->NumWarnings > 0);

  if (HasErrors || (Werror && HasWarns)) {
    if (HasWarns && Werror)
      llvm::errs() << "warnings-as-errors: failing due to warnings.\n";
    return 1;
  }

  // Success path
  if (!DumpJSON) {
    // Print footer for traceability (optional)
    llvm::outs() << "c2math (Clang " << getClangFullVersion() << ")\n";
    llvm::outs() << "Parsed " << Options.getSourcePathList().size()
                 << " file(s) successfully.\n";
  }
  return 0;
}


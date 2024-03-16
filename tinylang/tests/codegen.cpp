#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/CodeGen/CodeGenerator.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Parser/Parser.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/TargetParser/Host.h"
#include "gtest/gtest.h"
#include <memory>

using namespace llvm;
using namespace tinylang;

class CodeGeneratorFixture : public testing::Test {
protected:
  int NumErrors = 0;

  bool HasErrors() { return NumErrors > 0; }

  ModuleDeclaration *parse(StringRef Source) {
    NumErrors = 0;
    SourceMgr SrcMgr;
    DiagnosticsEngine DiagsEng(SrcMgr);
    std::unique_ptr<MemoryBuffer> SrcBuffer =
        MemoryBuffer::getMemBuffer(Source, "main", true);
    SrcMgr.AddNewSourceBuffer(std::move(SrcBuffer), SMLoc());
    Lexer TheLexer(SrcMgr, DiagsEng);
    Sema TheSema(DiagsEng);
    Parser TheParser(TheLexer, TheSema);
    ModuleDeclaration *ModuleDecl = TheParser.parse();
    NumErrors = DiagsEng.numErrors();
    return ModuleDecl;
  }

  llvm::TargetMachine *createTargetMachine() {
    llvm::Triple Triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());
    llvm::outs() << "DefaultTargetTriple: "
                 << llvm::sys::getDefaultTargetTriple() << "\n";

    llvm::TargetOptions TargetOptions =
        codegen::InitTargetOptionsFromCodeGenFlags(Triple);

    std::string Error;
    const llvm::Target *Target =
        llvm::TargetRegistry::lookupTarget(codegen::getMArch(), Triple, Error);
    if (!Target) {
      llvm::WithColor::error(llvm::errs(), "test") << Error;
      return nullptr;
    }

    std::string CPUStr = codegen::getCPUStr();
    std::string FeatureStr = "";

    llvm::TargetMachine *TM = Target->createTargetMachine(
        Triple.getTriple(), CPUStr, FeatureStr, TargetOptions,
        std::optional<llvm::Reloc::Model>(codegen::getRelocModel()));
    return TM;
  }
};

TEST_F(CodeGeneratorFixture, TestModule) {
  ModuleDeclaration *ModuleDecl = parse("MODULE Test; END Test.");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  LLVMContext Ctx;
  TargetMachine *TM = createTargetMachine();
  ASSERT_NE(TM, nullptr);
  CodeGenerator *CodeGen = CodeGenerator::create(Ctx, TM);
  ASSERT_NE(CodeGen, nullptr);
}

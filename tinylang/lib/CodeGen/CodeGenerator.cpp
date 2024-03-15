#include "tinylang/CodeGen/CodeGenerator.h"

using namespace tinylang;

CodeGenerator *CodeGenerator::create(llvm::LLVMContext &Ctx,
                                     llvm::TargetMachine *TM) {
  return new CodeGenerator(Ctx, TM);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(ModuleDeclaration *Mod,
                                                 std::string Filename) {
  std::unique_ptr<llvm::Module> M =
      std::make_unique<llvm::Module>(Filename, Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());
  return M;
}

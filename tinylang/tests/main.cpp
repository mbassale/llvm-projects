#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include <gtest/gtest.h>
#

using namespace llvm;

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  InitLLVM X(argc, argv);
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  return RUN_ALL_TESTS();
}

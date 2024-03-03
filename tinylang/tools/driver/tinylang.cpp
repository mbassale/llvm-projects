#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"

enum ExitCodes : int {
  ExitSuccess = 0,
};

int main(int argc, char *argv[]) { return ExitCodes::ExitSuccess; }

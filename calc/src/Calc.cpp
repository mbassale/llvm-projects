#include "Lexer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string> Input(
    llvm::cl::Positional,
    llvm::cl::desc("<input expression>"),
    llvm::cl::init(""));


int main(int argc, char *argv[]) {
  llvm::InitLLVM X(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv, "calc - the expression compiler\n");
  Lexer Lex(Input);
  return 0;
}
#include "CodeGen.h"
#include "Lexer.h"
#include "Parser.h"
#include "Sema.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

#define RETURN_SEMANTIC_ERROR 2
#define RETURN_SYNTAX_ERROR 1
#define RETURN_SUCCESS 0

static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::desc("<input expression>"),
                                        llvm::cl::init(""));

int main(int argc, char *argv[]) {
  llvm::InitLLVM X(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "calc - the expression compiler\n");
  Lexer Lex(Input);
  Parser Parser(Lex);
  AST *Tree = Parser.parse();
  if (!Tree || Parser.hasError()) {
    llvm::errs() << "Syntax error.\n";
    return RETURN_SYNTAX_ERROR;
  }

  Sema Sema;
  if (!Sema.semantic(Tree)) {
    llvm::errs() << "Semantic error.\n";
    return RETURN_SEMANTIC_ERROR;
  }

  CodeGen CodeGen;
  CodeGen.compile(Tree);
  return RETURN_SUCCESS;
}

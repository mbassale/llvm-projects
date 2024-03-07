#include "tinylang/Parser/Parser.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace tinylang;
using namespace tinylang::tok;

TEST(ParserTest, ModuleParsing) {
  StringRef InputSrc = "MODULE Test; Import A; END Test.";
  SourceMgr SrcMgr;
  DiagnosticsEngine DiagsEng(SrcMgr);
  std::unique_ptr<MemoryBuffer> SrcBuffer =
      MemoryBuffer::getMemBuffer(InputSrc, "main", true);
  SrcMgr.AddNewSourceBuffer(std::move(SrcBuffer), SMLoc());
  Lexer TheLexer(SrcMgr, DiagsEng);
  Sema TheSema;
  Parser TheParser(TheLexer, TheSema);
  ModuleDeclaration *ModDecl = TheParser.parse();
  ASSERT_NE(ModDecl, nullptr);
  ASSERT_EQ(ModDecl->getName(), "Test");
}

#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

using namespace llvm;
using namespace tinylang;
using namespace tinylang::tok;

TEST(LexerText, BasicLexing) {
  StringRef InputSrc =
      "abcdef123 _abcdef123 123 \"foo\" 'bar' + - * / := . , ; : = # "
      "< > <= >= ( ) AND BEGIN CONST DIV DO END ELSE FROM IF IMPORT MOD MODULE "
      "NOT OR PROCEDURE RETURN THEN VAR WHILE";
  SourceMgr SrcMgr;
  DiagnosticsEngine DiagsEng(SrcMgr);
  std::unique_ptr<MemoryBuffer> SrcBuffer =
      MemoryBuffer::getMemBuffer(InputSrc, "main", true);
  SrcMgr.AddNewSourceBuffer(std::move(SrcBuffer), SMLoc());
  Lexer TheLexer(SrcMgr, DiagsEng);

  SmallVector<tok::TokenKind, 50> ExpectedTokens = {
      tok::identifier,     tok::identifier,     tok::integer_literal,
      tok::string_literal, tok::string_literal, tok::plus,
      tok::minus,          tok::star,           tok::slash,
      tok::colonequal,     tok::period,         tok::comma,
      tok::semicolon,      tok::colon,          tok::equal,
      tok::hash,           tok::less,           tok::greater,
      tok::lessequal,      tok::greaterequal,   tok::l_paren,
      tok::r_paren,        tok::kw_AND,         tok::kw_BEGIN,
      tok::kw_CONST,       tok::kw_DIV,         tok::kw_DO,
      tok::kw_END,         tok::kw_ELSE,        tok::kw_FROM,
      tok::kw_IF,          tok::kw_IMPORT,      tok::kw_MOD,
      tok::kw_MODULE,      tok::kw_NOT,         tok::kw_OR,
      tok::kw_PROCEDURE,   tok::kw_RETURN,      tok::kw_THEN,
      tok::kw_VAR,         tok::kw_WHILE};

  Token Result(InputSrc.data(), InputSrc.size(), tok::unknown);
  for (auto ExpectedTokenKind : ExpectedTokens) {
    TheLexer.next(Result);
    ASSERT_EQ(ExpectedTokenKind, Result.getKind());
  }
  TheLexer.next(Result);
  ASSERT_EQ(tok::eof, Result.getKind());
}

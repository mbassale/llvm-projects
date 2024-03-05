#ifndef TINYLANG_LEXER_LEXER_H
#define TINYLANG_LEXER_LEXER_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SourceMgr.h"

namespace tinylang {

class KeywordFilter {
  StringMap<tok::TokenKind> HashTable;

  void addKeyword(StringRef Keyword, tok::TokenKind TokenCode);

public:
  void addKeywords();

  tok::TokenKind getKeyword(StringRef Name,
                            tok::TokenKind DefaultTokenCode = tok::unknown) {
    auto Result = HashTable.find(Name);
    if (Result != HashTable.end()) {
      return Result->second;
    }
    return DefaultTokenCode;
  }
};

class Lexer {
public:
  SourceMgr &SrcMgr;
  DiagnosticsEngine &DiagsEng;

  const char *CurPtr;
  StringRef CurBuffer;

  unsigned CurBufferID = 0;

  KeywordFilter Keywords;

public:
  Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), DiagsEng(Diags) {
    CurBufferID = SrcMgr.getMainFileID();
    CurBuffer = SrcMgr.getMemoryBuffer(CurBufferID)->getBuffer();
    CurPtr = CurBuffer.begin();
    Keywords.addKeywords();
  }

  DiagnosticsEngine &getDiagnosticsEngine() const { return DiagsEng; }

  void next(Token &Result);

  StringRef getBuffer() const { return CurBuffer; }

private:
  void identifier(Token &Result);
  void number(Token &Result);
  void string(Token &Result);
  void comment();

  SMLoc getLoc() { return SMLoc::getFromPointer(CurPtr); }

  void makeToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
};

} // namespace tinylang

#endif // TINYLANG_LEXER_LEXER_H

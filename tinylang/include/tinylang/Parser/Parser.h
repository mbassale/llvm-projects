#ifndef TINYLANG_PARSER_PARSER_H
#define TINYLANG_PARSER_PARSER_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/TokenKinds.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Sema/Sema.h"

namespace tinylang {

class Parser {
  Lexer &Lex;
  Sema &Actions;
  Token Tok;

  DiagnosticsEngine &getDiagnosticsEngine() const {
    return Lex.getDiagnosticsEngine();
  }

  void advance() { Lex.next(Tok); }

  bool expect(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      return true;
    }

    const char *Expected = tok::getPunctuatorSpelling(ExpectedTok);
    if (!Expected) {
      Expected = tok::getKeywordSpelling(ExpectedTok);
    }
    llvm::StringRef Actual(Tok.getLocation().getPointer(), Tok.getLength());
    getDiagnosticsEngine().report(Tok.getLocation(), diag::err_expected,
                                  Expected, Actual);
    return false;
  }

  bool consume(tok::TokenKind ExpectedTok) {
    if (Tok.is(ExpectedTok)) {
      advance();
      return true;
    }
    return false;
  }

  // Used for error recovery
  template <typename... Tokens> bool skipUntil(Tokens &&...Toks) {
    while (true) {
      if ((... || Tok.is(Toks))) {
        return true;
      }

      if (Tok.is(tok::eof)) {
        return false;
      }
      advance();
    }
  }

  bool parseCompilationUnit(ModuleDeclaration *&D);
  bool parseImport();
  bool parseBlock(DeclList &Decls, StmtList &Stmts);
  bool parseDeclaration(DeclList &Decls);
  bool parseConstantDeclaration(DeclList &Decls);
  bool parseVariableDeclaration(DeclList &Decls);

  bool parseIdentList(IdentList &Ids);
  bool parseQualident(Decl *D);

public:
  Parser(Lexer &Lex, Sema &Actions);

  ModuleDeclaration *parse();
};

} // namespace tinylang

#endif // TINYLANG_PARSER_PARSER_H
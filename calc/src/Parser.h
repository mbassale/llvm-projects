#ifndef PARSER_H
#define PARSER_H

#include "AST.h"
#include "Lexer.h"
#include "llvm/Support/raw_ostream.h"

class Parser {
  Lexer &Lex;
  Token Tok;
  bool HasError;

  void error() {
    llvm::errs() << "Unexpected: " << Tok.getText() << "\n";
    HasError = true;
  }

  void advance() { Lex.next(Tok); }

  bool expect(Token::TokenKind Kind) {
    if (Tok.getKind() != Kind) {
      error();
      return false;
    }
    return true;
  }

  bool consume(Token::TokenKind Kind) {
    if (!expect(Kind)) {
      return false;
    }
    advance();
    return true;
  }

  AST *parseCalc();
  Expr *parseExpr();
  Expr *parseTerm();
  Expr *parseFactor();

public:
  Parser(Lexer &Lex) : Lex(Lex), HasError(false) { advance(); }

  bool hasError() { return HasError; }
  AST *parse();
};

#endif // PARSER_H

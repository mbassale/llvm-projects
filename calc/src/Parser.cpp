#include "Parser.h"

AST *Parser::parse() {
  AST *Res = parseCalc();
  expect(Token::eoi);
  return Res;
}

/*
 * parseCalc: Parse top-level expression.
 * calc: ("with" ident ("," ident)* ":")? expr ;
 */
AST *Parser::parseCalc() {
  llvm::outs() << "parseCalc()\n";
  Expr *E;
  llvm::SmallVector<llvm::StringRef, 8> Vars;

  // parse optional with
  if (Tok.is(Token::KW_with)) {
    llvm::outs() << "Token with\n";
    advance();
    // expect an identifier
    if (!expect(Token::ident)) {
      goto _error;
    }
    llvm::outs() << "Var: " << Tok.getText() << "\n";
    Vars.push_back(Tok.getText());

    advance();

    // repeat if more identifiers
    while (Tok.is(Token::comma)) {
      advance();
      if (!expect(Token::ident)) {
        goto _error;
      }
      llvm::outs() << "Var: " << Tok.getText() << "\n";
      Vars.push_back(Tok.getText());
      advance();
    }

    // with group must finish with colon
    if (!consume(Token::colon)) {
      goto _error;
    }
  }

  E = parseExpr();
  if (Vars.empty()) {
    return E;
  } else {
    return new WithDecl(Vars, E);
  }

_error:
  while (!Tok.is(Token::eoi)) {
    advance();
  }
  return nullptr;
}

/*
 * parseExpr: Parse an arithmetic expression.
 * expr: term (("+" | "-") term)* ;
 */
Expr *Parser::parseExpr() {
  Expr *Left = parseTerm();
  while (Tok.isOneOf(Token::plus, Token::minus)) {
    BinaryOp::Operator Op =
        Tok.is(Token::plus) ? BinaryOp::Plus : BinaryOp::Minus;
    advance();
    Expr *Right = parseTerm();
    Left = new BinaryOp(Op, Left, Right);
  }
  return Left;
}

/*
 * parseTerm: Parse a term.
 * term: factor (("*" | "/") factor)* ;
 */
Expr *Parser::parseTerm() {
  Expr *Left = parseFactor();
  while (Tok.isOneOf(Token::star, Token::slash)) {
    BinaryOp::Operator Op = Tok.is(Token::star) ? BinaryOp::Mul : BinaryOp::Div;
    advance();
    Expr *Right = parseFactor();
    Left = new BinaryOp(Op, Left, Right);
  }
  return Left;
}

/*
 * parseFactor: Parse factors and terminals.
 * factor: ident | number | "(" expr ")" ;
 * ident: Tok::ident
 * number: Tok::number
 */
Expr *Parser::parseFactor() {
  Expr *Res = nullptr;
  switch (Tok.getKind()) {
  case Token::number:
    Res = new Factor(Factor::Number, Tok.getText());
    advance();
    break;
  case Token::ident:
    Res = new Factor(Factor::Ident, Tok.getText());
    advance();
    break;
  case Token::l_paren:
    advance();
    Res = parseExpr();
    if (consume(Token::r_paren)) {
      break;
    }
  default:
    if (!Res) {
      error();
    }
    // TODO: consume tokens until we reach r_paren
  }
  return Res;
}

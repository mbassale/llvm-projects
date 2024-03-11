#include "tinylang/Parser/Parser.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"

using namespace tinylang;

namespace {

OperatorInfo fromTok(Token Tok) {
  return OperatorInfo(Tok.getLocation(), Tok.getKind());
}

} // namespace

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
  advance();
}

ModuleDeclaration *Parser::parse() {
  ModuleDeclaration *ModDecl = nullptr;
  parseCompilationUnit(ModDecl);
  return ModDecl;
}

bool Parser::parseCompilationUnit(ModuleDeclaration *&D) {

  auto _errorHandler = [this] { return skipUntil(); };

  if (!consume(tok::kw_MODULE)) {
    return _errorHandler();
  }

  if (!expect(tok::identifier)) {
    return _errorHandler();
  }

  D = Actions.actOnModuleDeclaration(Tok.getLocation(), Tok.getIdentifier());

  EnterDeclScope S(Actions, D);
  advance(); // for identifier
  if (!consume(tok::semicolon)) {
    return _errorHandler();
  }

  // parse imports
  while (Tok.isOneOf(tok::kw_FROM, tok::kw_IMPORT)) {
    if (!parseImport()) {
      return _errorHandler();
    }
  }

  // parse module body
  DeclList Decls;
  StmtList Stmts;
  if (!parseBlock(Decls, Stmts)) {
    return _errorHandler();
  }

  if (!expect(tok::identifier)) {
    return _errorHandler();
  }

  Actions.actOnModuleDeclaration(D, Tok.getLocation(), Tok.getIdentifier(),
                                 Decls, Stmts);

  // END [ModuleName].
  advance();
  if (!consume(tok::period)) {
    return _errorHandler();
  }

  return true;
}

bool Parser::parseImport() {
  auto _errorHandler = [this] {
    return skipUntil(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END, tok::kw_FROM,
                     tok::kw_IMPORT, tok::kw_PROCEDURE, tok::kw_VAR);
  };

  IdentList Ids;
  StringRef ModuleName;

  if (Tok.is(tok::kw_FROM)) {
    advance();
    if (!expect(tok::identifier)) {
      return _errorHandler();
    }
    ModuleName = Tok.getIdentifier();
    advance();
  }
  if (!consume(tok::kw_IMPORT)) {
    return _errorHandler();
  }
  if (!parseIdentList(Ids)) {
    return _errorHandler();
  }
  if (!expect(tok::semicolon)) {
    return _errorHandler();
  }
  advance();
  return true;
}

bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {
  auto _errorHandler = [this] { return skipUntil(tok::identifier); };

  while (Tok.isOneOf(tok::kw_CONST, tok::kw_PROCEDURE, tok::kw_VAR)) {
    if (!parseDeclaration(Decls)) {
      return _errorHandler();
    }
  }
  if (Tok.is(tok::kw_BEGIN)) {
    advance();
    if (!parseStatementSequence(Stmts)) {
      return _errorHandler();
    }
  }
  if (!consume(tok::kw_END)) {
    return _errorHandler();
  }
  return true;
}

bool Parser::parseDeclaration(DeclList &Decls) {
  auto _errorHandler = [this] {
    return skipUntil(tok::kw_BEGIN, tok::kw_CONST, tok::kw_END,
                     tok::kw_PROCEDURE, tok::kw_VAR);
  };

  // const declaration
  if (Tok.is(tok::identifier)) {
    advance();
    while (Tok.is(tok::identifier)) {
      if (!parseConstantDeclaration(Decls)) {
        return _errorHandler();
      }
      if (!consume(tok::semicolon)) {
        return _errorHandler();
      }
    }
  } else if (Tok.is(tok::kw_VAR)) { // variable declaration
    advance();
    while (Tok.is(tok::identifier)) {
      if (!parseVariableDeclaration(Decls)) {
        return _errorHandler();
      }
      if (!consume(tok::semicolon)) {
        return _errorHandler();
      }
    }
  } else if (Tok.is(tok::kw_PROCEDURE)) { // procedure declaration
    if (!parseProcedureDeclaration(Decls)) {
      return _errorHandler();
    }
    if (!consume(tok::semicolon)) {
      return _errorHandler();
    }
  } else {
    // ERROR
    return _errorHandler();
  }
  return true;
}

bool Parser::parseConstantDeclaration(DeclList &Decls) {
  auto _errorHandler = [this] { return skipUntil(tok::semicolon); };
  if (!expect(tok::identifier)) {
    return _errorHandler();
  }

  SMLoc Loc = Tok.getLocation();
  StringRef Name = Tok.getIdentifier();
  advance();
  if (!expect(tok::equal)) {
    return _errorHandler();
  }

  Expr *E = nullptr;
  advance();
  if (!parseExpression(E)) {
    return _errorHandler();
  }
  Actions.actOnConstantDeclaration(Decls, Loc, Name, E);
  return true;
}

bool Parser::parseVariableDeclaration(DeclList &Decls) {
  auto _errorHandler = [this] {
    while (!Tok.is(tok::semicolon)) {
      return skipUntil(tok::semicolon);
    }
    return true;
  };

  Decl *D = nullptr;
  IdentList Ids;
  if (!parseIdentList(Ids)) {
    return _errorHandler();
  }
  if (!consume(tok::colon)) {
    return _errorHandler();
  }
  if (!parseQualifiedIdent(D)) {
    return _errorHandler();
  }

  Actions.actOnVariableDeclaration(Decls, Ids, D);
  return true;
}

bool Parser::parseProcedureDeclaration(DeclList &ParentDecls) {
  auto _errorHandler = [this] { return skipUntil(tok::semicolon); };
  if (!consume(tok::kw_PROCEDURE)) {
    return _errorHandler();
  }

  // parse procedure identifier
  if (!expect(tok::identifier)) {
    return _errorHandler();
  }

  ProcedureDeclaration *D =
      Actions.actOnProcedureDeclaration(Tok.getLocation(), Tok.getIdentifier());

  EnterDeclScope S(Actions, D);
  FormalParamList Params;
  Decl *RetType = nullptr;
  advance();

  // parse procedure parameters
  if (Tok.is(tok::l_paren)) {
    if (!parseFormalParameters(Params, RetType)) {
      return _errorHandler();
    }
  }

  Actions.actOnProcedureHeading(D, Params, RetType);

  if (!expect(tok::semicolon)) {
    return _errorHandler();
  }

  // parse procedure body
  DeclList Decls;
  StmtList Stmts;
  advance();
  if (!parseBlock(Decls, Stmts)) {
    return _errorHandler();
  }
  if (!expect(tok::identifier)) {
    return _errorHandler();
  }

  Actions.actOnProcedureDeclaration(D, Tok.getLocation(), Tok.getIdentifier(),
                                    Decls, Stmts);

  ParentDecls.push_back(D);
  advance();
  return true;
}

bool Parser::parseFormalParameters(FormalParamList &Params, Decl *&RetType) {
  auto _errorHandler = [this] { return skipUntil(tok::semicolon); };

  // formal parameter list: "(" formalParameterList ")" ":" qualifiedIdentifier
  if (!consume(tok::l_paren)) {
    return _errorHandler();
  }
  if (Tok.isOneOf(tok::kw_VAR, tok::identifier)) {
    if (!parseFormalParameterList(Params)) {
      return _errorHandler();
    }
  }
  if (!consume(tok::r_paren)) {
    return _errorHandler();
  }
  if (Tok.is(tok::colon)) {
    advance();
    if (!parseQualifiedIdent(RetType)) {
      return _errorHandler();
    }
  }
  return true;
}

bool Parser::parseFormalParameterList(FormalParamList &Params) {
  auto _errorHandler = [this] { return skipUntil(tok::r_paren); };
  if (!parseFormalParameter(Params)) {
    return _errorHandler();
  }
  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseFormalParameter(Params)) {
      return _errorHandler();
    }
  }
  return true;
}

bool Parser::parseFormalParameter(FormalParamList &Params) {
  auto _errorHandler = [this] {
    return skipUntil(tok::r_paren, tok::semicolon);
  };
  IdentList Ids;
  Decl *D;
  bool IsVar = false;
  if (Tok.is(tok::kw_VAR)) {
    IsVar = true;
    advance();
  }
  if (!parseIdentList(Ids)) {
    return _errorHandler();
  }
  if (!consume(tok::colon)) {
    return _errorHandler();
  }
  if (!parseQualifiedIdent(D)) {
    return _errorHandler();
  }
  Actions.actOnFormalParameterDeclaration(Params, Ids, D, IsVar);
  return true;
}

bool Parser::parseStatementSequence(StmtList &Stmts) {
  auto _errorHandler = [this] { return skipUntil(tok::kw_ELSE, tok::kw_END); };

  if (!parseStatement(Stmts)) {
    return _errorHandler();
  }
  // each stmt is separated by semicolon
  while (Tok.is(tok::semicolon)) {
    advance();
    if (!parseStatement(Stmts)) {
      return _errorHandler();
    }
  }
  return true;
}

bool Parser::parseStatement(StmtList &Stmts) {
  auto _errorHandler = [this] {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  // identifier?
  if (Tok.is(tok::identifier)) {

    Decl *D;
    Expr *E = nullptr;
    SMLoc Loc = Tok.getLocation();
    if (!parseQualifiedIdent(D)) {
      return _errorHandler();
    }

    // identifier := expr
    if (Tok.is(tok::colonequal)) {
      advance();
      if (!parseExpression(E)) {
        return _errorHandler();
      }
      Actions.actOnAssignment(Stmts, Loc, D, E);
    } else if (Tok.is(tok::l_paren)) { // identifer "(" ?
      ExprList Exprs;
      advance();
      if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT,
                      tok::identifier, tok::integer_literal)) {
        if (!parseExpList(Exprs)) {
          return _errorHandler();
        }
      }
      if (!consume(tok::r_paren)) {
        return _errorHandler();
      }
      Actions.actOnProcedureCall(Stmts, Loc, D, Exprs);
    }
  } else if (Tok.is(tok::kw_IF)) {
    if (!parseIfStatement(Stmts)) {
      return _errorHandler();
    }
  } else if (Tok.is(tok::kw_WHILE)) {
    if (!parseWhileStatement(Stmts)) {
      return _errorHandler();
    }
  } else if (Tok.is(tok::kw_RETURN)) {
    if (!parseReturnStatement(Stmts)) {
      return _errorHandler();
    }
  } else {
    // ERROR
    return _errorHandler();
  }
  return true;
}

bool Parser::parseIfStatement(StmtList &Stmts) {
  auto _errorhandler = [this] {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  Expr *E = nullptr;
  StmtList IfStmts, ElseStmts;
  SMLoc Loc = Tok.getLocation();
  if (!consume(tok::kw_IF)) {
    return _errorhandler();
  }
  if (!parseExpression(E)) {
    return _errorhandler();
  }
  if (!consume(tok::kw_THEN)) {
    return _errorhandler();
  }
  if (!parseStatementSequence(IfStmts)) {
    return _errorhandler();
  }
  if (Tok.is(tok::kw_ELSE)) {
    advance();
    if (!parseStatementSequence(ElseStmts)) {
      return _errorhandler();
    }
  }
  if (!expect(tok::kw_END)) {
    return _errorhandler();
  }
  Actions.actOnIfStatement(Stmts, Loc, E, IfStmts, ElseStmts);
  advance();
  return true;
}

bool Parser::parseWhileStatement(StmtList &Stmts) {
  auto _errorhandler = [this] {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  Expr *E = nullptr;
  StmtList WhileStmts;
  SMLoc Loc = Tok.getLocation();
  if (!consume(tok::kw_WHILE)) {
    return _errorhandler();
  }
  if (!parseExpression(E)) {
    return _errorhandler();
  }
  if (!consume(tok::kw_DO)) {
    return _errorhandler();
  }
  if (!parseStatementSequence(WhileStmts)) {
    return _errorhandler();
  }
  if (!expect(tok::kw_END)) {
    return _errorhandler();
  }
  Actions.actOnWhileStatement(Stmts, Loc, E, WhileStmts);
  advance();
  return true;
}

bool Parser::parseReturnStatement(StmtList &Stmts) {
  auto _errorhandler = [this] {
    return skipUntil(tok::semicolon, tok::kw_ELSE, tok::kw_END);
  };

  Expr *E = nullptr;
  SMLoc Loc = Tok.getLocation();
  if (!consume(tok::kw_RETURN)) {
    return _errorhandler();
  }
  if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT,
                  tok::identifier, tok::integer_literal)) {
    if (!parseExpression(E)) {
      return _errorhandler();
    }
  }
  Actions.actOnReturnStatement(Stmts, Loc, E);
  return true;
}

bool Parser::parseExpList(ExprList &Exprs) {
  auto _errorhandler = [this] { return skipUntil(tok::r_paren); };

  Expr *E = nullptr;
  if (!parseExpression(E)) {
    return _errorhandler();
  }
  if (E) {
    Exprs.push_back(E);
  }
  while (Tok.is(tok::comma)) {
    E = nullptr;
    advance();
    if (!parseExpression(E)) {
      return _errorhandler();
    }
    if (E) {
      Exprs.push_back(E);
    }
  }
  return true;
}

bool Parser::parseExpression(Expr *&E) {
  auto _errorhandler = [this] {
    return skipUntil(tok::r_paren, tok::comma, tok::semicolon, tok::kw_DO,
                     tok::kw_ELSE, tok::kw_END, tok::kw_THEN);
  };

  if (!parseSimpleExpression(E)) {
    return _errorhandler();
  }
  if (Tok.isOneOf(tok::hash, tok::less, tok::lessequal, tok::equal,
                  tok::greater, tok::greaterequal)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if (!parseRelation(Op)) {
      return _errorhandler();
    }
    if (!parseSimpleExpression(Right)) {
      return _errorhandler();
    }
    E = Actions.actOnExpression(E, Right, Op);
  }
  return true;
}

bool Parser::parseRelation(OperatorInfo &Op) {
  auto _errorhandler = [this] {
    return skipUntil(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT,
                     tok::identifier, tok::integer_literal);
  };

  if (Tok.is(tok::equal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::hash)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::less)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::lessequal)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::greater)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::greaterequal)) {
    Op = fromTok(Tok);
    advance();
  } else {
    /*ERROR*/
    return _errorhandler();
  }

  return true;
}

bool Parser::parseSimpleExpression(Expr *&E) {
  auto _errorhandler = [this] {
    return skipUntil(tok::hash, tok::r_paren, tok::comma, tok::semicolon,
                     tok::less, tok::lessequal, tok::equal, tok::greater,
                     tok::greaterequal, tok::kw_DO, tok::kw_ELSE, tok::kw_END,
                     tok::kw_THEN);
  };

  OperatorInfo PrefixOp;
  if (Tok.isOneOf(tok::plus, tok::minus)) {
    if (Tok.is(tok::plus)) {
      PrefixOp = fromTok(Tok);
      advance();
    } else if (Tok.is(tok::minus)) {
      PrefixOp = fromTok(Tok);
      advance();
    }
  }
  if (!parseTerm(E)) {
    return _errorhandler();
  }
  while (Tok.isOneOf(tok::plus, tok::minus, tok::kw_OR)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if (!parseAddOperator(Op)) {
      return _errorhandler();
    }
    if (!parseTerm(Right)) {
      return _errorhandler();
    }
    E = Actions.actOnSimpleExpression(E, Right, Op);
  }
  if (!PrefixOp.isUnspecified()) {
    E = Actions.actOnPrefixExpression(E, PrefixOp);
  }
  return true;
}

bool Parser::parseAddOperator(OperatorInfo &Op) {
  auto _errorhandler = [this] {
    return skipUntil(tok::l_paren, tok::kw_NOT, tok::identifier,
                     tok::integer_literal);
  };

  if (Tok.is(tok::plus)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::minus)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::kw_OR)) {
    Op = fromTok(Tok);
    advance();
  } else {
    /*ERROR*/
    return _errorhandler();
  }

  return true;
}

bool Parser::parseTerm(Expr *&E) {
  auto _errorhandler = [this] {
    return skipUntil(tok::hash, tok::r_paren, tok::plus, tok::comma, tok::minus,
                     tok::semicolon, tok::less, tok::lessequal, tok::equal,
                     tok::greater, tok::greaterequal, tok::kw_DO, tok::kw_ELSE,
                     tok::kw_END, tok::kw_OR, tok::kw_THEN);
  };

  if (!parseFactor(E)) {
    return _errorhandler();
  }

  while (Tok.isOneOf(tok::star, tok::slash, tok::kw_AND, tok::kw_DIV,
                     tok::kw_MOD)) {
    OperatorInfo Op;
    Expr *Right = nullptr;
    if (!parseMulOperator(Op)) {
      return _errorhandler();
    }
    if (!parseFactor(Right)) {
      return _errorhandler();
    }
    E = Actions.actOnTerm(E, Right, Op);
  }
  return true;
}

bool Parser::parseFactor(Expr *&E) {
  auto _errorhandler = [this] {
    return skipUntil(
        tok::hash, tok::r_paren, tok::star, tok::plus, tok::comma, tok::minus,
        tok::slash, tok::semicolon, tok::less, tok::lessequal, tok::equal,
        tok::greater, tok::greaterequal, tok::kw_AND, tok::kw_DIV, tok::kw_DO,
        tok::kw_ELSE, tok::kw_END, tok::kw_MOD, tok::kw_OR, tok::kw_THEN);
  };
  if (Tok.is(tok::integer_literal)) {
    E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();
  } else if (Tok.is(tok::identifier)) {
    Decl *D;
    ExprList Exprs;
    if (!parseQualifiedIdent(D)) {
      return _errorhandler();
    }
    if (Tok.is(tok::l_paren)) {
      advance();
      if (Tok.isOneOf(tok::l_paren, tok::plus, tok::minus, tok::kw_NOT,
                      tok::identifier, tok::integer_literal)) {
        if (!parseExpList(Exprs)) {
          return _errorhandler();
        }
      }
      if (!expect(tok::r_paren)) {
        return _errorhandler();
      }
      E = Actions.actOnFunctionCall(D, Exprs);
      advance();
    } else if (Tok.isOneOf(tok::hash, tok::r_paren, tok::star, tok::plus,
                           tok::comma, tok::minus, tok::slash, tok::semicolon,
                           tok::less, tok::lessequal, tok::equal, tok::greater,
                           tok::greaterequal, tok::kw_AND, tok::kw_DIV,
                           tok::kw_DO, tok::kw_ELSE, tok::kw_END, tok::kw_MOD,
                           tok::kw_OR, tok::kw_THEN)) {
      E = Actions.actOnVariable(D);
      assert(E && "No expr set");
    }
  } else if (Tok.is(tok::l_paren)) {
    advance();
    if (!parseExpression(E)) {
      return _errorhandler();
    }
    if (!consume(tok::r_paren)) {
      return _errorhandler();
    }
  } else if (Tok.is(tok::kw_NOT)) {
    OperatorInfo Op = fromTok(Tok);
    advance();
    if (!parseFactor(E)) {
      return _errorhandler();
    }
    E = Actions.actOnPrefixExpression(E, Op);
  } else {
    /*ERROR*/
    return _errorhandler();
  }
  return false;
}

bool Parser::parseMulOperator(OperatorInfo &Op) {
  auto _errorhandler = [this] {
    return skipUntil(tok::l_paren, tok::kw_NOT, tok::identifier,
                     tok::integer_literal);
  };

  if (Tok.is(tok::star)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::slash)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::kw_DIV)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::kw_MOD)) {
    Op = fromTok(Tok);
    advance();
  } else if (Tok.is(tok::kw_AND)) {
    Op = fromTok(Tok);
    advance();
  } else {
    /*ERROR*/
    return _errorhandler();
  }

  return true;
}

bool Parser::parseIdentList(IdentList &Ids) {
  auto _errorHandler = [this] { return skipUntil(tok::colon, tok::semicolon); };
  if (!expect(tok::identifier)) {
    return _errorHandler();
  }
  Ids.push_back(
      std::pair<SMLoc, StringRef>(Tok.getLocation(), Tok.getIdentifier()));
  advance();
  while (Tok.is(tok::comma)) {
    advance();
    if (!expect(tok::identifier)) {
      return _errorHandler();
    }
    Ids.push_back(
        std::pair<SMLoc, StringRef>(Tok.getLocation(), Tok.getIdentifier()));
    advance();
  }
  return true;
}

bool Parser::parseQualifiedIdent(Decl *&D) {
  auto _errorhandler = [this] {
    return skipUntil(tok::hash, tok::l_paren, tok::r_paren, tok::star,
                     tok::plus, tok::comma, tok::minus, tok::slash,
                     tok::colonequal, tok::semicolon, tok::less, tok::lessequal,
                     tok::equal, tok::greater, tok::greaterequal, tok::kw_AND,
                     tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_END,
                     tok::kw_MOD, tok::kw_OR, tok::kw_THEN);
  };

  D = nullptr;
  if (!expect(tok::identifier)) {
    return _errorhandler();
  }

  D = Actions.actOnQualifiedIdentPart(D, Tok.getLocation(),
                                      Tok.getIdentifier());
  advance();
  while (Tok.is(tok::period) && (isa<ModuleDeclaration>(D))) {
    advance();
    if (!expect(tok::identifier)) {
      return _errorhandler();
    }
    D = Actions.actOnQualifiedIdentPart(D, Tok.getLocation(),
                                        Tok.getIdentifier());
    advance();
  }
  return true;
}

#include "tinylang/Parser/Parser.h"
#include "tinylang/Basic/LLVM.h"
#include <tinylang/AST/AST.h>
#include <tinylang/Basic/TokenKinds.h>

using namespace tinylang;

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

  // EnterDeclScope S(Actions, D);
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

  // EnterDeclScope S(Actions, D);
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
  // TODO: unimplemented
  return true;
}

bool Parser::parseExpression(Expr *&E) {
  // TODO: unimplemented
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

bool Parser::parseQualifiedIdent(Decl *D) {
  auto _errorHandler = [this] {
    return skipUntil(tok::hash, tok::l_paren, tok::r_paren, tok::star,
                     tok::plus, tok::comma, tok::minus, tok::slash,
                     tok::colonequal, tok::semicolon, tok::less, tok::lessequal,
                     tok::equal, tok::greater, tok::greaterequal, tok::kw_AND,
                     tok::kw_DIV, tok::kw_DO, tok::kw_ELSE, tok::kw_END,
                     tok::kw_MOD, tok::kw_OR, tok::kw_THEN);
  };
  D = nullptr;
  if (!expect(tok::identifier)) {
    return _errorHandler();
  }
  D = Actions.actOnQualifiedIdentPart(D, Tok.getLocation(),
                                      Tok.getIdentifier());
  advance();
  while (Tok.is(tok::period) && isa<ModuleDeclaration>(D)) {
    advance();
    if (!expect(tok::identifier)) {
      return _errorHandler();
    }
    D = Actions.actOnQualifiedIdentPart(D, Tok.getLocation(),
                                        Tok.getIdentifier());
    advance();
  }
  return true;
}

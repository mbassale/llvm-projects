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
  // TODO: unimplemented
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
  if (!parseQualident(D)) {
    return _errorHandler();
  }

  Actions.actOnVariableDeclaration(Decls, Ids, D);
  return true;
}

bool Parser::parseIdentList(IdentList &Ids) {
  // TODO: unimplemented
  return true;
}

bool Parser::parseQualident(Decl *D) {
  // TODO: unimplemented
  return true;
}

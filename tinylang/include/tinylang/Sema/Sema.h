#ifndef TINYLANG_SEMA_SEMA_H
#define TINYLANG_SEMA_SEMA_H

#include "tinylang/AST/AST.h"

namespace tinylang {

class Sema {
  Decl *CurrentDecl{nullptr};

public:
  ModuleDeclaration *actOnModuleDeclaration(SMLoc Loc, StringRef Name);
  void actOnModuleDeclaration(ModuleDeclaration *ModDecl, SMLoc Loc,
                              StringRef Name, DeclList &Decls, StmtList &Stmts);
  void actOnVariableDeclaration(DeclList &Decls, IdentList &Ids, Decl *D);
  void actOnConstantDeclaration(DeclList &Decls, SMLoc Loc, StringRef Name,
                                Expr *E);
  ProcedureDeclaration *actOnProcedureDeclaration(SMLoc Loc, StringRef Name);
  void actOnProcedureHeading(ProcedureDeclaration *ProcDecl,
                             FormalParamList &Params, Decl *RetType);
  void actOnProcedureDeclaration(ProcedureDeclaration *ProcDecl, SMLoc Loc,
                                 StringRef Name, DeclList &Decls,
                                 StmtList &Stmts);
  void actOnFormalParameterDeclaration(FormalParamList &Params, IdentList &Ids,
                                       Decl *D, bool IsVar);
  Decl *actOnQualifiedIdentPart(Decl *Prev, SMLoc Loc, StringRef Name);
};

} // namespace tinylang

#endif // TINYLANG_SEMA_SEMA_H

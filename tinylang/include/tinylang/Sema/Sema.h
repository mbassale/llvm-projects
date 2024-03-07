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
};

} // namespace tinylang

#endif // TINYLANG_SEMA_SEMA_H

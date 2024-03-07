#include "tinylang/Sema/Sema.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"

using namespace tinylang;

ModuleDeclaration *Sema::actOnModuleDeclaration(SMLoc Loc, StringRef Name) {
  return new ModuleDeclaration(CurrentDecl, Loc, Name);
}

void Sema::actOnModuleDeclaration(ModuleDeclaration *ModDecl, SMLoc Loc,
                                  StringRef Name, DeclList &Decls,
                                  StmtList &Stmts) {
  // TODO: unimplemented
}

void Sema::actOnVariableDeclaration(DeclList &Decls, IdentList &Ids, Decl *D) {
  // TODO: unimplemented
}

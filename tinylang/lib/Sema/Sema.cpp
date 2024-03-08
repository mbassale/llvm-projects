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

void Sema::actOnConstantDeclaration(DeclList &Decls, SMLoc Loc, StringRef Name,
                                    Expr *E) {
  // TODO: unimplemented
}

ProcedureDeclaration *Sema::actOnProcedureDeclaration(SMLoc Loc,
                                                      StringRef Name) {
  return nullptr;
}

void Sema::actOnProcedureHeading(ProcedureDeclaration *ProcDecl,
                                 FormalParamList &Params, Decl *RetType) {
  // TODO: unimplemented
}

void Sema::actOnProcedureDeclaration(ProcedureDeclaration *ProcDecl, SMLoc Loc,
                                     StringRef Name, DeclList &Decls,
                                     StmtList &Stmts) {
  // TODO: unimplemented
}

void Sema::actOnFormalParameterDeclaration(FormalParamList &Params,
                                           IdentList &Ids, Decl *D,
                                           bool IsVar) {
  // TODO: unimplemented
}

Decl *Sema::actOnQualifiedIdentPart(Decl *Prev, SMLoc Loc, StringRef Name) {
  return nullptr;
}

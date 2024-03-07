#ifndef TINYLANG_AST_AST_H
#define TINYLANG_AST_AST_H

#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "llvm/Support/SourceMgr.h"

namespace tinylang {

class Decl;
class FormalParameterDeclaration;
class Expr;
class Stmt;

using DeclList = std::vector<Decl *>;
using FormalParamList = std::vector<FormalParameterDeclaration *>;
using ExprList = std::vector<Expr *>;
using StmtList = std::vector<Stmt *>;
using IdentList = std::vector<std::pair<SMLoc, StringRef>>;

class Ident {
  SMLoc Loc;
  StringRef Name;

public:
  Ident(SMLoc Loc, const StringRef &Name) : Loc(Loc), Name(Name) {}

  SMLoc getLocation() { return Loc; }
  const StringRef &getName() { return Name; }
};

class Decl {
public:
  enum DeclKind { DK_Module, DK_Const, DK_Type, DK_Var, DK_Param, DK_Proc };

private:
  const DeclKind Kind;

protected:
  Decl *EnclosingDecl;
  SMLoc Loc;
  StringRef Name;

public:
  Decl(DeclKind Kind, Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Kind(Kind), EnclosingDecl(EnclosingDecl), Loc(Loc), Name(Name) {}

  DeclKind getKind() const { return Kind; }
  SMLoc getLocation() const { return Loc; }
  StringRef getName() const { return Name; }
  Decl *getEnclosingDecl() { return EnclosingDecl; }
};

class ModuleDeclaration : public Decl {
  DeclList Decls;
  StmtList Stmts;

public:
  ModuleDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DeclKind::DK_Module, EnclosingDecl, Loc, Name) {}

  ModuleDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name,
                    DeclList &Decls, StmtList &Stmts)
      : Decl(DeclKind::DK_Module, EnclosingDecl, Loc, Name), Decls(Decls),
        Stmts(Stmts) {}

  const DeclList &getDecls() { return Decls; }
  void setDecls(DeclList &D) { Decls = D; }
  const StmtList &getStmts() { return Stmts; }
  void setStmts(StmtList &L) { Stmts = L; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Module; }
};

class ConstantDeclaration : public Decl {
  Expr *E;

public:
  ConstantDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name, Expr *E)
      : Decl(DeclKind::DK_Const, EnclosingDecl, Loc, Name), E(E) {}

  Expr *getExpr() { return E; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Const; }
};

class TypeDeclaration : public Decl {
public:
  TypeDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DK_Type, EnclosingDecl, Loc, Name) {}

  static bool classof(const Decl *D) { return D->getKind() == DK_Type; }
};

class VariableDeclaration : public Decl {
  TypeDeclaration *Ty;

public:
  VariableDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name,
                      TypeDeclaration *Ty)
      : Decl(DK_Var, EnclosingDecl, Loc, Name), Ty(Ty) {}

  TypeDeclaration *getType() { return Ty; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Var; }
};

class FormalParameterDeclaration : public Decl {
  TypeDeclaration *Ty;
  bool IsVar;

public:
  FormalParameterDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name,
                             TypeDeclaration *Ty, bool IsVar)
      : Decl(DK_Param, EnclosingDecl, Loc, Name), Ty(Ty), IsVar(IsVar) {}

  TypeDeclaration *getType() { return Ty; }
  bool isVar() { return IsVar; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Param; }
};

class ProcedureDeclaration : public Decl {
  FormalParamList Params;
  TypeDeclaration *RetType;
  DeclList Decls;
  StmtList Stmts;

public:
  ProcedureDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name)
      : Decl(DK_Proc, EnclosingDecl, Loc, Name) {}

  ProcedureDeclaration(Decl *EnclosingDecl, SMLoc Loc, StringRef Name,
                       FormalParamList &Params, TypeDeclaration *RetType,
                       DeclList &Decls, StmtList &Stmts)
      : Decl(DK_Proc, EnclosingDecl, Loc, Name), Params(Params),
        RetType(RetType), Decls(Decls), Stmts(Stmts) {}

  const FormalParamList &getFormalParams() { return Params; }
  void setFormalParams(FormalParamList &FP) { Params = FP; }
  TypeDeclaration *getRetType() { return RetType; }
  void setRetType(TypeDeclaration *Ty) { RetType = Ty; }

  const DeclList &getDecls() { return Decls; }
  void setDecls(DeclList &D) { Decls = D; }
  const StmtList &getStmts() { return Stmts; }
  void setStmts(StmtList &L) { Stmts = L; }

  static bool classof(const Decl *D) { return D->getKind() == DK_Proc; }
};

} // namespace tinylang

#endif // TINYLANG_AST_AST_H

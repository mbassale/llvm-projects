#include "tinylang/Parser/Parser.h"
#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/LLVM.h"
#include "tinylang/Lexer/Lexer.h"
#include "tinylang/Sema/Sema.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"
#include <tinylang/Basic/TokenKinds.h>

using namespace llvm;
using namespace tinylang;
using namespace tinylang::tok;

class ParserFixture : public testing::Test {
protected:
  int NumErrors = 0;

  void SetUp() override {}

  void TearDown() override {}

  bool HasErrors() { return NumErrors > 0; }

  ModuleDeclaration *parse(StringRef Source) {
    NumErrors = 0;
    SourceMgr SrcMgr;
    DiagnosticsEngine DiagsEng(SrcMgr);
    std::unique_ptr<MemoryBuffer> SrcBuffer =
        MemoryBuffer::getMemBuffer(Source, "main", true);
    SrcMgr.AddNewSourceBuffer(std::move(SrcBuffer), SMLoc());
    Lexer TheLexer(SrcMgr, DiagsEng);
    Sema TheSema(DiagsEng);
    Parser TheParser(TheLexer, TheSema);
    ModuleDeclaration *ModuleDecl = TheParser.parse();
    NumErrors = DiagsEng.numErrors();
    return ModuleDecl;
  }

  void assert_type(TypeDeclaration *Ty, StringRef Name) {
    ASSERT_NE(Ty, nullptr);
    ASSERT_EQ(Ty->getKind(), Decl::DK_Type);
    ASSERT_EQ(Ty->getName(), Name);
  }

  void assert_integer(Decl *Decl, StringRef Name) {
    VariableDeclaration *VarDecl = dyn_cast_or_null<VariableDeclaration>(Decl);
    ASSERT_NE(VarDecl, nullptr);
    ASSERT_EQ(VarDecl->getName(), Name);
    ASSERT_EQ(VarDecl->getKind(), Decl::DK_Var);
    assert_type(VarDecl->getType(), "INTEGER");
  }
};

TEST_F(ParserFixture, TestEmptyModule) {
  ModuleDeclaration *ModuleDecl = parse("MODULE Test; END Test.");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  ASSERT_EQ(ModuleDecl->getDecls().size(), 0);
  ASSERT_EQ(ModuleDecl->getStmts().size(), 0);
}

TEST_F(ParserFixture, TestModuleVariables) {
  ModuleDeclaration *ModuleDecl = parse("\
  MODULE Test;      \
  VAR a: INTEGER;   \
  VAR b: INTEGER;   \
  VAR c: INTEGER;   \
  END Test.         \
  ");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  ASSERT_EQ(ModuleDecl->getDecls().size(), 3);
  ASSERT_EQ(ModuleDecl->getStmts().size(), 0);
  assert_integer(ModuleDecl->getDecls()[0], "a");
  assert_integer(ModuleDecl->getDecls()[1], "b");
  assert_integer(ModuleDecl->getDecls()[2], "c");
}

TEST_F(ParserFixture, TestModuleProcedures) {
  ModuleDeclaration *ModuleDecl = parse("   \
  MODULE Test;                              \
                                            \
  PROCEDURE PROCA(a, b: INTEGER) : INTEGER; \
  BEGIN                                     \
    RETURN a;                               \
  END PROCA;                                \
                                            \
  END Test.                                 \
");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  ASSERT_EQ(ModuleDecl->getDecls().size(), 1);
  ASSERT_EQ(ModuleDecl->getStmts().size(), 0);

  ProcedureDeclaration *ProcDecl =
      dyn_cast_or_null<ProcedureDeclaration>(ModuleDecl->getDecls()[0]);
  ASSERT_NE(ProcDecl, nullptr);
  ASSERT_EQ(ProcDecl->getName(), "PROCA");
  assert_type(ProcDecl->getRetType(), "INTEGER");

  FormalParamList FormalParams = ProcDecl->getFormalParams();
  ASSERT_EQ(FormalParams.size(), 2);
  FormalParameterDeclaration *ParamA = FormalParams[0];
  ASSERT_EQ(ParamA->getName(), "a");
  assert_type(ParamA->getType(), "INTEGER");
  FormalParameterDeclaration *ParamB = FormalParams[1];
  ASSERT_EQ(ParamB->getName(), "b");
  assert_type(ParamB->getType(), "INTEGER");

  ASSERT_EQ(ProcDecl->getDecls().size(), 0);
  ASSERT_EQ(ProcDecl->getStmts().size(), 1);

  ReturnStatement *RetStmt =
      dyn_cast_or_null<ReturnStatement>(ProcDecl->getStmts()[0]);
  ASSERT_NE(RetStmt, nullptr);
  VariableAccess *VarExpr =
      dyn_cast_or_null<VariableAccess>(RetStmt->getRetVal());
  ASSERT_NE(VarExpr, nullptr);
  assert_type(VarExpr->getType(), "INTEGER");
  Decl *Decl = VarExpr->getDecl();
  ASSERT_EQ(Decl->getKind(), Decl::DK_Param);
  FormalParameterDeclaration *ParamDecl =
      dyn_cast_or_null<FormalParameterDeclaration>(Decl);
  ASSERT_NE(ParamDecl, nullptr);
  ASSERT_EQ(ParamDecl->getName(), "a");
  assert_type(ParamDecl->getType(), "INTEGER");
}

TEST_F(ParserFixture, TestParseIfStmt) {
  ModuleDeclaration *ModuleDecl = parse("   \
  MODULE Test;                              \
                                            \
  PROCEDURE PROCA(a, b: INTEGER) : INTEGER; \
  BEGIN                                     \
    IF a = 1 THEN                           \
      RETURN 1;                             \
    ELSE                                    \
      RETURN b;                             \
    END;                                    \
    RETURN a;                               \
  END PROCA;                                \
                                            \
  END Test.\n");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  ASSERT_EQ(ModuleDecl->getDecls().size(), 1);
  ASSERT_EQ(ModuleDecl->getStmts().size(), 0);

  ProcedureDeclaration *ProcDecl =
      dyn_cast_or_null<ProcedureDeclaration>(ModuleDecl->getDecls()[0]);
  ASSERT_NE(ProcDecl, nullptr);
  ASSERT_EQ(ProcDecl->getName(), "PROCA");
  assert_type(ProcDecl->getRetType(), "INTEGER");

  ASSERT_EQ(ProcDecl->getDecls().size(), 0);
  ASSERT_EQ(ProcDecl->getStmts().size(), 2);

  IfStatement *IfStmt = dyn_cast_or_null<IfStatement>(ProcDecl->getStmts()[0]);
  ASSERT_NE(IfStmt, nullptr);

  // check condition
  ASSERT_NE(IfStmt->getCond(), nullptr);
  InfixExpression *InfixExpr =
      dyn_cast_or_null<InfixExpression>(IfStmt->getCond());
  ASSERT_NE(InfixExpr, nullptr);
  Expr *Left = InfixExpr->getLeft();
  assert_type(Left->getType(), "INTEGER");
  Expr *Right = InfixExpr->getRight();
  assert_type(Right->getType(), "INTEGER");

  // check if stmts
  ASSERT_EQ(IfStmt->getIfStmts().size(), 1);
  ReturnStatement *RetStmt =
      dyn_cast_or_null<ReturnStatement>(IfStmt->getIfStmts()[0]);
  ASSERT_NE(RetStmt, nullptr);

  // check else stmts
  ASSERT_EQ(IfStmt->getElseStmts().size(), 1);
  RetStmt = dyn_cast_or_null<ReturnStatement>(IfStmt->getElseStmts()[0]);
  ASSERT_NE(RetStmt, nullptr);
}

TEST_F(ParserFixture, TestWhileStmt) {
  ModuleDeclaration *ModuleDecl = parse("   \
  MODULE Test;                              \
                                            \
  PROCEDURE PROCA(a, b: INTEGER) : INTEGER; \
  VAR i: INTEGER;                           \
  BEGIN                                     \
    i := a;                                 \
    WHILE i <= b DO                         \
      i := i + 1;                           \
    END;                                    \
    RETURN i;                               \
  END PROCA;                                \
                                            \
  END Test.\n");
  ASSERT_FALSE(HasErrors());
  ASSERT_NE(ModuleDecl, nullptr);
  ASSERT_EQ(ModuleDecl->getDecls().size(), 1);
  ASSERT_EQ(ModuleDecl->getStmts().size(), 0);

  ProcedureDeclaration *ProcDecl =
      dyn_cast_or_null<ProcedureDeclaration>(ModuleDecl->getDecls()[0]);
  ASSERT_NE(ProcDecl, nullptr);
  ASSERT_EQ(ProcDecl->getName(), "PROCA");
  assert_type(ProcDecl->getRetType(), "INTEGER");

  ASSERT_EQ(ProcDecl->getDecls().size(), 1);
  ASSERT_EQ(ProcDecl->getStmts().size(), 3);

  VariableDeclaration *ProcVarDecl =
      dyn_cast_or_null<VariableDeclaration>(ProcDecl->getDecls()[0]);
  ASSERT_NE(ProcVarDecl, nullptr);
  ASSERT_EQ(ProcVarDecl->getName(), "i");
  assert_type(ProcVarDecl->getType(), "INTEGER");

  WhileStatement *WhileStmt =
      dyn_cast_or_null<WhileStatement>(ProcDecl->getStmts()[1]);
  ASSERT_NE(WhileStmt, nullptr);

  // check condition
  ASSERT_NE(WhileStmt->getCond(), nullptr);
  InfixExpression *InfixExpr =
      dyn_cast_or_null<InfixExpression>(WhileStmt->getCond());
  ASSERT_NE(InfixExpr, nullptr);
  Expr *Left = InfixExpr->getLeft();
  assert_type(Left->getType(), "INTEGER");
  Expr *Right = InfixExpr->getRight();
  assert_type(Right->getType(), "INTEGER");

  // check loop body
  ASSERT_EQ(WhileStmt->getWhileStmts().size(), 1);
  AssignmentStatement *AssignStmt =
      dyn_cast_or_null<AssignmentStatement>(WhileStmt->getWhileStmts()[0]);
  ASSERT_NE(AssignStmt, nullptr);
  VariableDeclaration *VarDecl = AssignStmt->getVar();
  ASSERT_NE(VarDecl, nullptr);
  ASSERT_EQ(VarDecl->getName(), "i");
  InfixExpr = dyn_cast_or_null<InfixExpression>(AssignStmt->getExpr());
  ASSERT_NE(InfixExpr, nullptr);
  ASSERT_EQ(InfixExpr->getOperatorInfo().getKind(), tok::plus);
}

TEST_F(ParserFixture, FullModuleParsing) {
  StringRef InputSrc = "\
MODULE Test;                              \n\
                                          \n\
VAR x: INTEGER;                           \n\
                                          \n\
PROCEDURE TESTA(a, b: INTEGER) : INTEGER; \n\
VAR t: INTEGER;                           \n\
BEGIN                                     \n\
  IF a = 0 THEN                           \n\
    RETURN b;                             \n\
  END;                                    \n\
  RETURN a;                               \n\
END TESTA;                                \n\
                                          \n\
END Test.                                 \n\
";

  ModuleDeclaration *ModDecl = parse(InputSrc);
  ASSERT_NE(ModDecl, nullptr);
  ASSERT_EQ(ModDecl->getName(), "Test");
}

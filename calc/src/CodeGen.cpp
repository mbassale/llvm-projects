#include "CodeGen.h"
#include "AST.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  PointerType *PtrTy;
  Constant *Int32Zero;
  Value *V;
  StringMap<Value *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    PtrTy = PointerType::getUnqual(M->getContext());
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(AST *Tree) {
    // create main, function type, function instance and the basic block of IR
    FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, PtrTy}, false);
    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);

    // IR builder should insert instructions on this basic block
    Builder.SetInsertPoint(BB);

    // AST traversal to generate IR
    Tree->accept(*this);

    // After the traversal, print the computed value "V" to stdout using the
    // calc_write() function
    FunctionType *CalcWriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
    Function *CalcWriteFn = Function::Create(
        CalcWriteFnTy, GlobalValue::ExternalLinkage, "calc_write", M);
    Builder.CreateCall(CalcWriteFnTy, CalcWriteFn, {V});
    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(WithDecl &Node) override {
    // create function proto for calc_read()
    FunctionType *ReadFty = FunctionType::get(Int32Ty, {PtrTy}, false);
    Function *ReadFn =
        Function::Create(ReadFty, GlobalValue::ExternalLinkage, "calc_read", M);

    // invoke calc_read() for each variable that needs to be initialized
    for (auto I = Node.begin(), E = Node.end(); I != E; ++I) {
      StringRef Var = *I;
      Constant *StrText = ConstantDataArray::getString(M->getContext(), Var);
      GlobalVariable *Str = new GlobalVariable(
          *M, StrText->getType(), /*isConstant=*/true,
          GlobalValue::PrivateLinkage, StrText, Twine(Var).concat(".str"));

      // we invoke calc_read() to get the variable value from the user
      CallInst *Call = Builder.CreateCall(ReadFty, ReadFn, {Str});
      // returned value will be used later during AST generation
      nameMap[Var] = Call;
    }

    // after the with declaration block we have a single expression to evaluate
    Node.getExpr()->accept(*this);
  }

  virtual void visit(Factor &Node) override {
    if (Node.getKind() == Factor::Ident) {
      V = nameMap[Node.getVal()];
    } else {
      int intval;
      Node.getVal().getAsInteger(10, intval);
      V = ConstantInt::get(Int32Ty, intval, true);
    }
  }

  virtual void visit(BinaryOp &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;
    switch (Node.getOperator()) {
    case BinaryOp::Plus:
      V = Builder.CreateNSWAdd(Left, Right);
      break;
    case BinaryOp::Minus:
      V = Builder.CreateNSWSub(Left, Right);
      break;
    case BinaryOp::Mul:
      V = Builder.CreateNSWMul(Left, Right);
      break;
    case BinaryOp::Div:
      V = Builder.CreateSDiv(Left, Right);
      break;
    }
  }
};

} // namespace

void CodeGen::compile(AST *Tree) {
  LLVMContext Ctx;
  Module *M = new Module("calc.expr", Ctx);
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
  M->print(outs(), nullptr);
}

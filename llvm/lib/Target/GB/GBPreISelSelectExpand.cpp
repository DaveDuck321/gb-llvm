#include "GB.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

// Expands all remaining select/ select_cc instructions into branches regardless
// of optimization options (since we can't natively support select)
// See also: llvm/lib/CodeGen/SelectOptimize.cpp
class GBPreISelSelectExpand : public FunctionPass {
  static char ID;

public:
  GBPreISelSelectExpand() : FunctionPass(ID) {}
  StringRef getPassName() const override { return "GBPreISelSelectExpand"; }

  bool expandNextSelect(Function &);
  bool runOnFunction(Function &) override;
};

bool GBPreISelSelectExpand::expandNextSelect(Function &F) {
  for (BasicBlock &BB : F) {
    for (Instruction &I : BB) {
      SelectInst *SI = dyn_cast<SelectInst>(&I);
      if (SI == nullptr) {
        continue;
      }

      // Split select
      BasicBlock *EndBlock =
          BB.splitBasicBlock(++BasicBlock::iterator(I), "select.end");

      BasicBlock *TrueBlock = BasicBlock::Create(
          SI->getContext(), "select.true", EndBlock->getParent(), EndBlock);
      BasicBlock *FalseBlock = BasicBlock::Create(
          SI->getContext(), "select.false", EndBlock->getParent(), EndBlock);

      BranchInst *TrueBranch = BranchInst::Create(EndBlock, TrueBlock);
      BranchInst *FalseBranch = BranchInst::Create(EndBlock, FalseBlock);

      if (auto *TrueInst = dyn_cast<Instruction>(SI->getTrueValue());
          TrueInst != nullptr) {
        TrueInst->moveBefore(TrueBranch);
      }
      if (auto *FalseInst = dyn_cast<Instruction>(SI->getFalseValue());
          FalseInst != nullptr) {
        FalseInst->moveBefore(FalseBranch);
      }

      // Delete implicit terminator created during the split, replace with a
      // conditional branch
      BB.getTerminator()->eraseFromParent();
      IRBuilder<> IB(&BB);
      auto *CondFreeze =
          IB.CreateFreeze(SI->getCondition(), SI->getName() + ".frozen");
      IB.CreateCondBr(CondFreeze, TrueBlock, FalseBlock, SI);

      PHINode *PN = PHINode::Create(SI->getType(), 2);
      PN->insertBefore(EndBlock->begin());
      PN->takeName(SI);
      PN->addIncoming(SI->getTrueValue(), TrueBlock);
      PN->addIncoming(SI->getFalseValue(), FalseBlock);
      PN->setDebugLoc(SI->getDebugLoc());

      SI->replaceAllUsesWith(PN);
      SI->eraseFromParent();

      return true;
    }
  }
  return false;
}

bool GBPreISelSelectExpand::runOnFunction(Function &F) {
  bool MadeProgress = false;
  while (expandNextSelect(F)) {
    MadeProgress = true;
  }
  return MadeProgress;
}

char GBPreISelSelectExpand::ID = 0;

FunctionPass *llvm::createGBPreISelSelectExpand() {
  return new GBPreISelSelectExpand();
}

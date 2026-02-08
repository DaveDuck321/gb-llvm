#include "GB.h"
#include "llvm/CodeGen/GlobalISel/Combiner.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/ErrorHandling.h"

#define DEBUG_TYPE "gb-postlegalize-expand"

using namespace llvm;

class GBPostLegalizeExpand : public MachineFunctionPass {
public:
  static char ID;

  GBPostLegalizeExpand() : MachineFunctionPass(ID) {}

  StringRef getPassName() const override { return "GBPostLegalizeExpand"; }

  bool expandJP_CP(MachineFunction &MF, MachineBasicBlock &MBB,
                   MachineInstr &MI);

  bool expandMachineInstruction(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineInstr &MI);

  bool runOnMachineFunction(MachineFunction &MF) override;
};

bool GBPostLegalizeExpand::expandJP_CP(MachineFunction &MF,
                                       MachineBasicBlock &MBB,
                                       MachineInstr &MI) {
  assert(MI.getOpcode() == GB::G_JP_CP);
  auto S8 = LLT::scalar(8);
  auto &MRI = MF.getRegInfo();
  auto LHS = MI.getOperand(1).getReg();
  auto RHS = MI.getOperand(2).getReg();
  auto Predicate = ICmpInst::Predicate(MI.getOperand(0).getPredicate());

  if (getIConstantVRegValWithLookThrough(LHS, MRI).has_value() &&
      !getIConstantVRegValWithLookThrough(RHS, MRI).has_value()) {
    std::swap(LHS, RHS);
    Predicate = CmpInst::getSwappedPredicate(Predicate);
  }

  switch (Predicate) {
  case CmpInst::ICMP_EQ:
  case CmpInst::ICMP_NE:
  case CmpInst::ICMP_UGE:
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_UGT:
  case CmpInst::ICMP_ULT:
    return false; // Nothing to do
  default:
    break;
  }

  auto UnsignedPredicate = [&] {
    switch (Predicate) {
    default:
      llvm_unreachable("Unrecognized predicate");
    case CmpInst::ICMP_SGE:
      return CmpInst::ICMP_UGE;
    case CmpInst::ICMP_SGT:
      return CmpInst::ICMP_UGT;
    case CmpInst::ICMP_SLE:
      return CmpInst::ICMP_ULE;
    case CmpInst::ICMP_SLT:
      return CmpInst::ICMP_ULT;
    }
  }();

  // We want the following codegen:
  //    ld a, lhs
  //    xor rhs
  //    bit 7, a
  //    jp z, .same_sign
  //    jp .different_sign
  // .different_sign:
  //    bit 7, lhs
  //    jp nz .target
  //    jp .fallthrough
  // .same_sign:
  //    ld a, lhs
  //    cp rhs
  //    jp nz .target
  //    jp .fallthrough
  // .target
  //    jp __target
  // .fallthrough
  //    jp __fallthrough

  auto *TrueTargetBB = MI.getOperand(3).getMBB();
  auto *InitBB = &MBB;
  auto *FalseBB = MBB.splitAt(MI);
  auto *TrueBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *SignsDifferBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *SignsMatchBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());

  InitBB->removeSuccessor(FalseBB);
  FalseBB->removeSuccessor(TrueTargetBB);

  auto MBBI = ++InitBB->getIterator();
  MF.insert(MBBI, SignsDifferBB);
  MF.insert(MBBI, SignsMatchBB);
  MF.insert(MBBI, TrueBB);

  MachineIRBuilder MIB(MI);
  auto Zero = MIB.buildConstant(S8, 0);
  bool RHSIsCheapToReuse = (UnsignedPredicate == ICmpInst::ICMP_UGE ||
                            UnsignedPredicate == ICmpInst::ICMP_ULT);

  Register XorReg;
  {
    // Build init
    Register CmpRes;
    if (auto ConstValue = getIConstantVRegValWithLookThrough(RHS, MRI);
        ConstValue.has_value()) {
      auto IsRHSPositive = (ConstValue->Value.getZExtValue() & 0x80) == 0;
      auto Masked = MIB.buildAnd(S8, LHS, MIB.buildConstant(S8, 0x80));
      CmpRes =
          MIB.buildICmp(IsRHSPositive ? CmpInst::ICMP_EQ : CmpInst::ICMP_NE, S8,
                        Masked, Zero)
              .getReg(0);
    } else {
      XorReg = MRI.createGenericVirtualRegister(S8);
      MIB.buildXor(XorReg, LHS, RHS);
      auto Masked = MIB.buildAnd(S8, XorReg, MIB.buildConstant(S8, 0x80));
      CmpRes = MIB.buildICmp(CmpInst::ICMP_EQ, S8, Masked, Zero).getReg(0);
    }

    InitBB->addSuccessor(SignsMatchBB);
    InitBB->addSuccessor(SignsDifferBB);
    MIB.buildBrCond(CmpRes, *SignsMatchBB);
    MIB.buildBr(*SignsDifferBB);
  }

  {
    // Build different_sign
    MIB.setInsertPt(*SignsDifferBB, SignsDifferBB->begin());

    if (auto ConstValue = getIConstantVRegValWithLookThrough(RHS, MRI);
        ConstValue.has_value()) {
      auto IsRHSPositive = (ConstValue->Value.getZExtValue() & 0x80) == 0;
      auto IsGT =
          Predicate == CmpInst::ICMP_SGE || Predicate == CmpInst::ICMP_SGT;

      MachineBasicBlock *Target = (IsRHSPositive ^ IsGT) ? TrueBB : FalseBB;
      SignsDifferBB->addSuccessor(Target);
      MIB.buildBr(*Target);
    } else {
      bool UseRHS = RHSIsCheapToReuse;
      auto MaskedLHS =
          MIB.buildAnd(S8, UseRHS ? RHS : LHS, MIB.buildConstant(S8, 0x80));
      auto CmpRes = MIB.buildICmp(UseRHS ? CmpInst::ICMP_NE : CmpInst::ICMP_EQ,
                                  S8, MaskedLHS, Zero);

      SignsDifferBB->addSuccessor(TrueBB);
      SignsDifferBB->addSuccessor(FalseBB);
      if (Predicate == CmpInst::ICMP_SGE || Predicate == CmpInst::ICMP_SGT) {
        MIB.buildBrCond(CmpRes, *TrueBB);
        MIB.buildBr(*FalseBB);
      } else {
        assert(Predicate == CmpInst::ICMP_SLE ||
               Predicate == CmpInst::ICMP_SLT);
        MIB.buildBrCond(CmpRes, *FalseBB);
        MIB.buildBr(*TrueBB);
      }
    }
  }

  {
    // Build same_sign
    MIB.setInsertPt(*SignsMatchBB, SignsMatchBB->begin());
    auto UnsignedCmp = [&] {
      if (getIConstantVRegValWithLookThrough(RHS, MRI).has_value()) {
        return MIB.buildICmp(UnsignedPredicate, S8, LHS, RHS);
      }

      // Reduce register pressure by reusing the xor result rather than keeping
      // track of both side of the comparison.
      // It is more efficient to reuse RHS if the branch is already
      // gb-canonical... Otherwise flip the logic and the branch.
      bool UseRHS = RHSIsCheapToReuse;
      auto NewPred = UseRHS ? UnsignedPredicate
                            : CmpInst::getSwappedPredicate(UnsignedPredicate);
      auto OtherSide = MIB.buildXor(S8, XorReg, UseRHS ? RHS : LHS);
      return MIB.buildICmp(NewPred, S8, OtherSide, UseRHS ? RHS : LHS);
    }();

    SignsMatchBB->addSuccessor(TrueBB);
    SignsMatchBB->addSuccessor(FalseBB);
    MIB.buildBrCond(UnsignedCmp, *TrueBB);
    MIB.buildBr(*FalseBB);
  }

  // Build true_bb (which only exits as a PHI src)
  MIB.setInsertPt(*TrueBB, TrueBB->begin());
  TrueTargetBB->replacePhiUsesWith(FalseBB, TrueBB);
  TrueBB->addSuccessor(TrueTargetBB);
  MIB.buildBr(*TrueTargetBB);

  MI.eraseFromParent();
  return true;
}

bool GBPostLegalizeExpand::expandMachineInstruction(MachineFunction &MF,
                                                    MachineBasicBlock &MBB,
                                                    MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default:
    return false;
  case GB::G_JP_CP:
    return expandJP_CP(MF, MBB, MI);
  }
}

bool GBPostLegalizeExpand::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "======== GBPostLegalizeExpand ========\n";
             dbgs() << "Starting with: "; MF.dump());

  bool DidMakeChanges = false;

restart_with_invalid_iterators:
  for (auto &MBB : MF) {
    for (auto &MI : MBB) {
      bool DidExpand = expandMachineInstruction(MF, MBB, MI);
      if (DidExpand) {
        DidMakeChanges = true;
        goto restart_with_invalid_iterators;
      }
    }
  }

  LLVM_DEBUG(
      dbgs() << "After GBPostLegalizeExpand:\n";
      if (DidMakeChanges) { MF.dump(); } else { dbgs() << "No change!\n"; });

  return DidMakeChanges;
}

char GBPostLegalizeExpand::ID = 0;
FunctionPass *llvm::createGBPostLegalizeExpand(CodeGenOptLevel) {
  return new GBPostLegalizeExpand();
}

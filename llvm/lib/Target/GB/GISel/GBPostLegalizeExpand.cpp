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
  auto Predicate = ICmpInst::Predicate(MI.getOperand(0).getPredicate());
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

  auto [UnsignedPredicate, Unsigned2sComplementPredicate] = [&] {
    switch (Predicate) {
    default:
      llvm_unreachable("Unrecognized predicate");
    case CmpInst::ICMP_SGE:
      return std::make_pair(CmpInst::ICMP_UGE, CmpInst::ICMP_ULE);
    case CmpInst::ICMP_SGT:
      return std::make_pair(CmpInst::ICMP_UGT, CmpInst::ICMP_ULT);
    case CmpInst::ICMP_SLE:
      return std::make_pair(CmpInst::ICMP_ULE, CmpInst::ICMP_UGE);
    case CmpInst::ICMP_SLT:
      return std::make_pair(CmpInst::ICMP_ULT, CmpInst::ICMP_UGT);
    }
  }();

  // This is a signed comparison... Let's break it into more branches.
  // TODO: come up with a way to make this faster... I think the old meth
  // We want
  // .init_bb
  //    br .lhs_is_pos_bb or .lhs_is_neg_bb

  // .lhs_is_pos_bb
  //    br .pos_signs_match_bb or .false_bb
  // .pos_signs_match_bb
  //    br  .false_bb or .true_bb

  // .lhs_is_neg_bb
  //    br .neg_signs_match_bb or .false_bb
  // .neg_signs_match_bb
  //    br .false_bb or .true_bb

  // .signs_match_bb
  //
  // .true_bb
  // .false_bb

  auto LHS = MI.getOperand(1).getReg();
  auto RHS = MI.getOperand(2).getReg();
  auto *TrueBB = MI.getOperand(3).getMBB();
  auto *InitBB = &MBB;
  auto *FalseBB = MBB.splitAt(MI);
  auto *LHSIsPositiveBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *LHSIsNegativeBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *PosSignsMatchBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *NegSignsMatchBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());

  InitBB->removeSuccessor(FalseBB);
  FalseBB->removeSuccessor(TrueBB);

  auto MBBI = ++InitBB->getIterator();
  MF.insert(MBBI, LHSIsPositiveBB);
  MF.insert(MBBI, PosSignsMatchBB);
  MF.insert(MBBI, LHSIsNegativeBB);
  MF.insert(MBBI, NegSignsMatchBB);

  auto MaybeLHSConst = getIConstantVRegValWithLookThrough(LHS, MRI);
  auto MaybeRHSConst = getIConstantVRegValWithLookThrough(RHS, MRI);
  assert(!(MaybeLHSConst && MaybeRHSConst));

  MachineIRBuilder MIB(MI);

  // Build init
  auto Zero = MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 0);
  auto SignBitMask =
      MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 0x80);
  auto LHSSignBit =
      MIB.buildAnd(MRI.createGenericVirtualRegister(S8), LHS, SignBitMask);
  auto LHSSignBitCmp =
      MIB.buildICmp(CmpInst::ICMP_EQ, MRI.createGenericVirtualRegister(S8),
                    LHSSignBit.getReg(0), Zero);

  InitBB->addSuccessor(LHSIsPositiveBB);
  InitBB->addSuccessor(LHSIsNegativeBB);
  MIB.buildBrCond(LHSSignBitCmp, *LHSIsPositiveBB);
  MIB.buildBr(*LHSIsNegativeBB);

  // Build lhs_is_pos_bb
  MIB.setInsertPt(*LHSIsPositiveBB, LHSIsPositiveBB->begin());
  auto RHSSignBit =
      MIB.buildAnd(MRI.createGenericVirtualRegister(S8), RHS, SignBitMask);
  auto RHSSignBitCmp =
      MIB.buildICmp(CmpInst::ICMP_NE, MRI.createGenericVirtualRegister(S8),
                    RHSSignBit.getReg(0), Zero);
  //    If RHSSignBitCmp == 0, both are positive, otherwise RHS is negative
  if (Predicate == CmpInst::ICMP_SGT || Predicate == CmpInst::ICMP_SGE) {
    LHSIsPositiveBB->addSuccessor(TrueBB);
    MIB.buildBrCond(RHSSignBitCmp, *TrueBB);
  } else {
    assert(Predicate == CmpInst::ICMP_SLT || Predicate == CmpInst::ICMP_SLE);
    LHSIsPositiveBB->addSuccessor(FalseBB);
    MIB.buildBrCond(RHSSignBitCmp, *FalseBB);
  }
  LHSIsPositiveBB->addSuccessor(PosSignsMatchBB);
  MIB.buildBr(*PosSignsMatchBB);

  // Build pos_signs_match_bb
  MIB.setInsertPt(*PosSignsMatchBB, PosSignsMatchBB->begin());
  auto UnsignedCmp = MIB.buildICmp(
      UnsignedPredicate, MRI.createGenericVirtualRegister(S8), LHS, RHS);

  PosSignsMatchBB->addSuccessor(TrueBB);
  PosSignsMatchBB->addSuccessor(FalseBB);
  MIB.buildBrCond(UnsignedCmp.getReg(0), *TrueBB);
  MIB.buildBr(*FalseBB);

  // Build lhs_is_neg_bb
  MIB.setInsertPt(*LHSIsNegativeBB, LHSIsNegativeBB->begin());
  RHSSignBit =
      MIB.buildAnd(MRI.createGenericVirtualRegister(S8), RHS, SignBitMask);
  RHSSignBitCmp =
      MIB.buildICmp(CmpInst::ICMP_EQ, MRI.createGenericVirtualRegister(S8),
                    RHSSignBit.getReg(0), Zero);
  //    If RHSSignBitCmp == 0, both are negative, otherwise RHS is positive
  if (Predicate == CmpInst::ICMP_SGT || Predicate == CmpInst::ICMP_SGE) {
    LHSIsNegativeBB->addSuccessor(FalseBB);
    MIB.buildBrCond(RHSSignBitCmp, *FalseBB);
  } else {
    assert(Predicate == CmpInst::ICMP_SLT || Predicate == CmpInst::ICMP_SLE);
    LHSIsNegativeBB->addSuccessor(TrueBB);
    MIB.buildBrCond(RHSSignBitCmp, *TrueBB);
  }
  LHSIsNegativeBB->addSuccessor(NegSignsMatchBB);
  MIB.buildBr(*NegSignsMatchBB);

  // Build neg_signs_match_bb
  MIB.setInsertPt(*NegSignsMatchBB, NegSignsMatchBB->begin());
  UnsignedCmp = MIB.buildICmp(Unsigned2sComplementPredicate,
                              MRI.createGenericVirtualRegister(S8), LHS, RHS);
  NegSignsMatchBB->addSuccessor(TrueBB);
  NegSignsMatchBB->addSuccessor(FalseBB);
  MIB.buildBrCond(UnsignedCmp.getReg(0), *TrueBB);
  MIB.buildBr(*FalseBB);

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
  return DidMakeChanges;
}

char GBPostLegalizeExpand::ID = 0;
FunctionPass *llvm::createGBPostLegalizeExpand(CodeGenOptLevel) {
  return new GBPostLegalizeExpand();
}

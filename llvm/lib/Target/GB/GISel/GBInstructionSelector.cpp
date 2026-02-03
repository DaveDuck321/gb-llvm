#include "GB.h"
#include "GBInstrInfo.h"
#include "GBMOFlags.h"
#include "GBRegisterInfo.h"
#include "GBTargetMachine.h"
#include "GISel/GBCombinerCommon.h"
#include "GISel/GBRegisterBankInfo.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/Bitset.h"
#include "llvm/CodeGen/GlobalISel/GIMatchTableExecutorImpl.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelector.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/CmpPredicate.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CodeGenCoverage.h"
#include "llvm/Support/ErrorHandling.h"

#define DEBUG_TYPE "gb-isel"

using namespace llvm;

#define GET_GLOBALISEL_PREDICATE_BITSET
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATE_BITSET

namespace {
class GBInstructionSelector : public InstructionSelector {
  const GBRegisterBankInfo &RBI;
  const TargetInstrInfo &TII;
  const TargetRegisterInfo &TRI;

  void constrainGeneric(Register Reg, MachineRegisterInfo &MRI) const;

public:
  GBInstructionSelector(const GBSubtarget &STI, const GBRegisterBankInfo &);

  static const char *getName() { return DEBUG_TYPE; }

  bool select(MachineInstr &MI) override;
  bool selectImpl(MachineInstr &MI, CodeGenCoverage &CoverageInfo) const;

  bool selectConstant(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectPhi(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectCopy(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectUnmerge(MachineInstr &MI, MachineRegisterInfo &MRI) const;

  bool selectFrameIndex(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectFence(MachineInstr &MI, MachineRegisterInfo &MRI) const;

  bool selectAddress(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectJP_CP(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectJP_BINARY_OP(MachineInstr &MI, MachineRegisterInfo &MRI) const;
  bool selectICMP(MachineInstr &MI, MachineRegisterInfo &MRI) const;

private:
#define GET_GLOBALISEL_PREDICATES_DECL
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATES_DECL

#define GET_GLOBALISEL_TEMPORARIES_DECL
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_TEMPORARIES_DECL
};
} // namespace

#define GET_GLOBALISEL_IMPL
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_IMPL

GBInstructionSelector::GBInstructionSelector(const GBSubtarget &STI,
                                             const GBRegisterBankInfo &RBI)
    : RBI{RBI}, TII{*STI.getInstrInfo()}, TRI{*STI.getRegisterInfo()},
#define GET_GLOBALISEL_PREDICATES_INIT
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_PREDICATES_INIT
#define GET_GLOBALISEL_TEMPORARIES_INIT
#include "GBGenGlobalISel.inc"
#undef GET_GLOBALISEL_TEMPORARIES_INIT
{
}

bool GBInstructionSelector::select(MachineInstr &MI) {
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  auto DL = MI.getDebugLoc();

  switch (MI.getOpcode()) {
  case TargetOpcode::COPY:
    return selectCopy(MI, MRI);

  case TargetOpcode::G_UNMERGE_VALUES:
    return selectUnmerge(MI, MRI);

  case TargetOpcode::G_INTTOPTR:
  case TargetOpcode::G_PTRTOINT:
  case TargetOpcode::G_BITCAST:
  case TargetOpcode::G_FREEZE: {
    MI.setDesc(TII.get(GB::COPY));
    return selectCopy(MI, MRI);
  }
  case GB::G_JP_CP:
    return selectJP_CP(MI, MRI);

  case GB::G_JP_BINARY_OP:
    return selectJP_BINARY_OP(MI, MRI);

  case TargetOpcode::G_ICMP:
    return selectICMP(MI, MRI);

  case TargetOpcode::G_BLOCK_ADDR:
  case TargetOpcode::G_GLOBAL_VALUE:
  case TargetOpcode::G_CONSTANT_POOL:
    return selectAddress(MI, MRI);

  case TargetOpcode::G_CONSTANT:
    return selectConstant(MI, MRI);

  case TargetOpcode::G_PHI:
    return selectPhi(MI, MRI);

  case TargetOpcode::G_FRAME_INDEX:
    return selectFrameIndex(MI, MRI);

  case TargetOpcode::G_FENCE:
    return selectFence(MI, MRI);

  case TargetOpcode::G_IMPLICIT_DEF:
    MI.setDesc(TII.get(TargetOpcode::IMPLICIT_DEF));
    constrainGeneric(MI.getOperand(0).getReg(), MRI);
    return true;

  default:
    break; // Continue to tablegen
  }

  if (!isPreISelGenericOpcode(MI.getOpcode())) {
    return true;
  }

  if (selectImpl(MI, *CoverageInfo)) {
    return true;
  }

  return false;
}

void GBInstructionSelector::constrainGeneric(Register Reg,
                                             MachineRegisterInfo &MRI) const {
  if (Reg.isPhysical()) {
    return;
  }

  const LLT Ty = MRI.getType(Reg);
  const unsigned TySize = Ty.getSizeInBits();
  auto *RC = TySize == 8 ? &GB::GPR8RegClass : &GB::GPR16RegClass;
  if (!RBI.constrainGenericRegister(Reg, *RC, MRI)) {
    llvm_unreachable("Failed to constrain");
  }
}

bool GBInstructionSelector::selectConstant(MachineInstr &MI,
                                           MachineRegisterInfo &MRI) const {
  assert(MI.getOpcode() == TargetOpcode::G_CONSTANT);
  Register DstReg = MI.getOperand(0).getReg();
  auto Value = MI.getOperand(1).getCImm()->getZExtValue();
  MI.removeOperand(1);

  constrainGeneric(DstReg, MRI);
  if (MRI.getType(DstReg).getSizeInBits() == 8) {
    MI.setDesc(TII.get(GB::LDI8_r));
  } else {
    MI.setDesc(TII.get(GB::LDI16_r));
  }
  MI.addOperand(MachineOperand::CreateImm(Value));
  return true;
}

bool GBInstructionSelector::selectPhi(MachineInstr &MI,
                                      MachineRegisterInfo &MRI) const {
  assert(MI.getOpcode() == TargetOpcode::G_PHI);
  Register DstReg = MI.getOperand(0).getReg();
  MI.setDesc(TII.get(TargetOpcode::PHI));
  constrainGeneric(DstReg, MRI);
  return true;
}

bool GBInstructionSelector::selectCopy(MachineInstr &MI,
                                       MachineRegisterInfo &MRI) const {
  assert(MI.getOpcode() == TargetOpcode::COPY);
  Register SrcReg = MI.getOperand(1).getReg();
  Register DstReg = MI.getOperand(0).getReg();
  constrainGeneric(SrcReg, MRI);
  constrainGeneric(DstReg, MRI);
  return true;
}

bool GBInstructionSelector::selectUnmerge(MachineInstr &MI,
                                          MachineRegisterInfo &MRI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  assert(MI.getNumOperands() == 3);

  auto Lower = MI.getOperand(0).getReg();
  auto Upper = MI.getOperand(1).getReg();
  auto Src = MI.getOperand(2);

  if (MRI.getType(Lower).getSizeInBits() != 8 ||
      MRI.getType(Upper).getSizeInBits() != 8 ||
      MRI.getType(Src.getReg()).getSizeInBits() != 16) {
    llvm_unreachable("Legalizer produced illegal unmerge");
  }

  constrainGeneric(Lower, MRI);
  constrainGeneric(Upper, MRI);
  constrainGeneric(Src.getReg(), MRI);

  auto SrcSub1 = Src;
  SrcSub1.setSubReg(1);

  auto SrcSub2 = Src;
  SrcSub2.setSubReg(2);

  BuildMI(MBB, MI, DL, TII.get(TargetOpcode::COPY), Lower).add(SrcSub1);
  BuildMI(MBB, MI, DL, TII.get(TargetOpcode::COPY), Upper).add(SrcSub2);

  MI.eraseFromParent();
  return true;
}

bool GBInstructionSelector::selectFrameIndex(MachineInstr &MI,
                                             MachineRegisterInfo &MRI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();
  auto Dst = MI.getOperand(0).getReg();
  auto FrameIndex = MI.getOperand(1).getIndex();

  constrainGeneric(Dst, MRI);
  BuildMI(MBB, MI, DL, TII.get(GB::LD_HL_SP)).addFrameIndex(FrameIndex);
  BuildMI(MBB, MI, DL, TII.get(GB::COPY), Dst)
      .addUse(GB::HL, getKillRegState(true));

  MI.eraseFromParent();
  return true;
}

bool GBInstructionSelector::selectFence(MachineInstr &MI,
                                        MachineRegisterInfo &MRI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  const unsigned Flags = InlineAsm::Extra_MayLoad | InlineAsm::Extra_MayStore |
                         InlineAsm::Extra_HasSideEffects;
  BuildMI(MBB, MI, DL, TII.get(GB::INLINEASM))
      .add(MachineOperand::CreateES(MF->createExternalSymbolName("")))
      .addImm(Flags);
  MI.eraseFromParent();
  return true;
}

bool GBInstructionSelector::selectAddress(MachineInstr &MI,
                                          MachineRegisterInfo &MRI) const {
  auto &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  auto Dst = MI.getOperand(0).getReg();
  auto Target = MI.getOperand(1);

  unsigned Opcode = [&] {
    auto OpFlag = getGBFlag(Target);
    if (OpFlag == llvm::GBMOFlag::UPPER_PART ||
        OpFlag == llvm::GBMOFlag::LOWER_PART) {
      // 8-bit global
      return GB::LDI8_r;
    }
    return GB::LDI16_r;
  }();
  constrainGeneric(Dst, MRI);
  BuildMI(MBB, MI, DL, TII.get(Opcode), Dst).add(Target);
  MI.eraseFromParent();
  return true;
}

bool GBInstructionSelector::selectICMP(MachineInstr &MI,
                                       MachineRegisterInfo &MRI) const {
  assert(MI.getOpcode() == TargetOpcode::G_ICMP);
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  Register Result = MI.getOperand(0).getReg();
  auto Predicate = CmpInst::Predicate(MI.getOperand(1).getPredicate());
  auto LHS = MI.getOperand(2).getReg();
  auto RHS = MI.getOperand(3).getReg();
  constrainGeneric(LHS, MRI);
  constrainGeneric(RHS, MRI);
  constrainGeneric(Result, MRI);

  bool ReverseResult = false;
  switch (Predicate) {
  default:
    // ICMP_EQ/ ICMP_NE have already been legalized into a subtract
    llvm_unreachable("unrecognized predicate");
  case CmpInst::ICMP_UGT:
    std::swap(LHS, RHS);
    break;
  case CmpInst::ICMP_ULT:
    break;

  case CmpInst::ICMP_UGE:
    ReverseResult = true;
    break;
  case CmpInst::ICMP_ULE:
    std::swap(LHS, RHS);
    ReverseResult = true;
    break;
  }

  BuildMI(MBB, MI, DL, TII.get(GB::COPY)).addDef(GB::A).addReg(LHS);
  if (auto Imm = getIConstantVRegValWithLookThrough(RHS, MRI)) {
    BuildMI(MBB, MI, DL, TII.get(GB::CPI)).addImm(Imm->Value.getZExtValue());
  } else {
    BuildMI(MBB, MI, DL, TII.get(GB::CP_r)).addReg(RHS);
  }

  if (ReverseResult) {
    BuildMI(MBB, MI, DL, TII.get(GB::CCF));
  }
  BuildMI(MBB, MI, DL, TII.get(GB::RL_r))
      .addDef(Result)
      .addReg(MRI.createVirtualRegister(&GB::GPR8RegClass),
              getUndefRegState(true));

  MI.eraseFromParent();
  return true;
}

bool GBInstructionSelector::selectJP_CP(MachineInstr &MI,
                                        MachineRegisterInfo &MRI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  auto Predicate = CmpInst::Predicate(MI.getOperand(0).getPredicate());
  auto Src1 = MI.getOperand(1).getReg();
  auto Src2 = MI.getOperand(2).getReg();
  auto Dst = MI.getOperand(3);

  constrainGeneric(Src1, MRI);
  constrainGeneric(Src2, MRI);

  auto MaybeMoveConstantToSrc2 = [&] {
    if (auto Imm = getIConstantVRegValWithLookThrough(Src2, MRI)) {
      // Src2 is already constant... Nothing to do
      return;
    }

    if (auto Imm = getIConstantVRegValWithLookThrough(Src2, MRI)) {
      // Src1 is constant, Src2 isn't... flip them
      std::swap(Src1, Src2);
    }
  };

  auto MapSimpleUCP = [&](GBFlag::NodeType Flag, auto LHS, auto RHS) {
    BuildMI(MBB, MI, DL, TII.get(GB::COPY), GB::A).addReg(LHS);
    if (auto Imm = getIConstantVRegValWithLookThrough(RHS, MRI)) {
      // TODO: count uses of Imm... Sometime we shouldn't const fold
      BuildMI(MBB, MI, DL, TII.get(GB::CPI))
          .addImm(Imm->Value.getZExtValue())
          .addReg(GB::A, getImplRegState(true) | getKillRegState(true));
    } else {
      BuildMI(MBB, MI, DL, TII.get(GB::CP_r))
          .addReg(RHS)
          .addReg(GB::A, getImplRegState(true) | getKillRegState(true));
    }
    BuildMI(MBB, MI, DL, TII.get(GB::JP_COND)).addImm(Flag).add(Dst);
    MI.eraseFromParent();
    return true;
  };

  switch (Predicate) {
  default:
    llvm_unreachable("Unexpected predicate");

  case CmpInst::ICMP_EQ:
    MaybeMoveConstantToSrc2();
    return MapSimpleUCP(GBFlag::Z, Src1, Src2);
  case CmpInst::ICMP_NE:
    MaybeMoveConstantToSrc2();
    return MapSimpleUCP(GBFlag::NZ, Src1, Src2);

  case CmpInst::ICMP_UGE:
    return MapSimpleUCP(GBFlag::NC, Src1, Src2);
  case CmpInst::ICMP_UGT:
    return MapSimpleUCP(GBFlag::C, Src2, Src1);

  case CmpInst::ICMP_ULE:
    return MapSimpleUCP(GBFlag::NC, Src2, Src1);
  case CmpInst::ICMP_ULT:
    return MapSimpleUCP(GBFlag::C, Src1, Src2);
  }
  return true;
}

bool GBInstructionSelector::selectJP_BINARY_OP(MachineInstr &MI,
                                               MachineRegisterInfo &MRI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  auto DL = MI.getDebugLoc();

  auto Flag = MI.getOperand(0).getImm();
  auto Target = MI.getOperand(1);

  auto Opcode = MI.getOperand(2).getImm();
  auto Builder = BuildMI(MBB, MI, DL, TII.get(Opcode));
  for (size_t Operand = 3; Operand < MI.getNumOperands(); Operand += 1) {
    Builder.add(MI.getOperand(Operand));
  }
  BuildMI(MBB, MI, DL, TII.get(GB::JP_COND)).addImm(Flag).add(Target);
  MI.eraseFromParent();
  return true;
}

namespace llvm {
InstructionSelector *
createGBInstructionSelector(const GBSubtarget &Subtarget,
                            const GBRegisterBankInfo &RBI) {
  return new GBInstructionSelector(Subtarget, RBI);
}
} // end namespace llvm

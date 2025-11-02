#include "GBInstrInfo.h"
#include "GB.h"
#include "GBMOFlags.hpp"
#include "GBRegisterInfo.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include <optional>

#define GET_INSTRINFO_CTOR_DTOR
#include "GBGenInstrInfo.inc"

#define DEBUG_TYPE "gb-stack-slot-lowering"

using namespace llvm;

GBInstrInfo::GBInstrInfo()
    : GBGenInstrInfo(GB::ADJCALLSTACKDOWN, GB::ADJCALLSTACKUP) {}

void GBInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MBBI,
                              const DebugLoc &DL, Register DestReg,
                              Register SrcReg, bool KillSrc, bool RenamableDest,
                              bool RenamableSrc) const {

  // 8-bit copy
  if (GB::GPR8RegClass.contains(SrcReg, DestReg)) {
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }

  // 16-bit copy (copy lower half then upper half)
  // Applies to BC, DE, HL, (but not AF, SP)
  const auto IsSimpleCombinedGPR16 = [](MCRegister Reg, unsigned &Top,
                                        unsigned &Bottom) {
    switch (Reg) {
    case GB::BC:
      Top = GB::B;
      Bottom = GB::C;
      return true;
    case GB::DE:
      Top = GB::D;
      Bottom = GB::E;
      return true;
    case GB::HL:
      Top = GB::H;
      Bottom = GB::L;
      return true;
    default:
      return false;
    }
  };

  if (unsigned SrcA, SrcB, DestA, DestB;
      IsSimpleCombinedGPR16(SrcReg, SrcA, SrcB) &&
      IsSimpleCombinedGPR16(DestReg, DestA, DestB)) {

    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestA)
        .addReg(SrcA, getKillRegState(KillSrc));
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestB)
        .addReg(SrcB, getKillRegState(KillSrc));
    return;
  }

  dbgs() << SrcReg << "\n";
  dbgs() << DestReg << "\n";
  llvm_unreachable("Unsupported register copy!");
}

Register GBInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                          int &FrameIndex) const {
  switch (MI.getOpcode()) {
  case GB::Load8FromFrameIndex:
  case GB::Load16FromFrameIndex:
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  default:
    return 0;
  }
}

Register GBInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                         int &FrameIndex) const {
  switch (MI.getOpcode()) {
  case GB::Save8ToFrameIndex:
  case GB::Save16ToFrameIndex:
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  default:
    return false;
  }
}

void GBInstrInfo::storeRegToStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI, Register SrcReg,
    bool IsKill, int FrameIndex, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI, Register VReg,
    MachineInstr::MIFlag Flags) const {
  LLVM_DEBUG(dbgs() << "Added StoreToStackSlot: " << printReg(SrcReg, TRI)
                    << " slot." << FrameIndex << "\n");

  DebugLoc DL = MBB.findDebugLoc(MI);

  // TODO GB: The GameBoy is really ill-suited for this constant stack offset...
  // find a way to only use push/ pops (almost exclusively?)

  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FrameIndex),
      MachineMemOperand::MOStore, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlign(FrameIndex));

  if (GB::GPR8RegClass.hasSubClassEq(RC)) {
    BuildMI(MBB, MI, DL, get(GB::Save8ToFrameIndex))
        .addReg(SrcReg, getKillRegState(IsKill))
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    return;
  }

  if (GB::IntReg16RegClass.hasSubClassEq(RC)) {
    BuildMI(MBB, MI, DL, get(GB::Save16ToFrameIndex))
        .addReg(SrcReg, getKillRegState(IsKill))
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    return;
  }
  llvm_unreachable("Could not save reg to stack slot!");
}

void GBInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MI,
                                       Register DestReg, int FrameIndex,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo *TRI,
                                       Register VReg,
                                       MachineInstr::MIFlag Flags) const {
  LLVM_DEBUG(dbgs() << "Added LoadFromStackSlot: " << printReg(DestReg, TRI)
                    << " slot." << FrameIndex << "\n");
  DebugLoc DL = MBB.findDebugLoc(MI);

  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FrameIndex),
      MachineMemOperand::MOLoad, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlign(FrameIndex));

  if (GB::GPR8RegClass.hasSubClassEq(RC)) {
    BuildMI(MBB, MI, DL, get(GB::Load8FromFrameIndex), DestReg)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    return;
  }
  if (GB::IntReg16RegClass.hasSubClassEq(RC)) {
    BuildMI(MBB, MI, DL, get(GB::Load16FromFrameIndex), DestReg)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    return;
  }
  llvm_unreachable("Could not load reg to stack slot!");
}

unsigned GBInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  if (MI.isMetaInstruction()) {
    return 0;
  }

  if (MI.isInlineAsm()) {
    // Default conservatively to the largest possible instruction size
    return 3;
  }

  if (MI.isPseudo()) {
    LLVM_DEBUG(
        dbgs() << "Warning: analysis requires size of pseudo instruction\n");
    return 3;
  }
  return MI.getDesc().getSize();
}

bool GBInstrInfo::isBranchOffsetInRange(unsigned BranchOpc,
                                        int64_t BrOffset) const {
  switch (BranchOpc) {
  case GB::JR:
  case GB::JR_COND:
    return isInt<8>(BrOffset);
  case GB::JP:
  case GB::JP_COND:
    return true;
  default:
    LLVM_DEBUG(dbgs() << BranchOpc << "\n");
    llvm_unreachable("Unrecognised BranchOpc");
  }
}

MachineBasicBlock *
GBInstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case GB::JR:
  case GB::JP:
    return MI.getOperand(0).getMBB();
  case GB::JR_COND:
  case GB::JP_COND:
    return MI.getOperand(1).getMBB();
  default:
    LLVM_DEBUG(MI.print(dbgs()));
    llvm_unreachable("Unrecognised MI");
  }
}

bool GBInstrInfo::tightenBranchIfPossible(MachineInstr &CurrentMI,
                                          int64_t BranchOffset) const {
  auto TighterMI = [&]() -> std::optional<unsigned> {
    switch (CurrentMI.getOpcode()) {
    case GB::JR:
    case GB::JR_COND:
      return std::nullopt;
    case GB::JP:
      return GB::JR;
    case GB::JP_COND:
      return GB::JR_COND;
    default:
      llvm_unreachable("Unrecognised MI");
    }
  }();

  if (not TighterMI.has_value() ||
      not isBranchOffsetInRange(*TighterMI, BranchOffset)) {
    return false;
  }

  CurrentMI.setDesc(get(*TighterMI));
  return true;
}

bool GBInstrInfo::analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                                MachineBasicBlock *&FBB,
                                SmallVectorImpl<MachineOperand> &Cond,
                                bool AllowModify) const {

  MachineInstr *FinalConditionalBranch = nullptr;
  MachineInstr *FinalUnconditionalBranch = nullptr;
  for (auto &MI : *MBB.getIterator()) {
    if (MI.isDebugInstr()) {
      continue;
    }

    if (MI.isUnconditionalBranch()) {
      FinalUnconditionalBranch = &MI;
      break;
    }

    if (MI.isConditionalBranch()) {
      FinalConditionalBranch = &MI;
    } else {
      // TODO GB: should I relax this requirement?
      FinalConditionalBranch = nullptr;
    }

    if (MI.isBarrier()) {
      return true; // Cannot analyze
    }
  }

  /// 1. If this block ends with no branches (it just falls through to its succ)
  ///    just return false, leaving TBB/FBB null.
  if (FinalUnconditionalBranch == nullptr &&
      FinalConditionalBranch == nullptr) {
    return false;
  }

  /// 2. If this block ends with only an unconditional branch, it sets TBB to be
  ///    the destination block.
  if (FinalConditionalBranch == nullptr) {
    TBB = FinalUnconditionalBranch->getOperand(0).getMBB();
    return false;
  }

  /// 3. If this block ends with a conditional branch and it falls through to a
  ///    successor block, it sets TBB to be the branch destination block and a
  ///    list of operands that evaluate the condition. These operands can be
  ///    passed to other TargetInstrInfo methods to create new branches.
  if (FinalUnconditionalBranch == nullptr &&
      FinalConditionalBranch != nullptr) {
    TBB = FinalConditionalBranch->getOperand(1).getMBB();
    Cond.clear();
    Cond.push_back(FinalConditionalBranch->getOperand(0));
    return false;
  }

  /// 4. If this block ends with a conditional branch followed by an
  ///    unconditional branch, it returns the 'true' destination in TBB, the
  ///    'false' destination in FBB, and a list of operands that evaluate the
  ///    condition.  These operands can be passed to other TargetInstrInfo
  ///    methods to create new branches.
  assert(FinalUnconditionalBranch != nullptr and
         FinalUnconditionalBranch != nullptr);

  TBB = FinalConditionalBranch->getOperand(1).getMBB();
  FBB = FinalUnconditionalBranch->getOperand(0).getMBB();
  Cond.clear();
  Cond.push_back(FinalConditionalBranch->getOperand(0));
  return false;
}

unsigned GBInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                   int *BytesRemoved) const {
  unsigned BranchesRemoved = 0;
  if (BytesRemoved != nullptr) {
    *BytesRemoved = 0;
  }

  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  while (not MBB.empty() && I != MBB.end()) {
    if (not I->isBranch() || I->isIndirectBranch()) {
      break;
    }
    if (BytesRemoved != nullptr) {
      *BytesRemoved += getInstSizeInBytes(*I);
    }
    BranchesRemoved += 1;
    I->eraseFromParent();
    I = MBB.getLastNonDebugInstr();
  }
  return BranchesRemoved;
}

unsigned GBInstrInfo::insertBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *TBB,
                                   MachineBasicBlock *FBB,
                                   ArrayRef<MachineOperand> Cond,
                                   const DebugLoc &DL, int *BytesAdded) const {
  size_t SizeInBytes = 0;
  if (Cond.empty()) {
    // Unconditional branch
    MachineInstr &MI = *BuildMI(&MBB, DL, get(GB::JP)).addMBB(TBB);
    SizeInBytes += getInstSizeInBytes(MI);
    if (BytesAdded != nullptr) {
      *BytesAdded = SizeInBytes;
    }
    return 1;
  }

  // Conditional branches
  MachineInstr &TrueBranchMI =
      *BuildMI(&MBB, DL, get(GB::JP_COND)).addImm(Cond[0].getImm()).addMBB(TBB);
  SizeInBytes += getInstSizeInBytes(TrueBranchMI);
  if (BytesAdded != nullptr) {
    *BytesAdded = SizeInBytes;
  }

  if (FBB == nullptr) {
    return 1;
  }

  MachineInstr &FalseBranchMI = *BuildMI(&MBB, DL, get(GB::JP)).addMBB(FBB);
  SizeInBytes += getInstSizeInBytes(FalseBranchMI);
  if (BytesAdded != nullptr) {
    *BytesAdded = SizeInBytes;
  }
  return 2;
}

bool GBInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 1);
  Cond[0].setImm([&] {
    switch (Cond[0].getImm() & 0b11) {
    case GBFlag::C:
      return GBFlag::NC;
    case GBFlag::NC:
      return GBFlag::C;
    case GBFlag::Z:
      return GBFlag::NZ;
    case GBFlag::NZ:
      return GBFlag::Z;
    default:
      llvm_unreachable("Unrecognized branch flag");
    }
  }());
  return false;
}

bool GBInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  auto *MBB = MI.getParent();
  auto *TRI = MBB->getParent()->getRegInfo().getTargetRegisterInfo();
  auto MBBI = MI.getIterator();
  auto DL = MI.getDebugLoc();

  switch (MI.getOpcode()) {
  // During the pattern match, we produce LD_r_iGPR16 and LD_iGPR16_A
  // LD_r_iGPR16 is lowered to L_A_iHL or LD_A_iR16 depending on the 16-bit
  // source register. This can be improved  by detecting cases where L_r8_iHL
  // would be more optimal than L_A_iHL.
  case GB::LD_r_iGPR16: {
    Register DstReg = MI.getOperand(0).getReg();
    Register PtrReg = MI.getOperand(1).getReg();
    bool DidKillPtr = MI.killsRegister(PtrReg, TRI);

    switch (PtrReg) {
    default:
      llvm_unreachable("Unexpected register");
    case GB::HL:
      // Note: we always produce LD A, (HL) here rather than loading directly to
      // the target GPR since this allows us to produce peephole optimizations
      // like LDH.
      BuildMI(*MBB, MBBI, DL, get(GB::LD_r_iHL), GB::A)
          .addReg(GB::HL, getImplRegState(true) | getKillRegState(DidKillPtr));
      break;
    case GB::BC:
    case GB::DE:
      BuildMI(*MBB, MBBI, DL, get(GB::LD_A_iR16))
          .addDef(GB::A, getImplRegState(true))
          .addReg(PtrReg, getKillRegState(DidKillPtr));
      break;
    }
    if (DstReg != GB::A) {
      BuildMI(*MBB, MBBI, DL, get(GB::LD_rr), DstReg)
          .addReg(GB::A, getKillRegState(true));
    }
    MI.eraseFromParent();
    return true;
  }
  case GB::LD_iGPR16_A: {
    Register PtrReg = MI.getOperand(0).getReg();
    Register SrcReg = MI.getOperand(1).getReg();
    bool DidKillPtr = MI.killsRegister(PtrReg, TRI);
    bool DidKillSrc = MI.killsRegister(SrcReg, TRI);

    switch (PtrReg) {
    default:
      llvm_unreachable("Unexpected register");
    case GB::HL:
      BuildMI(*MBB, MBBI, DL, get(GB::LD_iHL_r))
          .addReg(GB::A, getKillRegState(DidKillSrc))
          .addReg(GB::HL, getImplRegState(true) | getKillRegState(DidKillPtr));
      break;
    case GB::BC:
    case GB::DE:
      BuildMI(*MBB, MBBI, DL, get(GB::LD_iR16_A))
          .addReg(PtrReg, getKillRegState(DidKillPtr))
          .addReg(GB::A, getImplRegState(true) | getKillRegState(DidKillSrc));
      break;
    }
    MI.eraseFromParent();
    return true;
  }
  }
  return false;
}

bool GBInstrInfo::getConstValDefinedInReg(const MachineInstr &MI,
                                          const Register Reg,
                                          int64_t &ImmVal) const {
  MachineFunction const *MF = MI.getParent()->getParent();
  TargetRegisterInfo const *TRI = MF->getRegInfo().getTargetRegisterInfo();
  switch (MI.getOpcode()) {
  default:
    return false;
  case GB::LDI16:
  case GB::LDI8_r:
    if (not MI.getOperand(1).isImm()) {
      return false;
    }

    assert(MI.definesRegister(Reg, TRI));
    ImmVal = MI.getOperand(1).getImm();
    return true;
  }
}

#undef DEBUG_TYPE
#define DEBUG_TYPE "gb-folding"

bool GBInstrInfo::foldAddressImmediate(MachineInstr &UseMI, MachineOperand &Imm,
                                       Register Reg,
                                       MachineRegisterInfo *MRI) const {
  auto *MBB = UseMI.getParent();
  auto MBBI = UseMI.getIterator();
  auto DL = UseMI.getDebugLoc();

  switch (UseMI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Fold failed: unsupported use opcode "
                      << UseMI.getOpcode() << "\n");
    return false;
  case GB::LD_r_iGPR16:
    BuildMI(*MBB, MBBI, DL, get(GB::LD_A_iImm))
        .addDef(GB::A, getImplRegState(true))
        ->addOperand(Imm);

    BuildMI(*MBB, MBBI, DL, get(GB::COPY), UseMI.getOperand(0).getReg())
        .addReg(GB::A, getKillRegState(true));

    break;
  case GB::LD_iGPR16_A:
    BuildMI(*MBB, MBBI, DL, get(GB::LD_iImm_A))
        .addReg(GB::A, getImplRegState(true))
        ->addOperand(Imm);
    break;
  }
  assert(Imm.isImm() || Imm.isSymbol() || Imm.isGlobal());
  return true;
}

bool GBInstrInfo::fold8BitImmediate(MachineInstr &UseMI,
                                    MachineOperand &DefMIImmOperand,
                                    Register Reg,
                                    MachineRegisterInfo *MRI) const {
  auto *MBB = UseMI.getParent();
  auto MBBI = UseMI.getIterator();
  auto DL = UseMI.getDebugLoc();

  unsigned FoldedOpcode = 0;
  MachineOperand Operand = DefMIImmOperand;
  switch (UseMI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Fold failed: unsupported use opcode "
                      << UseMI.getOpcode() << " ";
               UseMI.print(dbgs()));
    return false;
  case GB::COPY: {
    auto UseOp = UseMI.getOperand(1);
    auto SubReg = UseOp.getSubReg();

    FoldedOpcode = GB::LDI8_r;
    assert(UseOp.getReg() == Reg);

    if (MRI->getRegClassOrNull(Reg) != &GB::GPR8RegClass) {
      if (SubReg == 1) {
        setGBFlag(Operand, GBMOFlag::LOWER_PART);
      } else if (SubReg == 2) {
        setGBFlag(Operand, GBMOFlag::UPPER_PART);
      } else {
        LLVM_DEBUG(
            dbgs()
                << "Fold failed: cannot fold into 16-bit register assignment: "
                << UseMI.getOpcode() << " ";
            UseMI.print(dbgs()));
        return false;
      }
    } else {
      assert(SubReg == 0);
    }
    BuildMI(*MBB, MBBI, DL, get(FoldedOpcode))
        .addDef(UseMI.getOperand(0).getReg())
        ->addOperand(Operand);
    return true;
  }
  case GB::ADD_r:
    FoldedOpcode = GB::ADDI;
    break;
  case GB::ADC_r:
    FoldedOpcode = GB::ADCI;
    break;
  case GB::SUB_r:
    FoldedOpcode = GB::SUBI;
    break;
  case GB::SBC_r:
    FoldedOpcode = GB::SBCI;
    break;
  case GB::AND_r:
    FoldedOpcode = GB::ANDI;
    break;
  case GB::XOR_r:
    FoldedOpcode = GB::XORI;
    break;
  case GB::OR_r:
    FoldedOpcode = GB::ORI;
    break;
  }

  BuildMI(*MBB, MBBI, DL, get(FoldedOpcode))
      .addDef(GB::A, getImplRegState(true))
      ->addOperand(Operand);

  return true;
}

bool GBInstrInfo::foldImmediate(MachineInstr &UseMI, MachineInstr &DefMI,
                                Register Reg, MachineRegisterInfo *MRI,
                                bool &IsDeleted) const {
  auto *MBB = UseMI.getParent();

  LLVM_DEBUG(dbgs() << "foldImmediate: \n"; dbgs() << "  def=";
             DefMI.print(dbgs()); dbgs() << "  use="; UseMI.print(dbgs()));

  // TODO: maybe try aborting this if it has too many uses
  auto HasOneUse = MRI->hasOneNonDBGUse(Reg);

  MachineOperand *Imm = nullptr;
  switch (DefMI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Fold failed: unsupported def opcode "
                      << DefMI.getOpcode() << "\n");
    return false;
  case GB::LDI16:
    Imm = &DefMI.getOperand(1);
    break;
  case GB::LDI8_r:
    Imm = &DefMI.getOperand(1);
    break;
  }

  switch (UseMI.getOpcode()) {
  case GB::LD_r_iGPR16:
  case GB::LD_iGPR16_A:
    if (not foldAddressImmediate(UseMI, *Imm, Reg, MRI)) {
      return false;
    }
    break;
  default:
    if (not fold8BitImmediate(UseMI, *Imm, Reg, MRI)) {
      return false;
    }
    break;
  }

  if (HasOneUse) {
    DefMI.eraseFromParent();
  }
  UseMI.eraseFromParent();

  LLVM_DEBUG(dbgs() << "Fold accepted: "; MBB->print(dbgs()));
  IsDeleted = true;
  return true;
}

MachineInstr *GBInstrInfo::foldMemoryOperandImpl(
    MachineFunction &MF, MachineInstr &MI, ArrayRef<unsigned> Ops,
    MachineBasicBlock::iterator InsertPt, MachineInstr &LoadMI,
    LiveIntervals *LIS) const {
  auto *MBB = MI.getParent();
  auto *TRI = MBB->getParent()->getRegInfo().getTargetRegisterInfo();
  LLVM_DEBUG(dbgs() << "foldMemoryOperandImpl: \n"; dbgs() << "  mi=";
             MI.print(dbgs()));

  if (LoadMI.getOpcode() != GB::LD_r_iGPR16) {
    LLVM_DEBUG(dbgs() << "Fold rejected: load not supported\n");
    return nullptr;
  }
  auto LoadedPtrReg = LoadMI.getOperand(1).getReg();
  if (LoadedPtrReg.isPhysical()) {
    LLVM_DEBUG(dbgs() << "Fold rejected: must be a virtual register\n");
    return nullptr;
  }

  unsigned FoldedOpcode = 0;
  switch (MI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Fold rejected: MI not supported\n");
    return nullptr;

  case GB::ADD_r:
    FoldedOpcode = GB::ADD_iHL;
    break;
  case GB::ADC_r:
    FoldedOpcode = GB::ADC_iHL;
    break;
  case GB::SUB_r:
    FoldedOpcode = GB::SUB_iHL;
    break;
  case GB::SBC_r:
    FoldedOpcode = GB::SBC_iHL;
    break;
  case GB::AND_r:
    FoldedOpcode = GB::AND_iHL;
    break;
  case GB::XOR_r:
    FoldedOpcode = GB::XOR_iHL;
    break;
  case GB::OR_r:
    FoldedOpcode = GB::OR_iHL;
    break;
  }

  if (Ops.size() != 1 || Ops[0] != 0) {
    LLVM_DEBUG(dbgs() << "Fold rejected: Ops not supported\n");
    return nullptr;
  }

  BuildMI(*MBB, InsertPt, MI.getDebugLoc(), get(GB::COPY), GB::HL)
      .addReg(LoadedPtrReg,
              getKillRegState(LoadMI.killsRegister(LoadedPtrReg, TRI)));

  auto NewMI = BuildMI(*MBB, InsertPt, MI.getDebugLoc(), get(FoldedOpcode))
                   .addDef(GB::A, getImplRegState(true))
                   .addReg(GB::HL, getImplRegState(true));
  return NewMI;
}

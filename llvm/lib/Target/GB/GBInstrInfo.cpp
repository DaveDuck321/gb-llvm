#include "GBInstrInfo.h"
#include "GB.h"
#include "GBRegisterInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "GBGenInstrInfo.inc"

using namespace llvm;

GBInstrInfo::GBInstrInfo()
    : GBGenInstrInfo(GB::ADJCALLSTACKDOWN, GB::ADJCALLSTACKUP) {}

void GBInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MBBI,
                              const DebugLoc &DL, MCRegister DestReg,
                              MCRegister SrcReg, bool KillSrc) const {

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
    const TargetRegisterInfo *TRI, Register VReg) const {
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

  if (GB::GPR16RegClass.hasSubClassEq(RC)) {
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
                                       Register VReg) const {
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
  if (GB::GPR16RegClass.hasSubClassEq(RC)) {
    BuildMI(MBB, MI, DL, get(GB::Load16FromFrameIndex), DestReg)
        .addFrameIndex(FrameIndex)
        .addMemOperand(MMO);
    return;
  }
  llvm_unreachable("Could not load reg to stack slot!");
}

bool GBInstrInfo::analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                                MachineBasicBlock *&FBB,
                                SmallVectorImpl<MachineOperand> &Cond,
                                bool AllowModify) const {

  MachineInstr *FinalConditionalBranch = nullptr;
  MachineInstr *FinalUnconditionalBranch = nullptr;
  for (auto &MI : *MBB.getIterator()) {
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
  while (I != MBB.begin()) {
    if (not I->isBranch() || I->isIndirectBranch()) {
      break;
    }
    if (BytesRemoved != nullptr) {
      *BytesRemoved += getInstSizeInBytes(*I);
    }
    BranchesRemoved += 1;
    I->removeFromParent();
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

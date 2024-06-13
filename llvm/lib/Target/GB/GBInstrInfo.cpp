#include "GBInstrInfo.h"
#include "GB.h"
#include "GBRegisterInfo.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
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

    if (KillSrc) {
      BuildMI(MBB, MBBI, DL, get(TargetOpcode::KILL), SrcReg);
    }
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestA).addReg(SrcA);
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestB).addReg(SrcB);
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
  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

  // TODO GB: The GameBoy is really ill-suited for this constant stack offset...
  // find a way to only use push/ pops (almost exclusively?)

  MachineFunction *MF = MBB.getParent();
  MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FrameIndex),
      MachineMemOperand::MOStore, LocationSize::beforeOrAfterPointer(),
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
  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

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

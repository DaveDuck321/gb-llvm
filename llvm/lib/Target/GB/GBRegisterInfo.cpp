#include "GBRegisterInfo.h"
#include "GBFrameLowering.h"
#include "GBTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

#define GET_REGINFO_TARGET_DESC
#include "GBGenRegisterInfo.inc"

#define DEBUG_TYPE "gb-register-info"

using namespace llvm;

GBRegisterInfo::GBRegisterInfo() : GBGenRegisterInfo(GB::A) {}

const MCPhysReg *
GBRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return GB_CSRs_SaveList;
}

const uint32_t *GBRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                                     CallingConv::ID) const {
  return GB_CSRs_RegMask;
}

BitVector GBRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  markSuperRegs(Reserved, GB::SP);
  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

bool GBRegisterInfo::requiresRegisterScavenging(
    const MachineFunction &MF) const {
  // Required so that eliminateFrameIndex can preserve HL
  return true;
}

bool GBRegisterInfo::eliminateStackSlotFrameIndex(
    MachineBasicBlock::iterator MI, int Offset, RegScavenger &RS) const {

  MachineInstr &OldMI = *MI;
  DebugLoc DL = MI->getDebugLoc();
  Register Reg = MI->getOperand(0).getReg();
  bool IsKill = MI->getOperand(0).isKill();

  MachineBasicBlock &MBB = *MI->getParent();
  const MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
  assert(isInt<8>(Offset));

  bool PreserveHL = RS.isRegUsed(GB::HL);

  switch (MI->getOpcode()) {
  default:
    llvm_unreachable("Unrecognized opcode");
  case GB::Save8ToFrameIndex: {
    if (PreserveHL) {
      // TOOD GB: maybe turn this into a simple register copy if possible?
      // TOOD GB: can I use a virtual register here
      // TOOD GB: correctly model the cost of this/ the frame lowering
      BuildMI(MBB, MI, DL, TII.get(GB::PUSH))
          .addReg(GB::HL, getKillRegState(true));
    }
    BuildMI(MBB, MI, DL, TII.get(GB::LD_HL_SP)).addImm(Offset - 2 * PreserveHL);
    BuildMI(MBB, MI, DL, TII.get(GB::LD_iHL_r))
        .addReg(Reg, getKillRegState(IsKill));

    // TODO GB: should this be dead or kill?
    MI->addRegisterKilled(GB::HL, &TRI, true);
    if (PreserveHL) {
      BuildMI(MBB, MI, DL, TII.get(GB::POP)).addDef(GB::HL);
    }
    OldMI.eraseFromParent();
    return true;
  }
  case GB::Load8FromFrameIndex: {
    if (PreserveHL) {
      BuildMI(MBB, MI, DL, TII.get(GB::PUSH))
          .addReg(GB::HL, getKillRegState(true));
    }
    BuildMI(MBB, MI, DL, TII.get(GB::LD_HL_SP)).addImm(Offset - 2 * PreserveHL);
    BuildMI(MBB, MI, DL, TII.get(GB::LD_r_iHL)).addDef(Reg);
    MI->addRegisterKilled(GB::HL, &TRI, true);

    if (PreserveHL) {
      BuildMI(MBB, MI, DL, TII.get(GB::POP)).addDef(GB::HL);
    }
    OldMI.eraseFromParent();
    return true;
  }
  case GB::Save16ToFrameIndex: {
    Offset += 1; // TODO GB: understand this
    BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(Offset);
    BuildMI(MBB, MI, DL, TII.get(GB::PUSH))
        .addReg(Reg, getKillRegState(IsKill));
    BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(-Offset + 2);
    OldMI.eraseFromParent();
    return true;
  }
  case GB::Load16FromFrameIndex: {
    Offset += 1; // TODO GB: understand this
    BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(Offset - 2);
    BuildMI(MBB, MI, DL, TII.get(GB::POP)).addDef(Reg);
    BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(-Offset);
    OldMI.eraseFromParent();
    return true;
  }
  }
}

bool GBRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI,
                                         int SPAdj, unsigned FIOperandNum,
                                         RegScavenger *RS) const {
  assert(RS);

  DebugLoc DL = MI->getDebugLoc();
  MachineBasicBlock &MBB = *MI->getParent();
  const MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();
  const TargetFrameLowering &TFL = *MF.getSubtarget().getFrameLowering();

  // Stack slots require actual loads, this is MUCH more work
  const int FrameIndex = MI->getOperand(FIOperandNum).getIndex();
  const int Offset = MFI.getObjectOffset(FrameIndex) + MFI.getStackSize() -
                     TFL.getOffsetOfLocalArea() + SPAdj + 1;

  LLVM_DEBUG(dbgs() << "Eliminate frame index: " << MF.getName() << "\n"
                    << "FrameIndex: " << FrameIndex << ", "
                    << "Object Offset: " << MFI.getObjectOffset(FrameIndex)
                    << ", "
                    << "Stack size: " << MFI.getStackSize() << ", "
                    << "LAO: " << TFL.getOffsetOfLocalArea() << ", "
                    << "SPAdj: " << SPAdj << "\n"
                    << "Computed offset: " << Offset << "\n\n");

  if (int _;
      TII.isLoadFromStackSlot(*MI, _) || TII.isStoreToStackSlot(*MI, _)) {
    return eliminateStackSlotFrameIndex(MI, Offset, *RS);
  }

  // Otherwise the complexity is handled by the GBISelLowering
  assert(MI->getOpcode() == GB::LD_HL_SP);
  assert(isInt<8>(Offset));
  MI->getOperand(FIOperandNum).ChangeToImmediate(Offset);
  return false;
}

Register GBRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
  assert(not TFI->hasFP(MF) && "Frame pointer generation is not supported");
  return GB::SP;
}

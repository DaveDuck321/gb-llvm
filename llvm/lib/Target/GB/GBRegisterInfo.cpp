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
  auto CC = MF->getFunction().getCallingConv();
  switch (CC) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Cold:
  case CallingConv::Fast:
    return GB_CC_CSRs_SaveList;
  case CallingConv::GB_Interrupt:
    return GB_Interrupt_CC_CSRs_SaveList;
  }
}

const uint32_t *GBRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                                     CallingConv::ID CC) const {
  switch (CC) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Cold:
  case CallingConv::Fast:
    return GB_CC_CSRs_RegMask;
  case CallingConv::GB_Interrupt:
    return GB_Interrupt_CC_CSRs_RegMask;
  }
}

BitVector GBRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  markSuperRegs(Reserved, GB::SP);
  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

bool GBRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI,
                                         int SPAdj, unsigned FIOperandNum,
                                         RegScavenger *RS) const {
  DebugLoc DL = MI->getDebugLoc();
  MachineBasicBlock &MBB = *MI->getParent();
  const MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetFrameLowering &TFL = *MF.getSubtarget().getFrameLowering();
  const TargetInstrInfo &TII = *MF.getSubtarget().getInstrInfo();

  const int FrameIndex = MI->getOperand(FIOperandNum).getIndex();
  const int Offset = MFI.getObjectOffset(FrameIndex) + MFI.getStackSize() -
                     TFL.getOffsetOfLocalArea() + SPAdj;

  LLVM_DEBUG(dbgs() << "Eliminate frame index: " << MF.getName() << "\n"
                    << "FrameIndex: " << FrameIndex << ", "
                    << "Object Offset: " << MFI.getObjectOffset(FrameIndex)
                    << ", "
                    << "Stack size: " << MFI.getStackSize() << ", "
                    << "LAO: " << TFL.getOffsetOfLocalArea() << ", "
                    << "SPAdj: " << SPAdj << "\n"
                    << "Computed offset: " << Offset << "\n\n");

  switch (MI->getOpcode()) {
  default:
    llvm_unreachable("Unrecognized opcode");
  case GB::Load8FromFrameIndex:
  case GB::Load16FromFrameIndex:
  case GB::Save8ToFrameIndex:
  case GB::Save16ToFrameIndex:
    // Handled in GBStackSlotLowering.cpp
    MI->getOperand(FIOperandNum).ChangeToImmediate(Offset);
    return false;
  case GB::LD_HL_SP:
    break;
  }

  if (isInt<8>(Offset)) {
    // The offset is small, happy path
    MI->getOperand(FIOperandNum).ChangeToImmediate(Offset);
    return false;
  }

  // The immediate is too big for LD HL, SP + r8
  // Instead lower to:
  // LD HL, d16
  // ADD HL, SP
  BuildMI(MBB, MI, DL, TII.get(GB::LDI16_r), GB::HL).addImm(Offset);
  BuildMI(MBB, MI, DL, TII.get(GB::ADD_HL)).addReg(GB::SP);
  MI->eraseFromParent();
  return true;
}

Register GBRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
  assert(not TFI->hasFP(MF) && "Frame pointer generation is not supported");
  return GB::SP;
}

bool GBRegisterInfo::shouldUseDeferredSpillingForVirtReg(
    const MachineFunction &MF, const LiveInterval &VirtReg) const {
  return true;
}

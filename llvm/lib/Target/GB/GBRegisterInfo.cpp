#include "GBRegisterInfo.h"
#include "GBFrameLowering.h"
#include "GBTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/Debug.h"
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

bool GBRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI,
                                         int SPAdj, unsigned FIOperandNum,
                                         RegScavenger *RS) const {
  DebugLoc DL = MI->getDebugLoc();
  MachineBasicBlock &MBB = *MI->getParent();
  const MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
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

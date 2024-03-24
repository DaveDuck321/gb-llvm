#include "GBRegisterInfo.h"
#include "GBFrameLowering.h"
#include "GBTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

#define GET_REGINFO_TARGET_DESC
#include "GBGenRegisterInfo.inc"

using namespace llvm;

GBRegisterInfo::GBRegisterInfo() : GBGenRegisterInfo(GB::A) {}

const MCPhysReg *
GBRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return GB_CSRs_SaveList;
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
  assert(SPAdj == 0 && "Unexpected SPAdj value");

  DebugLoc DL = MI->getDebugLoc();
  MachineBasicBlock &MBB = *MI->getParent();
  const MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  int FrameIndex = MI->getOperand(FIOperandNum).getIndex();
  int Offset = MFI.getObjectOffset(FrameIndex);

  assert(isInt<8>(Offset));
  MI->getOperand(FIOperandNum).ChangeToImmediate(Offset);
  return false;
}

Register GBRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
  assert(not TFI->hasFP(MF) && "Frame pointer generation is not supported");
  return GB::SP;
}

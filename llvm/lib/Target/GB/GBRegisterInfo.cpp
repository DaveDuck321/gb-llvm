#include "GBRegisterInfo.h"
#include "GBFrameLowering.h"
#include "GBTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"

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
  report_fatal_error("Not yet implemented!");
}

Register GBRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  // TODO GB: atm there is NO frame pointer, only BC
  // Since the GB does not support dynamic allocation, alloca might be useful
  // and this decision might not be smart.
  return GB::BC;
}

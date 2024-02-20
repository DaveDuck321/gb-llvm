#include "GBFrameLowering.h"

using namespace llvm;

GBFrameLowering::GBFrameLowering(const GBSubtarget &STI)
    : TargetFrameLowering(StackGrowsDown, Align(1),
                          /* LocalAreaOffset = */ 0) {}

void GBFrameLowering::emitPrologue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {}

void GBFrameLowering::emitEpilogue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {}

bool GBFrameLowering::hasFP(const MachineFunction &MF) const {
  // TODO GB: I really want this to be always false... what would break?
  return true;
}

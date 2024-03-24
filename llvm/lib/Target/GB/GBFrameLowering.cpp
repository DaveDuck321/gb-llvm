#include "GBFrameLowering.h"
#include "llvm/CodeGen/MachineFunction.h"

using namespace llvm;

GBFrameLowering::GBFrameLowering(const GBSubtarget &STI)
    : TargetFrameLowering(StackGrowsDown, Align(1),
                          /* LocalAreaOffset = */ 0) {}

void GBFrameLowering::emitPrologue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {}

void GBFrameLowering::emitEpilogue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {}

bool GBFrameLowering::hasFP(const MachineFunction &MF) const {
  // FIXME GB: I'm assuming this won't hold forever
  return false;
}

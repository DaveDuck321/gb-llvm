#include "GBFrameLowering.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"

#include <cstdint>

using namespace llvm;

GBFrameLowering::GBFrameLowering(const GBSubtarget &STI)
    : TargetFrameLowering(StackGrowsDown, Align(1),
                          /* LocalAreaOffset = */ -2) {}

void GBFrameLowering::emitPrologue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {

  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  assert(&MF.front() == &MBB);
  assert(not MFI.hasVarSizedObjects());

  MachineBasicBlock::iterator MBBI = MBB.getFirstNonDebugInstr();

  uint64_t StackSize = MFI.getStackSize();
  if (StackSize == 0) {
    return; // Nothing to do
  }
  BuildMI(MBB, MBBI, DebugLoc{}, TII.get(GB::ADD_SP)).addImm(-StackSize);
}

void GBFrameLowering::emitEpilogue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  assert(not MFI.hasVarSizedObjects());

  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();

  uint64_t StackSize = MFI.getStackSize();
  if (StackSize == 0) {
    return; // Nothing to do
  }
  BuildMI(MBB, MBBI, DebugLoc{}, TII.get(GB::ADD_SP)).addImm(StackSize);
}

bool GBFrameLowering::hasFP(const MachineFunction &MF) const {
  // FIXME GB: alloca
  return false;
}

bool GBFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // TOOD GB: setting this false and using a PUSH/POP sequence would likely be
  // very fast.
  return true;
}

MachineBasicBlock::iterator GBFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  if (hasReservedCallFrame(MF)) {
    return MBB.erase(MI);
  }

  // TODO GB: convert this to push/ pop sequence
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  DebugLoc DL = MI->getDebugLoc();

  int Amount = TII.getFrameSize(*MI);
  if (Amount == 0) {
    return MBB.erase(MI);
  }

  if (MI->getOpcode() == GB::ADJCALLSTACKDOWN) {
    Amount *= -1;
  }
  BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(Amount);
  return MBB.erase(MI);
}

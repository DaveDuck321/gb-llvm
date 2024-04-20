#ifndef LLVM_LIB_TARGET_GB_GBFRAMELOWERING_H
#define LLVM_LIB_TARGET_GB_GBFRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {

class GBSubtarget;

class GBFrameLowering : public TargetFrameLowering {
public:
  explicit GBFrameLowering(const GBSubtarget &);

  bool hasReservedCallFrame(const MachineFunction &MF) const override;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI) const override;

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  bool hasFP(const MachineFunction &MF) const override;
};

} // namespace llvm

#endif

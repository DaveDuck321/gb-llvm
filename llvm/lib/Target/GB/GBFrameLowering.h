#ifndef LLVM_LIB_TARGET_GB_GBFRAMELOWERING_H
#define LLVM_LIB_TARGET_GB_GBFRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {

class GBSubtarget;

class GBFrameLowering : public TargetFrameLowering {
public:
  explicit GBFrameLowering(const GBSubtarget &);

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  bool hasFP(const MachineFunction &MF) const override;
};

} // namespace llvm

#endif

#ifndef LLVM_LIB_TARGET_GB_GBINSTRINFO_H
#define LLVM_LIB_TARGET_GB_GBINSTRINFO_H

#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "GBGenInstrInfo.inc"

namespace llvm {
struct GBInstrInfo final : public GBGenInstrInfo {
  GBInstrInfo();

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                   const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg,
                   bool KillSrc) const override;
};
} // namespace llvm

#endif

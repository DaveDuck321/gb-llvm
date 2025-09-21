#ifndef LLVM_LIB_TARGET_GB_GBMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_GB_GBMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

class GBMachineFunctionInfo : public MachineFunctionInfo {
  unsigned CalleeSavedFrameSize = 0;

public:
  GBMachineFunctionInfo() = default;

  GBMachineFunctionInfo(const Function &F, const TargetSubtargetInfo *STI)
      : CalleeSavedFrameSize(0) {}

  unsigned getCalleeSavedFrameSize() const { return CalleeSavedFrameSize; }
  void setCalleeSavedFrameSize(unsigned Bytes) { CalleeSavedFrameSize = Bytes; }
};

} // namespace llvm

#endif

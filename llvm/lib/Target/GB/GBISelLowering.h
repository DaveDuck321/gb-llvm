#ifndef LLVM_LIB_TARGET_GB_GBISELLOWERING_H
#define LLVM_LIB_TARGET_GB_GBISELLOWERING_H

#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class GBSubtarget;

class GBTargetLowering : public TargetLowering {
public:
  GBTargetLowering(const TargetMachine &, const GBSubtarget &);

  bool useSoftFloat() const override;

  bool isSelectSupported(SelectSupportKind) const override;

  bool allowsMisalignedMemoryAccesses(EVT, unsigned AddrSpace, Align,
                                      MachineMemOperand::Flags,
                                      unsigned *Fast) const override;
};
} // namespace llvm
#endif

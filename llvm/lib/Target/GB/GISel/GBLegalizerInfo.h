#ifndef LLVM_LIB_TARGET_GB_GISEL_GBLEGALIZERINFO_H
#define LLVM_LIB_TARGET_GB_GISEL_GBLEGALIZERINFO_H

#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"

namespace llvm {
class GBSubtarget;

class GBLegalizerInfo : public LegalizerInfo {
public:
  GBLegalizerInfo(const GBSubtarget &STI);

  bool legalizeCustom(LegalizerHelper &Helper, MachineInstr &MI,
                      LostDebugLocObserver &LocObserver) const override;

private:
  bool legalizeLoadStore(LegalizerHelper &Helper, MachineInstr &MI,
                         LostDebugLocObserver &LocObserver) const;

  bool legalizeTrunc(LegalizerHelper &Helper, MachineInstr &MI,
                     LostDebugLocObserver &LocObserver) const;

  bool legalizeUnmerge(LegalizerHelper &Helper, MachineInstr &MI,
                       LostDebugLocObserver &LocObserver) const;

  bool legalizeSExt(LegalizerHelper &Helper, MachineInstr &MI,
                    LostDebugLocObserver &LocObserver) const;

  bool legalizeSelect(LegalizerHelper &Helper, MachineInstr &MI,
                      LostDebugLocObserver &LocObserver) const;

  bool legalizeSignedICmp(LegalizerHelper &Helper, MachineInstr &MI,
                          LostDebugLocObserver &LocObserver) const;

  bool legalizeICmp(LegalizerHelper &Helper, MachineInstr &MI,
                    LostDebugLocObserver &LocObserver) const;

  bool legalizeLargeAddSub(LegalizerHelper &Helper, MachineInstr &MI,
                           LostDebugLocObserver &LocObserver) const;

  bool legalizeByteSwap(LegalizerHelper &Helper, MachineInstr &MI,
                        LostDebugLocObserver &LocObserver) const;

  bool legalizeShiftRotate(LegalizerHelper &Helper, MachineInstr &MI,
                           LostDebugLocObserver &LocObserver) const;

  bool legalizeConstantShiftRotate(LegalizerHelper &Helper, MachineInstr &MI,
                                   LostDebugLocObserver &LocObserver,
                                   uint8_t Amount) const;
};
} // namespace llvm

#endif

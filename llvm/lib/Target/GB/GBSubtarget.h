#ifndef LLVM_LIB_TARGET_GB_GBSUBTARGET_H
#define LLVM_LIB_TARGET_GB_GBSUBTARGET_H

#include "GBFrameLowering.h"
#include "GBISelLowering.h"
#include "GBInstrInfo.h"
#include "GBRegisterInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/TargetParser/Triple.h"

#define GET_SUBTARGETINFO_HEADER
#include "GBGenSubtargetInfo.inc"

namespace llvm {
class GBSubtarget final : public GBGenSubtargetInfo {
  GBInstrInfo InstrInfo;
  GBFrameLowering FrameLowering;
  GBRegisterInfo RegisterInfo;

  GBTargetLowering TargetLowering;
  SelectionDAGTargetInfo TSInfo;

public:
  GBSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
              const TargetMachine &TM);

  bool enableSubRegLiveness() const override { return true; }

  const GBInstrInfo *getInstrInfo() const override { return &InstrInfo; }
  const GBFrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }
  const GBTargetLowering *getTargetLowering() const override {
    return &TargetLowering;
  }
  const SelectionDAGTargetInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }
  const GBRegisterInfo *getRegisterInfo() const override {
    return &RegisterInfo;
  }
};
} // namespace llvm
#endif

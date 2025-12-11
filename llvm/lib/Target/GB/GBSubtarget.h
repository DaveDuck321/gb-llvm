#ifndef LLVM_LIB_TARGET_GB_GBSUBTARGET_H
#define LLVM_LIB_TARGET_GB_GBSUBTARGET_H

#include "GBFrameLowering.h"
#include "GBISelLowering.h"
#include "GBInstrInfo.h"
#include "GBRegisterInfo.h"
#include "GISel/GBRegisterBankInfo.h"
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

  mutable std::unique_ptr<CallLowering> CallLoweringInfo = nullptr;
  mutable std::unique_ptr<InstructionSelector> InstSelector = nullptr;
  mutable std::unique_ptr<LegalizerInfo> Legalizer = nullptr;
  mutable std::unique_ptr<GBRegisterBankInfo> RegBankInfo = nullptr;

public:
  GBSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
              const TargetMachine &TM);
  ~GBSubtarget();

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

  const CallLowering *getCallLowering() const override;
  InstructionSelector *getInstructionSelector() const override;
  const LegalizerInfo *getLegalizerInfo() const override;
  const GBRegisterBankInfo *getRegBankInfo() const override;
};
} // namespace llvm
#endif

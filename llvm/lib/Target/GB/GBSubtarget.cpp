#include "GBSubtarget.h"
#include "GB.h"
#include "GBFrameLowering.h"
#include "GBInstrInfo.h"
#include "GBRegisterInfo.h"
#include "GBTargetMachine.h"
#include "GISel/GBCallLowering.h"
#include "GISel/GBLegalizerInfo.h"
#include "GISel/GBRegisterBankInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/GlobalISel/InlineAsmLowering.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelector.h"
#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"

using namespace llvm;

#define DEBUG_TYPE "gb-subtarget"

#define GET_SUBTARGETINFO_CTOR
#include "GBGenSubtargetInfo.inc"

GBSubtarget::GBSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
                         const TargetMachine &TM)
    : GBGenSubtargetInfo(TT, CPU, /* TuneCPU */ CPU, FS), InstrInfo(),
      FrameLowering(*this), RegisterInfo(), TargetLowering(TM, *this),
      TSInfo() {}

GBSubtarget::~GBSubtarget() = default;

const InlineAsmLowering *GBSubtarget::getInlineAsmLowering() const {
  if (!InlineAsm) {
    InlineAsm.reset(new InlineAsmLowering(getTargetLowering()));
  }
  return InlineAsm.get();
}

const CallLowering *GBSubtarget::getCallLowering() const {
  if (!CallLoweringInfo) {
    CallLoweringInfo.reset(new GBCallLowering(*getTargetLowering()));
  }
  return CallLoweringInfo.get();
}

InstructionSelector *GBSubtarget::getInstructionSelector() const {
  if (!InstSelector) {
    InstSelector.reset(createGBInstructionSelector(*this, *getRegBankInfo()));
  }
  return InstSelector.get();
}

const LegalizerInfo *GBSubtarget::getLegalizerInfo() const {
  if (!Legalizer) {
    Legalizer.reset(new GBLegalizerInfo(*this));
  }
  return Legalizer.get();
}

const GBRegisterBankInfo *GBSubtarget::getRegBankInfo() const {
  if (!RegBankInfo) {
    RegBankInfo.reset(new GBRegisterBankInfo());
  }
  return RegBankInfo.get();
}

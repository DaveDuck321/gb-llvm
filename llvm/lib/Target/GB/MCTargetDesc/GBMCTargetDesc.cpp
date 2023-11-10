#include "GBMCTargetDesc.h"
#include "GBMCAsmInfo.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Triple.h"

#define GET_INSTRINFO_MC_DESC
#include "GBGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "GBGenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createGBInstrInfo() {
  MCInstrInfo *InstrInfo = new MCInstrInfo();
  InitGBMCInstrInfo(InstrInfo);
  return InstrInfo;
}

static MCAsmInfo *createGBMCAsmInfo(const MCRegisterInfo &MRI, const Triple &TT,
                                    const MCTargetOptions &MTO) {
  return new GBMCAsmInfo();
}

static MCRegisterInfo *createGBMCRegisterInfo(const Triple &TT) {
  auto *RegInfo = new MCRegisterInfo();
  InitGBMCRegisterInfo(RegInfo, GB::A);
  return RegInfo;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBTargetMC() {
  auto &T = getTheGBTarget();
  TargetRegistry::RegisterMCAsmInfo(T, createGBMCAsmInfo);
  TargetRegistry::RegisterMCInstrInfo(T, createGBInstrInfo);
  TargetRegistry::RegisterMCRegInfo(T, createGBMCRegisterInfo);
  TargetRegistry::RegisterMCAsmBackend(T, createGBAsmBackend);
  TargetRegistry::RegisterMCCodeEmitter(T, createGBMCCodeEmitter);
}

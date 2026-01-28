#include "GBMCTargetDesc.h"
#include "GBInstPrinter.h"
#include "GBMCAsmInfo.h"

#include "TargetInfo/GBTargetInfo.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSchedule.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/TargetParser/Triple.h"

#define GET_INSTRINFO_MC_DESC
#include "GBGenInstrInfo.inc"

#define GET_REGINFO_MC_DESC
#include "GBGenRegisterInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "GBGenSubtargetInfo.inc"

using namespace llvm;

static MCInstPrinter *createGBInstPrinter(const Triple &,
                                          unsigned SyntaxVariant,
                                          const MCAsmInfo &MAI,
                                          const MCInstrInfo &MII,
                                          const MCRegisterInfo &MRI) {
  return new GBInstPrinter(MAI, MII, MRI);
}

static MCAsmInfo *createGBMCAsmInfo(const MCRegisterInfo &MRI, const Triple &TT,
                                    const MCTargetOptions &MTO) {
  int StackGrowth = -2;

  auto *MAI = new GBMCAsmInfo();
  unsigned SP = MRI.getDwarfRegNum(GB::SP, false);
  MCCFIInstruction Inst =
      MCCFIInstruction::cfiDefCfa(nullptr, SP, -StackGrowth);
  MAI->addInitialFrameState(Inst);

  unsigned PC = MRI.getDwarfRegNum(GB::PC, true);
  MCCFIInstruction Inst2 =
      MCCFIInstruction::createOffset(nullptr, PC, StackGrowth);
  MAI->addInitialFrameState(Inst2);
  return MAI;
}

static MCInstrInfo *createGBInstrInfo() {
  MCInstrInfo *InstrInfo = new MCInstrInfo();
  InitGBMCInstrInfo(InstrInfo);
  return InstrInfo;
}

static MCRegisterInfo *createGBMCRegisterInfo(const Triple &TT) {
  auto *RegInfo = new MCRegisterInfo();
  InitGBMCRegisterInfo(RegInfo, GB::A);
  return RegInfo;
}

static MCSubtargetInfo *createGBMCSubtargetInfo(const Triple &TT, StringRef CPU,
                                                StringRef FS) {
  return createGBMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBTargetMC() {
  auto &T = getTheGBTarget();
  TargetRegistry::RegisterMCAsmBackend(T, createGBAsmBackend);
  TargetRegistry::RegisterMCAsmInfo(T, createGBMCAsmInfo);
  TargetRegistry::RegisterMCCodeEmitter(T, createGBMCCodeEmitter);
  TargetRegistry::RegisterMCInstPrinter(T, createGBInstPrinter);
  TargetRegistry::RegisterMCInstrInfo(T, createGBInstrInfo);
  TargetRegistry::RegisterMCRegInfo(T, createGBMCRegisterInfo);
  TargetRegistry::RegisterMCSubtargetInfo(T, createGBMCSubtargetInfo);
}

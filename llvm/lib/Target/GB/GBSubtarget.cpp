#include "GBSubtarget.h"
#include "GB.h"
#include "GBFrameLowering.h"
#include "GBInstrInfo.h"
#include "GBRegisterInfo.h"
#include "llvm/ADT/StringRef.h"

using namespace llvm;

#define DEBUG_TYPE "gb-subtarget"

#define GET_SUBTARGETINFO_CTOR
#include "GBGenSubtargetInfo.inc"

GBSubtarget::GBSubtarget(const Triple &TT, StringRef CPU, StringRef FS,
                         const TargetMachine &TM)
    : GBGenSubtargetInfo(TT, CPU, /* TuneCPU */ CPU, FS), InstrInfo(),
      FrameLowering(*this), RegisterInfo(), TargetLowering(TM, *this),
      TSInfo() {}

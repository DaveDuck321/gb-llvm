#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class GBMCAsmInfo : public MCAsmInfoELF {
public:
  GBMCAsmInfo() {
    CodePointerSize = 2;
    // TODO GB: maybe this should be '1' since we support unaligned access
    CalleeSaveStackSlotSize = 2;
    AlignmentIsInBytes = false;
    UseMotorolaIntegers = true;
  }
};

} // namespace llvm

#endif

#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class GBMCAsmInfo : public MCAsmInfoELF {
public:
  GBMCAsmInfo() {
    CodePointerSize = 2;
    CalleeSaveStackSlotSize = 2;
    AlignmentIsInBytes = false;
    UseMotorolaIntegers = true;
    CommentString = "#";
    SeparatorString = ";";
    SupportsDebugInformation = true;
  }
};

} // namespace llvm

#endif

#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class GBMCAsmInfo : public MCAsmInfoELF {
public:
  GBMCAsmInfo() {
    CodePointerSize = 4;
    CalleeSaveStackSlotSize = 2;
    AlignmentIsInBytes = false;
    UseMotorolaIntegers = true;
    CommentString = "#";
    SeparatorString = ";";
    SupportsDebugInformation = true;

    DwarfRegNumForCFI = true;
    UsesCFIWithoutEH = true;
    SupportsDebugInformation = true;
  }
};

} // namespace llvm

#endif

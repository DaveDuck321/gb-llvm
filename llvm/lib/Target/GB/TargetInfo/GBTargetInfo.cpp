#include "GBTargetInfo.h"

#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

Target &llvm::getTheGBTarget() {
  static Target TheGBTarget;
  return TheGBTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBTargetInfo() {
  RegisterTarget<Triple::gb> _{getTheGBTarget(), "gb", "Gameboy/ Gameboy color",
                               "GB"};
}

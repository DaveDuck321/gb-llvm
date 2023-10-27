#ifndef LLVM_LIB_TARGET_GB_GBTARGETMACHINE_H
#define LLVM_LIB_TARGET_GB_GBTARGETMACHINE_H

#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"

#include <memory>
#include <optional>

namespace llvm {

class GBTargetMachine : public LLVMTargetMachine {
  const static std::string DataLayout;

  std::unique_ptr<TargetLoweringObjectFile> TLOF;

public:
  GBTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                  StringRef FS, const TargetOptions &Options,
                  std::optional<Reloc::Model> RM,
                  std::optional<CodeModel::Model> CM, CodeGenOptLevel OL,
                  bool JIT);

  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};

} // namespace llvm

#endif

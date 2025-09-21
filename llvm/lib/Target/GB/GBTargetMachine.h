#ifndef LLVM_LIB_TARGET_GB_GBTARGETMACHINE_H
#define LLVM_LIB_TARGET_GB_GBTARGETMACHINE_H

#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/CodeGen/CodeGenTargetMachineImpl.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetMachine.h"

#include <memory>
#include <optional>

namespace llvm {

class GBTargetMachine : public CodeGenTargetMachineImpl {
  const static std::string DataLayout;

  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  GBSubtarget Subtarget;

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

  bool shouldAssumeDSOLocal(const GlobalValue *GV) const override;

  const GBSubtarget *getGBSubtargetImpl(const Function &) const {
    return &Subtarget;
  }

  const TargetSubtargetInfo *
  getSubtargetImpl(const Function &F) const override {
    return getGBSubtargetImpl(F);
  }

  TargetTransformInfo getTargetTransformInfo(const Function &F) const override;

  MachineFunctionInfo *
  createMachineFunctionInfo(BumpPtrAllocator &Allocator, const Function &F,
                            const TargetSubtargetInfo *STI) const override;
};

} // namespace llvm

#endif

#include "GBTargetMachine.h"
#include "GB.h"
#include "TargetInfo/GBTargetInfo.h"

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetMachine.h"

#include <memory>
#include <optional>

using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBTarget() {
  llvm::RegisterTargetMachine<GBTargetMachine> _(getTheGBTarget());
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           std::optional<Reloc::Model> RM) {
  if (!RM.has_value())
    return Reloc::Static;
  return *RM;
}

// https://llvm.org/docs/LangRef.html#langref-datalayout
const std::string GBTargetMachine::DataLayout =
    "e-S8-p:16:16-i8:8-i16:16-m:e-n8:16";

// TODO: CM:Large might also make sense here... Check how this works in a 16 bit
// address space
GBTargetMachine::GBTargetMachine(const Target &T, const Triple &TT,
                                 StringRef CPU, StringRef FS,
                                 const TargetOptions &Options,
                                 std::optional<Reloc::Model> RM,
                                 std::optional<CodeModel::Model> CM,
                                 CodeGenOptLevel OL, bool JIT)
    : LLVMTargetMachine(T, DataLayout, TT, CPU, FS, Options,
                        getEffectiveRelocModel(TT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF{std::make_unique<TargetLoweringObjectFileELF>()},
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

const TargetSubtargetInfo *
GBTargetMachine::getSubtargetImpl(const Function &) const {
  return &Subtarget;
}

namespace {
class GBPassConfig : public TargetPassConfig {
public:
  GBPassConfig(GBTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  GBTargetMachine &getGBTargetMachine() const {
    return getTM<GBTargetMachine>();
  }

  bool addInstSelector() override;
  void addPreSched2() override;
};
} // namespace

TargetPassConfig *GBTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new GBPassConfig(*this, PM);
}

bool GBPassConfig::addInstSelector() {
  addPass(createGBISelDag(getGBTargetMachine(), getOptLevel()));
  return false;
}

void GBPassConfig::addPreSched2() {
  addPass(createGBStackSlotLowering(getGBTargetMachine(), getOptLevel()));
}

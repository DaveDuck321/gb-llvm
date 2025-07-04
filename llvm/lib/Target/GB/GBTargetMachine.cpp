#include "GBTargetMachine.h"
#include "GB.h"
#include "GBSubtarget.h"
#include "GBTargetTransformInfo.h"
#include "TargetInfo/GBTargetInfo.h"

#include "llvm/Analysis/TargetTransformInfo.h"
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
  PassRegistry &PR = *PassRegistry::getPassRegistry();
  initializeGBDAGToDAGISelLegacyPass(PR);
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           std::optional<Reloc::Model> RM) {
  if (!RM.has_value())
    return Reloc::Static;
  return *RM;
}

// https://llvm.org/docs/LangRef.html#langref-datalayout
const std::string GBTargetMachine::DataLayout =
    "e-S16-p:16:16-i8:8-i16:16-a:0-m:e-n8:16";

GBTargetMachine::GBTargetMachine(const Target &T, const Triple &TT,
                                 StringRef CPU, StringRef FS,
                                 const TargetOptions &Options,
                                 std::optional<Reloc::Model> RM,
                                 std::optional<CodeModel::Model> CM,
                                 CodeGenOptLevel OL, bool JIT)
    : CodeGenTargetMachineImpl(T, DataLayout, TT, CPU, FS, Options,
                               getEffectiveRelocModel(TT, RM),
                               getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF{std::make_unique<TargetLoweringObjectFileELF>()},
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

TargetTransformInfo
GBTargetMachine::getTargetTransformInfo(const Function &F) const {
  return TargetTransformInfo(std::make_unique<GBTTIImpl>(this, F));
}

namespace {
class GBPassConfig : public TargetPassConfig {
public:
  GBPassConfig(GBTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  GBTargetMachine &getGBTargetMachine() const {
    return getTM<GBTargetMachine>();
  }

  void addMachineSSAOptimization() override;
  bool addInstSelector() override;
  void addPreSched2() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *GBTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new GBPassConfig(*this, PM);
}

void GBPassConfig::addMachineSSAOptimization() {
  if (TM->getOptLevel() != CodeGenOptLevel::None) {
    addPass(createGBFoldImmediates(getGBTargetMachine(), getOptLevel()));
  }
  TargetPassConfig::addMachineSSAOptimization();
}

bool GBPassConfig::addInstSelector() {
  addPass(createGBISelDag(getGBTargetMachine(), getOptLevel()));
  return false;
}

void GBPassConfig::addPreSched2() {
  // Relaxation may reduce register pressure before stack slot lowering
  addPass(createGBInstructionRelaxation(getGBTargetMachine(), getOptLevel()));

  addPass(createGBStackSlotLowering(getGBTargetMachine(), getOptLevel()));
  addPass(createGBPushPopCombine(getGBTargetMachine(), getOptLevel()));

  // Relax again to catch anything generated in previous passes
  addPass(createGBInstructionRelaxation(getGBTargetMachine(), getOptLevel()));
}

void GBPassConfig::addPreEmitPass() {
  addPass(createGBBranchRelaxation(getGBTargetMachine(), getOptLevel()));
}

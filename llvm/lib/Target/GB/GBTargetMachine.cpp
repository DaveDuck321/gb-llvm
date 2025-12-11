#include "GBTargetMachine.h"
#include "GB.h"
#include "GBMachineFunctionInfo.h"
#include "GBSubtarget.h"
#include "GBTargetTransformInfo.h"
#include "TargetInfo/GBTargetInfo.h"

#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/GlobalISel/IRTranslator.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelect.h"
#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/RegBankSelect.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include <memory>
#include <optional>

using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBTarget() {
  llvm::RegisterTargetMachine<GBTargetMachine> _(getTheGBTarget());
  PassRegistry &PR = *PassRegistry::getPassRegistry();
  initializeGBDAGToDAGISelLegacyPass(PR);

  initializeGlobalISel(PR);
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           std::optional<Reloc::Model> RM) {
  if (!RM.has_value())
    return Reloc::Static;
  return *RM;
}

// https://llvm.org/docs/LangRef.html#langref-datalayout
const std::string GBTargetMachine::DataLayout =
    "e-S16-p:16:16-i8:8-i16:16-i32:16-i64:16-f32:16-f64:16-a:0-m:e-n8:16";

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

  setGlobalISel(true);
  setGlobalISelAbort(GlobalISelAbortMode::Enable);
}

TargetTransformInfo
GBTargetMachine::getTargetTransformInfo(const Function &F) const {
  return TargetTransformInfo(std::make_unique<GBTTIImpl>(this, F));
}

MachineFunctionInfo *GBTargetMachine::createMachineFunctionInfo(
    BumpPtrAllocator &Allocator, const Function &F,
    const TargetSubtargetInfo *STI) const {
  return GBMachineFunctionInfo::create<GBMachineFunctionInfo>(Allocator, F,
                                                              STI);
}

bool GBTargetMachine::shouldAssumeDSOLocal(const GlobalValue *GV) const {
  if (RM == Reloc::Static) {
    return true;
  }
  return TargetMachine::shouldAssumeDSOLocal(GV);
}

namespace {
class GBPassConfig : public TargetPassConfig {
public:
  GBPassConfig(GBTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  GBTargetMachine &getGBTargetMachine() const {
    return getTM<GBTargetMachine>();
  }

  void addLatePreRegAlloc() override;
  void addMachineSSAOptimization() override;
  bool addInstSelector() override;
  void addPreSched2() override;
  void addPreEmitPass() override;

  // Global ISEL
  bool addIRTranslator() override;
  bool addLegalizeMachineIR() override;
  void addPreGlobalInstructionSelect() override;
  bool addRegBankSelect() override;
  void addPreRegBankSelect() override;
  void addPreLegalizeMachineIR() override;
  bool addGlobalInstructionSelect() override;
};
} // namespace

TargetPassConfig *GBTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new GBPassConfig(*this, PM);
}

void GBPassConfig::addLatePreRegAlloc() {
  TargetPassConfig::addLatePreRegAlloc();

  if (TM->getOptLevel() != CodeGenOptLevel::None) {
    addPass(createGBEarlyLowerIntoStack(getGBTargetMachine()));
  }
}

void GBPassConfig::addMachineSSAOptimization() {
  TargetPassConfig::addMachineSSAOptimization();

  // Run after LICM and fold immediates back into loops
  // TODO: We will undo this if there are registers available after allocation
  if (TM->getOptLevel() != CodeGenOptLevel::None) {
    addPass(createGBFoldImmediates(getGBTargetMachine(), getOptLevel()));
  }
}

bool GBPassConfig::addInstSelector() {
  addPass(createGBISelDag(getGBTargetMachine(), getOptLevel()));
  return false;
}

bool GBPassConfig::addIRTranslator() {
  addPass(new IRTranslator(getOptLevel()));
  return false;
}

bool GBPassConfig::addLegalizeMachineIR() {
  addPass(new Legalizer());
  return false;
}

bool GBPassConfig::addRegBankSelect() {
  addPass(new RegBankSelect());
  return false;
}

void GBPassConfig::addPreLegalizeMachineIR() {
  addPass(createGBPreLegalizeCombiner(getOptLevel()));
}

void GBPassConfig::addPreRegBankSelect() {
  addPass(createGBPostLegalizeCombiner(getOptLevel()));
  addPass(createGBPostLegalizeExpand(getOptLevel()));
  addPass(createGBPostLegalizeCombiner(getOptLevel()));
}

void GBPassConfig::addPreGlobalInstructionSelect() {
  // TODO: serialize increments
}

bool GBPassConfig::addGlobalInstructionSelect() {
  addPass(new InstructionSelect(getOptLevel()));
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

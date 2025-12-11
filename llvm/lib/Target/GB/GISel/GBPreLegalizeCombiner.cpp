#include "GB.h"
#include "GBLegalizerInfo.h"
#include "GBSubtarget.h"
#include "llvm/CodeGen/GlobalISel/CSEInfo.h"
#include "llvm/CodeGen/GlobalISel/Combiner.h"
#include "llvm/CodeGen/GlobalISel/CombinerHelper.h"
#include "llvm/CodeGen/GlobalISel/GIMatchTableExecutorImpl.h"
#include "llvm/CodeGen/GlobalISel/GISelChangeObserver.h"
#include "llvm/CodeGen/GlobalISel/GISelValueTracking.h"
#include "llvm/CodeGen/GlobalISel/GenericMachineInstrs.h"
#include "llvm/CodeGen/GlobalISel/MIPatternMatch.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/Pass.h"
#include "llvm/Passes/OptimizationLevel.h"

#define GET_GICOMBINER_DEPS
#include "GBGenPreLegalizeGICombiner.inc"
#undef GET_GICOMBINER_DEPS

#define DEBUG_TYPE "gb-postlegalize-combiner"

using namespace llvm;
using namespace MIPatternMatch;

namespace {

#define GET_GICOMBINER_TYPES
#include "GBGenPreLegalizeGICombiner.inc"
#undef GET_GICOMBINER_TYPES

class GBPreLegalizeCombinerImpl : public Combiner {
  const CombinerHelper Helper;
  const GBPreLegalizeCombinerImplRuleConfig &RuleConfig;

public:
  GBPreLegalizeCombinerImpl(
      MachineFunction &MF, CombinerInfo &CInfo, const TargetPassConfig *TPC,
      GISelValueTracking &VT, GISelCSEInfo *CSEInfo,
      const GBPreLegalizeCombinerImplRuleConfig &RuleConfig,
      const GBSubtarget &STI, MachineDominatorTree *MDT,
      const LegalizerInfo *LI);

  static const char *getName() { return "GBPreLegalizeCombiner"; }
  bool tryCombineAll(MachineInstr &I) const override;

private:
#define GET_GICOMBINER_CLASS_MEMBERS
#include "GBGenPreLegalizeGICombiner.inc"
#undef GET_GICOMBINER_CLASS_MEMBERS
};

#define GET_GICOMBINER_IMPL
#include "GBGenPreLegalizeGICombiner.inc"
#undef GET_GICOMBINER_IMPL

GBPreLegalizeCombinerImpl::GBPreLegalizeCombinerImpl(
    MachineFunction &MF, CombinerInfo &CInfo, const TargetPassConfig *TPC,
    GISelValueTracking &VT, GISelCSEInfo *CSEInfo,
    const GBPreLegalizeCombinerImplRuleConfig &RuleConfig,
    const GBSubtarget &STI, MachineDominatorTree *MDT, const LegalizerInfo *LI)
    : Combiner(MF, CInfo, TPC, &VT, CSEInfo),
      Helper(Observer, B, /*IsPreLegalize*/ false, &VT, MDT, LI),
      RuleConfig(RuleConfig),
#define GET_GICOMBINER_CONSTRUCTOR_INITS
#include "GBGenPreLegalizeGICombiner.inc"
#undef GET_GICOMBINER_CONSTRUCTOR_INITS
{
}

} // namespace

class GBPreLegalizeCombiner : public MachineFunctionPass {
public:
  static char ID;
  bool IsOptNone;
  GBPreLegalizeCombinerImplRuleConfig RuleConfig;

  GBPreLegalizeCombiner(CodeGenOptLevel OptLvl);

  StringRef getPassName() const override { return "GBPreLegalizeCombiner"; }

  bool runOnMachineFunction(MachineFunction &MF) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;
};

GBPreLegalizeCombiner::GBPreLegalizeCombiner(CodeGenOptLevel OptLvl)
    : MachineFunctionPass(ID), IsOptNone(OptLvl == CodeGenOptLevel::None) {
  if (!RuleConfig.parseCommandLineOption()) {
    report_fatal_error("Invalid rule identifier");
  }
}

bool GBPreLegalizeCombiner::runOnMachineFunction(MachineFunction &MF) {
  assert(!MF.getProperties().hasFailedISel());

  assert(!MF.getProperties().hasLegalized() &&
         "Expected a pre-legalized function?");
  auto *TPC = &getAnalysis<TargetPassConfig>();
  const Function &F = MF.getFunction();
  bool EnableOpt =
      MF.getTarget().getOptLevel() != CodeGenOptLevel::None && !skipFunction(F);

  const GBSubtarget &ST = MF.getSubtarget<GBSubtarget>();
  const auto *LI = ST.getLegalizerInfo();

  GISelValueTracking *VT =
      &getAnalysis<GISelValueTrackingAnalysisLegacy>().get(MF);
  MachineDominatorTree *MDT =
      &getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree();
  GISelCSEAnalysisWrapper &Wrapper =
      getAnalysis<GISelCSEAnalysisWrapperPass>().getCSEWrapper();
  auto *CSEInfo = &Wrapper.get(TPC->getCSEConfig());

  CombinerInfo CInfo(/*AllowIllegalOps*/ true, /*ShouldLegalizeIllegal*/ false,
                     /*LegalizerInfo*/ nullptr, EnableOpt, F.hasOptSize(),
                     F.hasMinSize());
  GBPreLegalizeCombinerImpl Impl(MF, CInfo, TPC, *VT, CSEInfo, RuleConfig, ST,
                                 MDT, LI);
  return Impl.combineMachineInstrs();
}

void GBPreLegalizeCombiner::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetPassConfig>();
  AU.setPreservesCFG();
  getSelectionDAGFallbackAnalysisUsage(AU);
  AU.addRequired<GISelValueTrackingAnalysisLegacy>();
  AU.addPreserved<GISelValueTrackingAnalysisLegacy>();
  AU.addRequired<MachineDominatorTreeWrapperPass>();
  AU.addPreserved<MachineDominatorTreeWrapperPass>();
  AU.addRequired<GISelCSEAnalysisWrapperPass>();
  AU.addPreserved<GISelCSEAnalysisWrapperPass>();
  MachineFunctionPass::getAnalysisUsage(AU);
}

char GBPreLegalizeCombiner::ID = 0;
FunctionPass *llvm::createGBPreLegalizeCombiner(CodeGenOptLevel OptLvl) {
  return new GBPreLegalizeCombiner(OptLvl);
}

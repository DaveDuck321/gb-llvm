#include "GB.h"
#include "GBLegalizerInfo.h"
#include "GBSubtarget.h"
#include "GISel/GBCombinerCommon.h"
#include "llvm/ADT/bit.h"
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
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGenTypes/LowLevelType.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Pass.h"
#include "llvm/Passes/OptimizationLevel.h"

#define GET_GICOMBINER_DEPS
#include "GBGenPostLegalizeGICombiner.inc"
#undef GET_GICOMBINER_DEPS

#define DEBUG_TYPE "gb-postlegalize-combiner"

using namespace llvm;
using namespace llvm::gb;
using namespace MIPatternMatch;

namespace {

#define GET_GICOMBINER_TYPES
#include "GBGenPostLegalizeGICombiner.inc"
#undef GET_GICOMBINER_TYPES

bool matchNormalizeJpCp(MachineInstr &MI,
                        std::function<void(MachineIRBuilder &)> &MatchInfo) {
  auto &MF = *MI.getMF();
  auto &MRI = MF.getRegInfo();

  assert(MI.getOpcode() == GB::G_JP_CP);
  auto Predicate =
      static_cast<ICmpInst::Predicate>(MI.getOperand(0).getPredicate());
  auto Src1 = MI.getOperand(1).getReg();
  auto Src2 = MI.getOperand(2).getReg();
  auto Target = MI.getOperand(3);

  auto Src1RegAndVal = getIConstantVRegValWithLookThrough(Src1, MRI);
  auto Src2RegAndVal = getIConstantVRegValWithLookThrough(Src2, MRI);

  // Normalize the operand order
  if (Src1RegAndVal && !Src2RegAndVal) {
    MatchInfo = [=](MachineIRBuilder &MIR) {
      MIR.buildInstr(GB::G_JP_CP)
          .addPredicate(CmpInst::getSwappedPredicate(Predicate))
          .addReg(Src2)
          .addReg(Src1)
          .add(Target);
    };
    return true;
  }

  if (not Src2RegAndVal) {
    return false;
  }

  auto MatchIntoNewBranchConstant = [&](ICmpInst::Predicate Predicate,
                                        size_t Value) {
    MatchInfo = [=](MachineIRBuilder &MIR) {
      auto Constant = MIR.buildConstant(LLT::scalar(8), Value);
      MIR.buildInstr(GB::G_JP_CP)
          .addPredicate(Predicate)
          .addReg(Src1)
          .addReg(Constant.getReg(0))
          .add(Target);
    };
    return true;
  };

  size_t Src2Value = Src2RegAndVal->Value.getZExtValue();

  // Avoid swaps in isel: convert UGT -> UGE and ULE -> ULT
  if (Predicate == CmpInst::ICMP_UGT && Src2Value != 0xff) {
    return MatchIntoNewBranchConstant(CmpInst::ICMP_UGE, Src2Value + 1);
  }

  if (Predicate == CmpInst::ICMP_ULE && Src2Value != 0xff) {
    return MatchIntoNewBranchConstant(CmpInst::ICMP_ULT, Src2Value + 1);
  }

  // Convert unsigned compares to equalities
  if (Predicate == CmpInst::ICMP_UGE && Src2Value == 1) {
    return MatchIntoNewBranchConstant(CmpInst::ICMP_NE, 0);
  }

  if (Predicate == CmpInst::ICMP_ULT && Src2Value == 1) {
    return MatchIntoNewBranchConstant(CmpInst::ICMP_EQ, 0);
  }

  return false;
}

bool matchJpCmpToJpBinaryOp(
    MachineInstr &MI, std::function<void(MachineIRBuilder &)> &MatchInfo) {
  auto &MF = *MI.getMF();
  auto &MRI = MF.getRegInfo();

  assert(MI.getOpcode() == GB::G_JP_CP);
  auto Predicate =
      static_cast<ICmpInst::Predicate>(MI.getOperand(0).getPredicate());
  auto Src1 = MI.getOperand(1).getReg();
  auto Src2 = MI.getOperand(2).getReg();
  auto Target = MI.getOperand(3);

  auto *Src1DefOp = MRI.getOneDef(Src1);
  assert(Src1DefOp);
  auto &Src1DefMI = *Src1DefOp->getParent();

  // Match BIT
  bool DidMatchBitMask = [&] {
    if (!ICmpInst::isEquality(Predicate)) {
      return false;
    }

    if (!MRI.hasOneNonDBGUse(Src1)) {
      // We shouldn't waste time here if we still need Src1
      return false;
    }

    auto Src2RegAndVal = getIConstantVRegValWithLookThrough(Src2, MRI);
    if (!Src2RegAndVal.has_value()) {
      return false;
    }

    auto Src2Imm = Src2RegAndVal->Value.getZExtValue();
    if (llvm::popcount(Src2Imm) != 1 && Src2Imm != 0) {
      return false;
    }

    if (Src1DefMI.getOpcode() != TargetOpcode::G_AND) {
      return false;
    }

    auto Src1OtherReg = Src1DefMI.getOperand(1).getReg();
    auto Src1MaskValue = getIConstantVRegValWithLookThrough(
        Src1DefMI.getOperand(2).getReg(), MRI);
    if (!Src1MaskValue.has_value()) {
      return false;
    }

    auto Src1MaskImm = Src1MaskValue->Value.getZExtValue();
    if (llvm::popcount(Src1MaskImm) != 1 ||
        (Src1MaskImm != Src2Imm && Src2Imm != 0)) {
      // We're expecting a single-bit bitmask
      return false;
    }

    // TODO: this would be easier if I normalized more prior to this
    auto Flag = ((Src2Imm == 0) ^ (Predicate == ICmpInst::ICMP_NE))
                    ? GBFlag::Z
                    : GBFlag::NZ;
    auto Bit = llvm::countr_zero(Src1MaskImm);
    MatchInfo = [=](MachineIRBuilder &MIR) {
      MIR.buildInstr(GB::G_JP_BINARY_OP)
          .addImm(Flag)
          .add(Target)
          .addImm(GB::BIT_r)
          .addUse(Src1OtherReg)
          .addImm(Bit);
    };

    return true;
  }();
  if (DidMatchBitMask) {
    return true;
  }

  return false;
}

class GBPostLegalizeCombinerImpl : public Combiner {
  const CombinerHelper Helper;
  const GBPostLegalizeCombinerImplRuleConfig &RuleConfig;

public:
  GBPostLegalizeCombinerImpl(
      MachineFunction &MF, CombinerInfo &CInfo, const TargetPassConfig *TPC,
      GISelValueTracking &VT, GISelCSEInfo *CSEInfo,
      const GBPostLegalizeCombinerImplRuleConfig &RuleConfig,
      const GBSubtarget &STI, MachineDominatorTree *MDT,
      const LegalizerInfo *LI);

  static const char *getName() { return "GBPostLegalizeCombiner"; }
  bool tryCombineAll(MachineInstr &I) const override;

private:
#define GET_GICOMBINER_CLASS_MEMBERS
#include "GBGenPostLegalizeGICombiner.inc"
#undef GET_GICOMBINER_CLASS_MEMBERS
};

#define GET_GICOMBINER_IMPL
#include "GBGenPostLegalizeGICombiner.inc"
#undef GET_GICOMBINER_IMPL

GBPostLegalizeCombinerImpl::GBPostLegalizeCombinerImpl(
    MachineFunction &MF, CombinerInfo &CInfo, const TargetPassConfig *TPC,
    GISelValueTracking &VT, GISelCSEInfo *CSEInfo,
    const GBPostLegalizeCombinerImplRuleConfig &RuleConfig,
    const GBSubtarget &STI, MachineDominatorTree *MDT, const LegalizerInfo *LI)
    : Combiner(MF, CInfo, TPC, &VT, CSEInfo),
      Helper(Observer, B, /*IsPreLegalize*/ false, &VT, MDT, LI),
      RuleConfig(RuleConfig),
#define GET_GICOMBINER_CONSTRUCTOR_INITS
#include "GBGenPostLegalizeGICombiner.inc"
#undef GET_GICOMBINER_CONSTRUCTOR_INITS
{
}

} // namespace

class GBPostLegalizeCombiner : public MachineFunctionPass {
public:
  static char ID;
  bool IsOptNone;
  GBPostLegalizeCombinerImplRuleConfig RuleConfig;

  GBPostLegalizeCombiner(CodeGenOptLevel OptLvl);

  StringRef getPassName() const override { return "GBPostLegalizeCombiner"; }

  bool runOnMachineFunction(MachineFunction &MF) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;
};

GBPostLegalizeCombiner::GBPostLegalizeCombiner(CodeGenOptLevel OptLvl)
    : MachineFunctionPass(ID), IsOptNone(OptLvl == CodeGenOptLevel::None) {
  if (!RuleConfig.parseCommandLineOption()) {
    report_fatal_error("Invalid rule identifier");
  }
}

bool GBPostLegalizeCombiner::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "======== GBPostLegalizeCombiner ========\n";
             dbgs() << "Starting with: "; MF.dump());

  assert(!MF.getProperties().hasFailedISel());

  assert(MF.getProperties().hasLegalized() && "Expected a legalized function?");
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
  GBPostLegalizeCombinerImpl Impl(MF, CInfo, TPC, *VT, CSEInfo, RuleConfig, ST,
                                  MDT, LI);
  auto Result = Impl.combineMachineInstrs();
  LLVM_DEBUG(
      dbgs() << "After GBPostLegalizeCombiner:\n";
      if (Result) { MF.dump(); } else { dbgs() << "No change!\n"; });
  return Result;
}

void GBPostLegalizeCombiner::getAnalysisUsage(AnalysisUsage &AU) const {
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

char GBPostLegalizeCombiner::ID = 0;
FunctionPass *llvm::createGBPostLegalizeCombiner(CodeGenOptLevel OptLvl) {
  return new GBPostLegalizeCombiner(OptLvl);
}

#ifndef LLVM_LIB_TARGET_GB_GBTARGETTRANSFORMINFO_H
#define LLVM_LIB_TARGET_GB_GBTARGETTRANSFORMINFO_H

#include "GBSubtarget.h"
#include "GBTargetMachine.h"
#include "llvm/Analysis/IVDescriptors.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"

#define GB_TTI_TRACE_FUNCTION_CALLS 0
#if GB_TTI_TRACE_FUNCTION_CALLS
#define TRACE_FN(name) dbgs() << name << "\n";
#else
#define TRACE_FN(name) ;
#endif
#undef GB_TTI_TRACE_FUNCTION_CALLS

namespace llvm {

class GBTTIImpl : public BasicTTIImplBase<GBTTIImpl> {
  using BaseT = BasicTTIImplBase<GBTTIImpl>;
  using TTI = TargetTransformInfo;

  friend BaseT;

  const GBSubtarget *ST;
  const GBTargetLowering *TLI;

  const GBSubtarget *getST() const { return ST; }
  const GBTargetLowering *getTLI() const { return TLI; }

public:
  explicit GBTTIImpl(const GBTargetMachine *TM, const Function &F)
      : BaseT(TM, F.getParent()->getDataLayout()),
        ST(TM->getGBSubtargetImpl(F)), TLI(ST->getTargetLowering()) {}

  bool isTypeConversionAlwaysUndesirable(unsigned FromWidth,
                                         unsigned ToWidth) const override {
    return FromWidth == 8 && ToWidth > 8;
  }

  InstructionCost getTypeCostMultiplier(EVT Type) const override {
    auto SizeInBits = Type.getSizeInBits();
    if (SizeInBits <= 8) {
      return 1;
    }
    if (SizeInBits == 16) {
      return 2;
    }
    return 3;
  }

  unsigned getNumberOfRegisters(unsigned ClassID) const override {
    if (ClassID == 0) {
      return 5; // A, B, C, D, E
    }
    return 3; // HL, BC, DE
  }

  unsigned getRegisterClassForType(bool Vector,
                                   Type *Ty = nullptr) const override {
    if (!Vector && Ty != nullptr && Ty->getPrimitiveSizeInBits() == 8) {
      // We're an 8-bit register
      return 0;
    }

    // Else assume that we're a 16 bit register
    return 1;
  };

  bool isLSRCostLess(const TTI::LSRCost &C1,
                     const TTI::LSRCost &C2) const override {
    // Increments are cheap but multiplications and arbitrary 16-bit additions
    // are expensive.
    return std::tie(C1.NumRegs, C1.NumIVMuls, C1.NumBaseAdds, C1.AddRecCost,
                    C1.ScaleCost, C1.ImmCost, C1.SetupCost) <
           std::tie(C2.NumRegs, C2.NumIVMuls, C2.NumBaseAdds, C2.AddRecCost,
                    C2.ScaleCost, C2.ImmCost, C2.SetupCost);
  }

  TTI::AddressingModeKind
  getPreferredAddressingMode(const Loop *L,
                             ScalarEvolution *SE) const override {
    TRACE_FN("getPreferredAddressingMode");
    return TTI::AMK_PostIndexed;
  }

  bool isIndexedLoadLegal(TargetTransformInfo::MemIndexedMode Mode,
                          Type *Ty) const override {
    TRACE_FN("isIndexedLoadLegal");
    return Ty->getPrimitiveSizeInBits() == 16 &&
           (Mode == TargetTransformInfo::MIM_PostInc);
  }
  bool isIndexedStoreLegal(TargetTransformInfo::MemIndexedMode Mode,
                           Type *Ty) const override {
    TRACE_FN("isIndexedStoreLegal");
    return isIndexedLoadLegal(Mode, Ty);
  }

  bool canMacroFuseCmp() const override {
    TRACE_FN("canMacroFuseCmp");
    return true;
  }

  const char *getRegisterClassName(unsigned ClassID) const override {
    switch (ClassID) {
    default:
      return "GB::Unknown Register Class";
    case 0:
      return "GB::r8";
    case 1:
      return "GB::Other Register Class";
    }
  }

  TypeSize
  getRegisterBitWidth(TargetTransformInfo::RegisterKind) const override {
    TRACE_FN("getRegisterBitWidth");
    // Hmm, seems unnecessary when we already have the datalayout
    // Let's return our preferred size
    return TypeSize::getFixed(8);
  }

  // unsigned getInliningThresholdMultiplier() const override { return 1; }
  // unsigned getInliningCostBenefitAnalysisSavingsMultiplier() const override {
  //   return 1;
  // }

  // unsigned getInliningCostBenefitAnalysisProfitableMultiplier() const override {
  //   return 1000;
  // }

  // int getInliningLastCallToStaticBonus() const override {
  //   // This is the value of InlineConstants::LastCallToStaticBonus before it was
  //   // removed along with the introduction of this function.
  //   return 0;
  // }

  // unsigned adjustInliningThreshold(const CallBase *CB) const override {
  //   return 0;
  // }

  // unsigned getCallerAllocaCost(const CallBase *CB,
  //                              const AllocaInst *AI) const override {
  //   return 0;
  // };

  // InstructionCost getMemcpyCost(const Instruction *I) const override {
  //   return TTI::TCC_Expensive;
  // }

  //uint64_t getMaxMemIntrinsicInlineSizeThreshold() const override { return 8; }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_GB_GBTARGETTRANSFORMINFO_H

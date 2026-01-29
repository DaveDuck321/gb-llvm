#include "GISel/GBCombinerCommon.h"
#include "llvm/ADT/APInt.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"

#include <functional>

using namespace llvm::gb;
using namespace llvm;

namespace llvm::gb {
bool matchCombineICMPToUnsigned(
    MachineInstr &MI, std::function<void(MachineIRBuilder &)> &MatchInfo) {
  auto Predicate = (ICmpInst::Predicate)MI.getOperand(1).getPredicate();
  switch (Predicate) {
  default:
    return false;
  case CmpInst::ICMP_SGT:
  case CmpInst::ICMP_SGE:
  case CmpInst::ICMP_SLT:
  case CmpInst::ICMP_SLE:
    break;
  }

  auto &MRI = MI.getMF()->getRegInfo();

  auto Dst = MI.getOperand(0).getReg();
  auto LHS = MI.getOperand(2).getReg();
  auto RHS = MI.getOperand(3).getReg();
  auto RHSConstant = getIConstantVRegValWithLookThrough(RHS, MRI);
  if (!RHSConstant.has_value()) {
    return false;
  }
  auto RHSValue = RHSConstant->Value.getSExtValue();
  auto RHSNumBits = RHSConstant->Value.getBitWidth();

  ICmpInst::Predicate NewPredicate = {};
  int64_t NewRHSValue = {};

  if ((Predicate == CmpInst::ICMP_SGT && RHSValue == -1) ||
      (Predicate == CmpInst::ICMP_SGE && RHSValue == 0)) {
    NewPredicate = CmpInst::ICMP_ULT;
    NewRHSValue = (1U << (RHSNumBits - 1)); // First signed number
  } else if ((Predicate == CmpInst::ICMP_SLE && RHSValue == -1) ||
             (Predicate == CmpInst::ICMP_SLT && RHSValue == 0)) {
    NewPredicate = CmpInst::ICMP_UGE;
    NewRHSValue = (1U << (RHSNumBits - 1)); // First signed number
  } else {
    return false;
  }

  MatchInfo = [NewPredicate, NewRHSValue, Dst, LHS, RHS,
               &MRI](MachineIRBuilder &B) {
    auto NewRHSReg = MRI.cloneVirtualRegister(RHS);
    B.buildConstant(NewRHSReg, NewRHSValue);
    B.buildICmp(NewPredicate, Dst, LHS, NewRHSReg);
  };
  return true;
}
} // namespace llvm::gb

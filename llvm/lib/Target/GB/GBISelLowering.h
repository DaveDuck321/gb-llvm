#ifndef LLVM_LIB_TARGET_GB_GBISELLOWERING_H
#define LLVM_LIB_TARGET_GB_GBISELLOWERING_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetCallingConv.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class GBSubtarget;
namespace GBISD {
enum NodeType : unsigned {
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  CP,
  BR_CC,
  RET,
};
} // namespace GBISD

class GBTargetLowering : public TargetLowering {
public:
  GBTargetLowering(const TargetMachine &, const GBSubtarget &);

  SDValue LowerOperation(SDValue Op, SelectionDAG &) const override;
  const char *getTargetNodeName(unsigned Opcode) const override;

private:
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID, bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &,
                               SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutsVals, const SDLoc &DL,
                      SelectionDAG &) const override;
  bool shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                         Type *Ty) const override;
};
} // namespace llvm
#endif

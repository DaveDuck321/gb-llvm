#ifndef LLVM_LIB_TARGET_GB_GBISELLOWERING_H
#define LLVM_LIB_TARGET_GB_GBISELLOWERING_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetCallingConv.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class GBSubtarget;
namespace GBISD {
enum NodeType : unsigned {
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  ADDR_WRAPPER,
  BR_CC,
  COMBINE_CHAIN,
  COMBINE,
  CP,
  LOWER,
  RET,
  RLA,
  RLCA,
  UPPER,
};
} // namespace GBISD

class GBTargetLowering : public TargetLowering {
  const GBSubtarget &Subtarget;

public:
  GBTargetLowering(const TargetMachine &, const GBSubtarget &);

  SDValue LowerOperation(SDValue Op, SelectionDAG &) const override;
  const char *getTargetNodeName(unsigned Opcode) const override;

private:
  SDValue LowerLOAD16(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSTORE16(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSETCC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerBinaryOp(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID, bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &,
                               SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutsVals, const SDLoc &DL,
                      SelectionDAG &) const override;
  EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                         EVT VT) const override;
  bool isSelectSupported(SelectSupportKind) const override;
  bool convertSetCCLogicToBitwiseLogic(EVT VT) const override;
  bool shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                         Type *Ty) const override;

  bool allowsMisalignedMemoryAccesses(EVT, unsigned AddrSpace, Align,
                                      MachineMemOperand::Flags,
                                      unsigned *Fast) const override;
};
} // namespace llvm
#endif

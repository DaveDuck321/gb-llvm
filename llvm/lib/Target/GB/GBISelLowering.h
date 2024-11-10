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
  CALL,
  COMBINE,
  CP,
  INC16,
  LD_HL_SP,
  LOWER,
  RET,
  RLA,
  RLCA,
  SELECT_CC,
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
  SDValue LowerCMP_CC(SDValue LHS, SDValue RHS, ISD::CondCode &CCode,
                      SelectionDAG &DAG, SDLoc DL) const;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSETCC(SDValue Op, SelectionDAG &DAG) const;

  SDValue LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerExternalSymbol(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSignExtendInReg(SDValue Op, SelectionDAG &DAG) const;

  SDValue LowerCall(CallLoweringInfo &CLI,
                    SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID, bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &,
                               SmallVectorImpl<SDValue> &InVals) const override;
  bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                      bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      LLVMContext &Context) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutsVals, const SDLoc &DL,
                      SelectionDAG &) const override;

  MachineBasicBlock *
  EmitInstrWithCustomInserter(MachineInstr &MI,
                              MachineBasicBlock *MBB) const override;

  MachineBasicBlock *
  emitSelectCCWithCustomInserter(MachineInstr &MI,
                                 MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitUnknownShiftWithCustomInserter(MachineInstr &MI,
                                     MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitConstantShiftWithCustomInserter(MachineInstr &MI,
                                      MachineBasicBlock *MBB) const;

  MVT getScalarShiftAmountTy(const DataLayout &, EVT) const override;
  EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                         EVT VT) const override;
  bool isSelectSupported(SelectSupportKind) const override;
  bool convertSetCCLogicToBitwiseLogic(EVT VT) const override;
  bool shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                         Type *Ty) const override;

  bool allowsMisalignedMemoryAccesses(EVT, unsigned AddrSpace, Align,
                                      MachineMemOperand::Flags,
                                      unsigned *Fast) const override;

  void splitValue(SelectionDAG &DAG, SDValue Value, SDValue &Lo,
                  SDValue &Hi) const override;
  SDValue mergeValues(SelectionDAG &DAG, SDValue Lo, SDValue Hi,
                      bool &FreshNode) const override;

  EVT getTypeToTransformTo(LLVMContext &Context, EVT VT) const override;
  LegalizeTypeAction getTypeActionForOperand(SDNode *N,
                                             unsigned Operand) const override;
  LegalizeTypeAction getTypeActionForResult(SDNode *N,
                                            unsigned Result) const override;
};
} // namespace llvm
#endif

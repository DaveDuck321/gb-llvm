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
  ASHR,
  ATOMIC_BIT_RESET,
  ATOMIC_BIT_SET,
  ATOMIC_DEC,
  ATOMIC_INC,
  BR_CC,
  CALL,
  COMBINE,
  CP,
  DEC16,
  INC16,
  LD_HL_SP,
  LOWER,
  LSHR,
  RET,
  RETI,
  RL,
  RR,
  SBR_CC,
  SELECT_CC,
  SHL,
  SSELECT_CC,
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
  SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

  SDValue LowerCMP_CC(SDValue LHS, SDValue RHS, ISD::CondCode &CCode,
                      SelectionDAG &DAG, SDLoc DL) const;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSETCC(SDValue Op, SelectionDAG &DAG) const;

  SDValue LowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerExternalSymbol(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSignExtendInReg(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerAtomicFence(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerAtomicReadModifyWrite(SDValue Op, SelectionDAG &DAG) const;

  SDValue LowerCall(CallLoweringInfo &CLI,
                    SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID, bool IsVarArg,
                               const SmallVectorImpl<ISD::InputArg> &Ins,
                               const SDLoc &DL, SelectionDAG &,
                               SmallVectorImpl<SDValue> &InVals) const override;
  bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                      bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      LLVMContext &Context, const Type *RetTy) const override;
  EVT getTypeForExtReturn(LLVMContext &Context, EVT VT,
                          ISD::NodeType ExtendKind) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID, bool IsVarArg,
                      const SmallVectorImpl<ISD::OutputArg> &Outs,
                      const SmallVectorImpl<SDValue> &OutsVals, const SDLoc &DL,
                      SelectionDAG &) const override;

public:
  MachineBasicBlock *
  EmitInstrWithCustomInserter(MachineInstr &MI,
                              MachineBasicBlock *MBB) const override;

  MachineBasicBlock *
  emitUnknownSBRCCWithCustomInserter(MachineInstr &MI,
                                     MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitConstantSBRCCWithCustomInserter(MachineInstr &MI,
                                      MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitSelectCCWithCustomInserter(MachineInstr &MI,
                                 MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitFlagSelectCCWithCustomInserter(MachineInstr &MI,
                                     MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitSignedSelectCCWithCustomInserter(MachineInstr &MI,
                                       MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitUnknownShiftWithCustomInserter(MachineInstr &MI,
                                     MachineBasicBlock *MBB) const;

  MachineBasicBlock *
  emitConstantShiftWithCustomInserter(MachineInstr &MI,
                                      MachineBasicBlock *MBB) const;

  bool expandShiftByConstant(SelectionDAG &DAG, SDNode *N, const APInt &Amt,
                             SDValue &Lo, SDValue &Hi) const override;

  unsigned maximumLegalStoreInBits() const override;

  ShiftLegalizationStrategy
  preferredShiftLegalizationStrategy(SelectionDAG &DAG, SDNode *N,
                                     unsigned ExpansionFactor) const override;
  MVT getScalarShiftAmountTy(const DataLayout &, EVT) const override;
  EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                         EVT VT) const override;

  // TODO: try setting when benchmarks are more meaningful
  // bool shouldAvoidTransformToShift(EVT VT, unsigned Amount) const override
  bool useSoftFloat() const override;

  EVT getOptimalMemOpType(const MemOp &Op,
                          const AttributeList &) const override;

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

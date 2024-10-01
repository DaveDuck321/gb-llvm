#include "GBISelLowering.h"
#include "GBInstrInfo.h"
#include "GBRegisterInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/ModuleSummaryIndex.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Target/TargetMachine.h"
#include <cstddef>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "gb-lower"

GBTargetLowering::GBTargetLowering(const TargetMachine &TM,
                                   const GBSubtarget &STI)
    : TargetLowering(TM), Subtarget(STI) {
  addRegisterClass(MVT::i8, &GB::GPR8RegClass);
  addRegisterClass(MVT::i16, &GB::GPR16RegClass);
  computeRegisterProperties(STI.getRegisterInfo());

  // setStackPointerRegisterToSaveRestore(GB::SP);
  setMinFunctionAlignment(Align{1});
  setPrefFunctionAlignment(Align{1});
  setMinimumJumpTableEntries(INT_MAX); // Disable jump tables

  // Undefined bools allow a fast setcc implementation using the rla instruction
  // This makes select 4 cycles slower compared to
  // ZeroOrNegativeOneBooleanContent but makes setcc 12-20 cycles faster.
  setBooleanContents(UndefinedBooleanContent);

  // AssertSext, AssertZext, AssertAlign
  // RegisterMask
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);
  // GlobalTLSAddress
  setOperationAction(ISD::FrameIndex, MVT::i16, Legal);
  // JumpTable
  // ConstantPool
  // ExternalSymbol
  setOperationAction(ISD::BlockAddress, MVT::i16, Custom);
  // GLOBAL_OFFSET_TABLE
  // FRAMEADDR
  // RETURNADDR
  // ADDROFRETURNADDR
  // SPONENTRY
  // LOCAL_RECOVER, READ_REGISTER, WRITE_REGISTER
  // FRAME_TO_ARGS_OFFSET
  // MCSymbol
  // INTRINSIC_WO_CHAIN, INTRINSIC_W_CHAIN, INTRINSIC_VOID
  for (const auto &ArithmeticOp :
       {ISD::SUB, ISD::ADD, ISD::ADDC, ISD::SUBC, ISD::ADDE, ISD::SUBE}) {
    setOperationAction(ArithmeticOp, MVT::i8, Legal);
  }
  // TODO GB: setOperationAction(ISD::ADD, MVT::i16, Legal);
  //  MUL, SDIV, UDIV, SREM, UREM
  //  SMUL_LOHI, UMUL_LOHI
  //  SDIVREM, UDIVREM
  //  CARRY_FALSE
  //  UADDO_CARRY         // Expanded
  //  USUBO_CARRY         // Expanded
  //  SADDO_CARRY         // Expanded
  //  SSUBO_CARRY         // Expanded
  //  SADDO               // Expanded
  //  UADDO               // Expanded
  //  SSUBO               // Expanded
  //  USUBO               // Expanded
  //  SMULO               // Expanded
  //  UMULO               // Expanded
  //  MULHU, MULHS
  for (const auto &BinaryOp : {ISD::AND, ISD::OR, ISD::XOR}) {
    setOperationAction(BinaryOp, MVT::i8, Legal);
  }
  for (const auto &BinaryOp :
       {ISD::SHL, ISD::SRA, ISD::SRL, ISD::ROTL, ISD::ROTR}) {
    setOperationAction(BinaryOp, MVT::i8, Legal);
  }
  // BSWAP
  // CTTZ, CTLZ
  // CTPOP
  // SETCC, SELECT, SELECT_CC
  setOperationAction(ISD::SETCC, MVT::i8, Custom);
  setOperationAction(ISD::SELECT, MVT::i8, Expand); // -> select_cc
  setOperationAction(ISD::SELECT_CC, MVT::i8, Custom);
  // SETCCCARRY         // Expanded
  // SHL_PARTS, SRA_PARTS, SRL_PARTS
  setOperationAction(ISD::TRUNCATE, MVT::i8, Legal); // i16 to i8
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Custom);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand); // -> SIGN_EXTEND
  // BITCAST
  // ADDRSPACECAST
  setLoadExtAction(ISD::SEXTLOAD, MVT::i8, MVT::i1, Expand);
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i8, MVT::i1, Expand);
  for (const auto &MemoryOp : {ISD::LOAD, ISD::STORE}) {
    setOperationAction(MemoryOp, MVT::i8, Legal);
  }
  //  DYNAMIC_STACKALLOC
  //  BR_JT
  //  TODO: brcond should generate rra as part of the br_cc pattern
  setOperationAction(ISD::BR, MVT::Other, Legal);
  setOperationAction(ISD::BRIND, MVT::Other, Legal);
  setOperationAction(ISD::BRCOND, MVT::Other, Expand); // -> br_cc
  setOperationAction(ISD::BR_CC, MVT::i8, Custom);
  // INLINEASM, INLINEASM_BR
  // ANNOTATION_LABEL
  // STACKSAVE, STACKRESTORE
  // CALLSEQ_START, CALLSEQ_END
  // VAARG, VACOPY, VAEND, VASTART
  // PREALLOCATED_SETUP, PREALLOCATED_ARG
  // SRCVALUE
  // MDNODE_SDNODE
  // PCMARKER
  // READCYCLECOUNTER
  // HANDLENODE
  // INIT_TRAMPOLINE, ADJUST_TRAMPOLINE
  setOperationAction(ISD::TRAP, MVT::Other, Legal);
  // LIFETIME_START, LIFETIME_END
  // GET_DYNAMIC_AREA_OFFSET
  // PSEUDO_PROBE
  // STACKMAP
  // PATCHPOINT
}

SDValue GBTargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default:
    report_fatal_error("GBTargetLowering::LowerOperation unimplemented!!");
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::SETCC:
    return LowerSETCC(Op, DAG);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG);
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:
    return LowerBlockAddress(Op, DAG);
  case ISD::SIGN_EXTEND_INREG:
    return LowerSignExtendInReg(Op, DAG);
  }
}

SDValue GBTargetLowering::LowerCMP_CC(SDValue LHS, SDValue RHS,
                                      ISD::CondCode &CCode, SelectionDAG &DAG,
                                      SDLoc DL) const {
  assert(LHS.getValueType() == MVT::i8 && RHS.getValueType() == MVT::i8);
  auto ConvertToSubCp = [&](ISD::CondCode UCond, unsigned Val) {
    SDValue Sub = DAG.getNode(ISD::SUB, DL, LHS.getValueType(), LHS, RHS);
    LHS = Sub;
    RHS = DAG.getConstant(Val, DL, MVT::i8);
    CCode = UCond;
  };

  switch (CCode) {
  default:
    llvm_unreachable("unrecognized condition code");

  // Native support
  case ISD::CondCode::SETUGT:
  case ISD::CondCode::SETUGE:
  case ISD::CondCode::SETULT:
  case ISD::CondCode::SETULE:
  case ISD::CondCode::SETEQ:
  case ISD::CondCode::SETNE:
    break;

  // We doesn't have hardware support for signed cmp, convert to subtract
  // and unsigned cmp
  case ISD::CondCode::SETGT:
    std::swap(LHS, RHS);
    [[fallthrough]];
  case ISD::CondCode::SETLT:
    ConvertToSubCp(ISD::CondCode::SETUGE, 0x80);
    break;

  case ISD::CondCode::SETLE:
    std::swap(LHS, RHS);
    [[fallthrough]];
  case ISD::CondCode::SETGE:
    // Use ULT and 0x80 here rather than ULE and 0x7f so we can emit CPI rather
    // than an intermediate register copy
    // TODO GB: we could improve codegen by inversing the jump condition and
    // using an rla... But... I want this to work in the general case
    ConvertToSubCp(ISD::CondCode::SETULT, 0x80);
    break;
  }

  SDValue CC = DAG.getCondCode(CCode);
  return DAG.getNode(GBISD::CP, DL, MVT::Glue, CC, LHS, RHS);
}

SDValue GBTargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CCode =
      dyn_cast<CondCodeSDNode>(Op.getOperand(1).getNode())->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue BB = Op.getOperand(4);
  SDLoc DL = Op;

  SDValue CmpGlue = LowerCMP_CC(LHS, RHS, CCode, DAG, DL);
  SDValue CC = DAG.getCondCode(CCode);
  return DAG.getNode(GBISD::BR_CC, DL, MVT::Other, Chain, CC, BB, CmpGlue);
}

SDValue GBTargetLowering::LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue IfTrue = Op->getOperand(2);
  SDValue IfFalse = Op->getOperand(3);
  ISD::CondCode CCode =
      dyn_cast<CondCodeSDNode>(Op.getOperand(4).getNode())->get();
  SDLoc DL = Op;

  SDValue CmpGlue = LowerCMP_CC(LHS, RHS, CCode, DAG, DL);
  SDValue CC = DAG.getCondCode(CCode);
  return DAG.getNode(GBISD::SELECT_CC, DL, IfTrue.getValueType(), CC, IfTrue,
                     IfFalse, CmpGlue);
}

SDValue GBTargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CCode =
      dyn_cast<CondCodeSDNode>(Op.getOperand(2).getNode())->get();
  SDLoc DL = Op;

  // TODO GB: can I move this entire table into the pattern matching?
  bool ReverseResult = false;
  switch (CCode) {
  default:
    llvm_unreachable("unrecognized condition code");

  // Native support
  case ISD::CondCode::SETUGT:
  case ISD::CondCode::SETULT:
    break;

  // a == b:
  // and a, b  (res < b if a != b)
  // cmp b
  // rla
  case ISD::CondCode::SETEQ:
    ReverseResult = true;
    [[fallthrough]];
  case ISD::CondCode::SETNE: {
    SDValue And = DAG.getNode(ISD::AND, DL, LHS.getValueType(), LHS, RHS);
    LHS = And;
    CCode = ISD::CondCode::SETULT;
    break;
  }

  // a >= b
  // !(a < b)
  case ISD::CondCode::SETUGE:
    CCode = ISD::CondCode::SETULT;
    ReverseResult = true;
    break;
  case ISD::CondCode::SETULE:
    CCode = ISD::CondCode::SETUGT;
    ReverseResult = true;
    break;

  // (signed) a > (signed) b
  // sign(b - a)
  case ISD::CondCode::SETGT:
    std::swap(LHS, RHS);
    [[fallthrough]];
  case ISD::CondCode::SETLT: {
    // Subtract and move the sign bit directly into the accumulator
    SDValue Sub = DAG.getNode(ISD::SUB, DL, MVT::i8, LHS, RHS);
    SDValue Result = DAG.getNode(GBISD::RLCA, DL, MVT::i8, Sub);
    return Result;
  }

  // (signed) a >= (signed) b
  // !sign(a - b)
  case ISD::CondCode::SETLE:
    std::swap(LHS, RHS);
    [[fallthrough]];
  case ISD::CondCode::SETGE:
    // Subtract, move the sign bit directly into the accumulator and reverse
    SDValue Sub = DAG.getNode(ISD::SUB, DL, MVT::i8, LHS, RHS);
    SDValue Result = DAG.getNode(GBISD::RLCA, DL, MVT::i8, Sub);
    return DAG.getNOT(DL, Result, MVT::i8);
  }

  SDValue CC = DAG.getCondCode(CCode);
  SDValue Cmp = DAG.getNode(GBISD::CP, DL, MVT::Glue, CC, LHS, RHS);

  // Primary operand is ignored, only glue matters
  SDValue Result =
      DAG.getNode(GBISD::RLA, DL, MVT::i8, DAG.getUNDEF(MVT::i8), Cmp);
  if (ReverseResult) {
    Result = DAG.getNOT(DL, Result, MVT::i8);
  }
  return Result;
}

SDValue GBTargetLowering::LowerBlockAddress(SDValue Op,
                                            SelectionDAG &DAG) const {
  BlockAddressSDNode *Node = dyn_cast<BlockAddressSDNode>(Op);
  assert(Node);

  SDLoc DL = Op;
  SDValue TargetAddr = DAG.getTargetBlockAddress(Node->getBlockAddress(),
                                                 MVT::i16, Node->getOffset());
  return DAG.getNode(GBISD::ADDR_WRAPPER, DL, MVT::i16, TargetAddr);
}

SDValue GBTargetLowering::LowerGlobalAddress(SDValue Op,
                                             SelectionDAG &DAG) const {
  GlobalAddressSDNode *Node = dyn_cast<GlobalAddressSDNode>(Op);
  assert(Node);

  SDLoc DL = Op;
  SDValue TargetAddr =
      DAG.getTargetGlobalAddress(Node->getGlobal(), DL, MVT::i16);
  return DAG.getNode(GBISD::ADDR_WRAPPER, DL, MVT::i16, TargetAddr);
}

SDValue GBTargetLowering::LowerSignExtendInReg(SDValue Op,
                                               SelectionDAG &DAG) const {
  EVT ExtraVT = cast<VTSDNode>(Op.getOperand(1))->getVT();
  assert(ExtraVT.getSizeInBits() == 1); // Everything else is lowered out-of-reg

  SDLoc DL = Op;

  // Optimized to exclusively use 8-bit operations but otherwise the same
  // procedure as LegalizeDAG.cpp

  // LLVM tries to be helpful and gives us the value in a 16 bit register via
  // AnyExtend, lets just undo that... hopefully this is folded into nothing.
  SDValue Trunc = DAG.getNode(ISD::TRUNCATE, DL, MVT::i8, Op.getOperand(0));

  SDValue And = DAG.getNode(ISD::AND, DL, MVT::i8, Trunc,
                            DAG.getConstant(1, DL, MVT::i8));
  return DAG.getNode(ISD::SUB, DL, MVT::i8, DAG.getConstant(0, DL, MVT::i8),
                     And);
}

const char *GBTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((GBISD::NodeType)Opcode) {
  case GBISD::FIRST_NUMBER:
    break;
  case GBISD::ADDR_WRAPPER:
    return "GBISD::ADDR_WRAPPER";
  case GBISD::BR_CC:
    return "GBISD::BR_CC";
  case GBISD::CALL:
    return "GBISD::CALL";
  case GBISD::COMBINE:
    return "GBISD::COMBINE";
  case GBISD::CP:
    return "GBISD::CP";
  case GBISD::LD_HL_SP:
    return "GBISD::LD_HL_SP";
  case GBISD::LOWER:
    return "GBISD::LOWER";
  case GBISD::RET:
    return "GBISD::RET";
  case GBISD::RLA:
    return "GBISD::RLA";
  case GBISD::RLCA:
    return "GBISD::RLCA";
  case GBISD::SELECT_CC:
    return "GBISD::SELECT_CC";
  case GBISD::UPPER:
    return "GBISD::UPPER";
  }
  return nullptr;
}

#include "GBGenCallingConv.inc"

SDValue GBTargetLowering::LowerCall(CallLoweringInfo &CLI,
                                    SmallVectorImpl<SDValue> &InVals) const {
  assert(not CLI.IsVarArg);

  SelectionDAG &DAG = CLI.DAG;
  MachineFunction &MF = DAG.getMachineFunction();

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState ArgCCInfo(CLI.CallConv, CLI.IsVarArg, MF, ArgLocs, *DAG.getContext());
  ArgCCInfo.AnalyzeCallOperands(CLI.Outs, CC_GB);

  unsigned NumBytes = ArgCCInfo.getStackSize();

  SDValue Chain = CLI.Chain;
  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, CLI.DL);

  SmallVector<SDValue, 8> MemOpChains;
  SmallVector<std::pair<unsigned, SDValue>, 8> RegToPass;
  for (unsigned I = 0; I < ArgLocs.size(); ++I) {
    CCValAssign &VA = ArgLocs[I];
    SDValue ArgValue = CLI.OutVals[I];

    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown argument location");
    case CCValAssign::Full:
      break;
    }

    if (VA.isRegLoc()) {
      RegToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
    } else {
      int Offset = VA.getLocMemOffset();
      assert(isInt<8>(Offset));
      assert(not CLI.Outs[I].Flags.isByVal());

      // Generated prior to type legalization, a 16-bit store is fine here!
      SDValue Address = DAG.getNode(GBISD::LD_HL_SP, CLI.DL, MVT::i16,
                                    DAG.getConstant(Offset, CLI.DL, MVT::i8));
      MemOpChains.push_back(
          DAG.getStore(Chain, CLI.DL, ArgValue, Address,
                       MachinePointerInfo::getStack(MF, Offset)));
    }
  }

  if (!MemOpChains.empty()) {
    Chain = DAG.getNode(ISD::TokenFactor, CLI.DL, MVT::Other, MemOpChains);
  }

  // Copy registers into physical register as per the calling convention
  SDValue Glue;
  for (auto &Reg : RegToPass) {
    Chain = DAG.getCopyToReg(Chain, CLI.DL, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  SDValue Callee = CLI.Callee;
  // TODO GB: maybe generate a TargetGlobalAddress here?

  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  for (auto &Reg : RegToPass) {
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));
  }

  // Add a register mask for the call-preserved registers
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(MF, CLI.CallConv);
  assert(Mask != nullptr);
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (Glue.getNode()) {
    Ops.push_back(Glue);
  }

  // Emit the call
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  Chain = DAG.getNode(GBISD::CALL, CLI.DL, NodeTys, Ops);
  Glue = Chain.getValue(1);

  Chain = DAG.getCALLSEQ_END(
      Chain, DAG.getConstant(NumBytes, CLI.DL, MVT::i16, true),
      DAG.getConstant(0, CLI.DL, MVT::i16, true), Glue, CLI.DL);
  Glue = Chain.getValue(1);

  // Handle the return values
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RetCCInfo(CLI.CallConv, CLI.IsVarArg, MF, RVLocs, *DAG.getContext());
  RetCCInfo.AnalyzeCallResult(CLI.Ins, RetCC_GB);

  for (auto &VA : RVLocs) {
    SDValue RetValue =
        DAG.getCopyFromReg(Chain, CLI.DL, VA.getLocReg(), VA.getLocVT(), Glue);
    Chain = RetValue.getValue(1);
    Glue = RetValue.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }
  return Chain;
}

SDValue GBTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  if (CallConv != CallingConv::C) {
    report_fatal_error("Unsupported calling convention");
  }
  if (IsVarArg) {
    report_fatal_error("Variadic arguments are not supported");
  }

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  // Use the autogenerated calling convention to assign a location to each
  // argument
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_GB);

  for (unsigned I = 0; I < ArgLocs.size(); I++) {
    auto &VA = ArgLocs[I];
    EVT LocVT = VA.getLocVT();

    SDValue ArgIn;
    if (VA.isRegLoc()) {
      assert(VA.getLocInfo() == CCValAssign::Full);

      MVT ValVT = VA.getValVT();
      MachineRegisterInfo &RegInfo = MF.getRegInfo();
      const auto *RC = getRegClassFor(ValVT);
      Register VReg = RegInfo.createVirtualRegister(RC);
      RegInfo.addLiveIn(VA.getLocReg(), VReg);
      ArgIn = DAG.getCopyFromReg(Chain, DL, VReg, LocVT);
    } else {
      // Stack
      assert(not Ins[I].Flags.isByVal());
      assert(not Ins[I].Flags.isPreallocated());
      assert(VA.isMemLoc());
      EVT ValVT = VA.getValVT();
      EVT PtrVT =
          MVT::getIntegerVT(DAG.getDataLayout().getPointerSizeInBits(0));
      auto FI =
          MFI.CreateFixedObject(ValVT.getStoreSize(), VA.getLocMemOffset(),
                                /* IsImmutable = */ true);

      SDValue FIN = DAG.getFrameIndex(FI, PtrVT);
      ArgIn = DAG.getLoad(
          LocVT, DL, Chain, FIN,
          MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
    }
    InVals.push_back(ArgIn);
  }
  return Chain;
}

bool GBTargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
  assert(not IsVarArg);
  assert(CallConv == CallingConv::C);

  if (Outs.size() == 0) {
    return true;
  }

  if (Outs.size() == 1) {
    return Outs[0].VT.getStoreSize() <= 2;
  }

  // TODO GB: support multiple 8-bit returns
  return false;
}

MachineBasicBlock *
GBTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                              MachineBasicBlock *MBB) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Instruction emitter not implemented!");
  case GB::P_SELECT_CC:
    return emitSelectCCWithCustomInserter(MI, MBB);
  case GB::P_SLA:
  case GB::P_SRA:
  case GB::P_SRL:
  case GB::P_ROTL:
  case GB::P_ROTR:
    return emitShiftWithCustomInserter(MI, MBB);
  }
}

MachineBasicBlock *
GBTargetLowering::emitSelectCCWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *MBB) const {
  const TargetInstrInfo &TII = *MBB->getParent()->getSubtarget().getInstrInfo();
  // Atm, we've got the following sequence
  // cmp xx
  // P_SELECT_CC flag ifTrue, ifFalse
  const auto MIResult = MI.getOperand(0).getReg();
  const auto MIFlag = MI.getOperand(1).getImm();
  const auto MIIfTrue = MI.getOperand(2).getReg();
  const auto MIIfFalse = MI.getOperand(3).getReg();

  // This should codegen to:
  //      cmp xx
  //      resultReg = ifTrue
  //      BR_CC flag .end
  //      resultReg = ifFalse
  //  .end:

  // In llvm world that is:
  // .start
  //      cmp xx
  //      BR_CC flag .end
  //      BR .false
  // .false
  //      BR .end
  // .end
  //      PHI ifTrue .start ifFalse .false
  MachineFunction *Func = MBB->getParent();
  const BasicBlock *BB = MBB->getBasicBlock();
  MachineFunction::iterator I = ++MBB->getIterator();
  DebugLoc DL = MI.getDebugLoc();

  MachineBasicBlock *StartMBB = MBB;
  MachineBasicBlock *FalseMBB = Func->CreateMachineBasicBlock(BB);
  MachineBasicBlock *EndMBB = Func->CreateMachineBasicBlock(BB);
  Func->insert(I, FalseMBB);
  Func->insert(I, EndMBB);

  // Split the basic block after the select
  EndMBB->splice(EndMBB->begin(), StartMBB,
                 std::next(MachineBasicBlock::iterator(MI)), StartMBB->end());
  EndMBB->transferSuccessorsAndUpdatePHIs(StartMBB);

  // Insert the new basic blocks
  StartMBB->addSuccessor(FalseMBB);
  StartMBB->addSuccessor(EndMBB);
  FalseMBB->addSuccessor(EndMBB);

  // Add the jumps
  BuildMI(StartMBB, DL, TII.get(GB::JR_COND)).addImm(MIFlag).addMBB(EndMBB);
  BuildMI(StartMBB, DL, TII.get(GB::JR)).addMBB(FalseMBB);
  BuildMI(FalseMBB, DL, TII.get(GB::JR)).addMBB(EndMBB);

  // Add the PHI
  // %result = phi [ %TrueValue, StartMBB ], [ %FalseValue, FalseMBB ]
  BuildMI(*EndMBB, EndMBB->begin(), DL, TII.get(GB::PHI), MIResult)
      .addReg(MIIfTrue)
      .addMBB(StartMBB)
      .addReg(MIIfFalse)
      .addMBB(FalseMBB);

  MI.eraseFromParent(); // The pseudo instruction is gone now.
  return EndMBB;
}

MachineBasicBlock *
GBTargetLowering::emitShiftWithCustomInserter(MachineInstr &MI,
                                              MachineBasicBlock *MBB) const {
  const TargetInstrInfo &TII = *MBB->getParent()->getSubtarget().getInstrInfo();
  MachineFunction *MF = MBB->getParent();
  MachineRegisterInfo &RI = MF->getRegInfo();

  DebugLoc DL = MI.getDebugLoc();
  const auto MIResult = MI.getOperand(0).getReg();
  const auto MISrc = MI.getOperand(1).getReg();
  const auto MIAmount = MI.getOperand(2).getReg();

  const auto OpCode = [&] {
    switch (MI.getOpcode()) {
    case GB::P_SLA:
      return GB::SLA_r;
    case GB::P_SRA:
      return GB::SRA_r;
    case GB::P_SRL:
      return GB::SRL_r;
    case GB::P_ROTL:
      return GB::RLC_r;
    case GB::P_ROTR:
      return GB::RRC_r;
    default:
      llvm_unreachable("Unexpected Opcode");
    }
  }();
  // This should codegen to:
  //  dec $amount
  //  inc $amount   (ahh, dec doesn't set carry)
  //  JR Z .end
  //  .main_loop:
  //    sla $rs
  //    dec $amount
  //    JR NZ .loop
  //  .end:

  // In llvm world that is:
  // .BB
  //    ...
  //    %amount1 = dec $shift_n
  //    %amount2 = inc $shift_n
  //    JR Z .end
  //    JR .LoopBB
  // .LoopBB
  //    %to_shift = PHI[%src, BB] [%shifted, LoopBB]
  //    %shifted = sla $to_shift
  //
  //    %to_decrement = PHI[%amount2, BB] [%decremented, LoopBB]
  //    %decremented = dec $to_decrement
  //
  //    JR NZ .LoopBB
  //    JR .end
  // .end
  //      PHI [%src BB] [shifted LoopBB
  const BasicBlock *BB = MBB->getBasicBlock();
  MachineFunction::iterator I = ++MBB->getIterator();

  // Create the basic blocks
  MachineBasicBlock *StartMBB = MBB;
  MachineBasicBlock *LoopMBB = MF->CreateMachineBasicBlock(BB);
  MachineBasicBlock *EndMBB = MF->CreateMachineBasicBlock(BB);

  MF->insert(I, LoopMBB);
  MF->insert(I, EndMBB);

  // Move everything after the shift into the new BB
  EndMBB->splice(EndMBB->begin(), StartMBB,
                 std::next(MachineBasicBlock::iterator(MI)), StartMBB->end());
  EndMBB->transferSuccessorsAndUpdatePHIs(StartMBB);

  StartMBB->addSuccessor(LoopMBB);
  StartMBB->addSuccessor(EndMBB);
  LoopMBB->addSuccessor(EndMBB);
  LoopMBB->addSuccessor(LoopMBB);

  // Populate the registers
  Register InitialDecReg = RI.createVirtualRegister(&GB::GPR8RegClass);
  Register StartAmount = RI.createVirtualRegister(&GB::GPR8RegClass);
  Register LoopDecReg = RI.createVirtualRegister(&GB::GPR8RegClass);
  Register ToDecReg = RI.createVirtualRegister(&GB::GPR8RegClass);

  Register SrcShiftReg = MISrc;
  Register LoopShiftReg = RI.createVirtualRegister(&GB::GPR8RegClass);
  Register ToShiftReg = RI.createVirtualRegister(&GB::GPR8RegClass);

  // StartBB
  BuildMI(StartMBB, DL, TII.get(GB::DEC_r), InitialDecReg).addReg(MIAmount);
  BuildMI(StartMBB, DL, TII.get(GB::INC_r), StartAmount).addReg(InitialDecReg);
  BuildMI(StartMBB, DL, TII.get(GB::JR_COND)).addImm(GBFlag::Z).addMBB(EndMBB);
  BuildMI(StartMBB, DL, TII.get(GB::JR)).addMBB(LoopMBB);

  // Loop BB
  BuildMI(LoopMBB, DL, TII.get(GB::PHI), ToShiftReg)
      .addReg(SrcShiftReg)
      .addMBB(StartMBB)
      .addReg(LoopShiftReg)
      .addMBB(LoopMBB);

  BuildMI(LoopMBB, DL, TII.get(GB::PHI), ToDecReg)
      .addReg(StartAmount)
      .addMBB(StartMBB)
      .addReg(LoopDecReg)
      .addMBB(LoopMBB);

  BuildMI(LoopMBB, DL, TII.get(OpCode), LoopShiftReg).addReg(ToShiftReg);
  BuildMI(LoopMBB, DL, TII.get(GB::DEC_r), LoopDecReg).addReg(ToDecReg);

  BuildMI(LoopMBB, DL, TII.get(GB::JR_COND)).addImm(GBFlag::NZ).addMBB(LoopMBB);
  BuildMI(LoopMBB, DL, TII.get(GB::JR)).addMBB(EndMBB);

  // End BB
  BuildMI(*EndMBB, EndMBB->begin(), DL, TII.get(GB::PHI), MIResult)
      .addReg(SrcShiftReg)
      .addMBB(StartMBB)
      .addReg(LoopShiftReg)
      .addMBB(LoopMBB);

  MI.eraseFromParent();
  return EndMBB;
}

SDValue
GBTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                              bool IsVarArg,
                              const SmallVectorImpl<ISD::OutputArg> &Outs,
                              const SmallVectorImpl<SDValue> &OutsVals,
                              const SDLoc &DL, SelectionDAG &DAG) const {
  if (CallConv != CallingConv::C) {
    report_fatal_error("Unsupported calling convention");
  }
  if (IsVarArg) {
    report_fatal_error("Variadic arguments are not supported");
  }

  MachineFunction &MF = DAG.getMachineFunction();

  // Use the autogenerated calling convention to assign a location to each
  // argument
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  CCInfo.AnalyzeReturn(Outs, RetCC_GB);

  SmallVector<SDValue, 4> RetOps(1, Chain);
  SDValue Glue;

  for (unsigned I = 0; I < RVLocs.size(); ++I) {
    CCValAssign &VA = RVLocs[I];
    assert(VA.isRegLoc());

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutsVals[I], Glue);
    Glue = Chain.getValue(1);

    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // Update the chain, add the glue
  RetOps[0] = Chain;
  if (Glue.getNode()) {
    RetOps.push_back(Glue);
  }

  return DAG.getNode(GBISD::RET, DL, MVT::Other, RetOps);
}

MVT GBTargetLowering::getScalarShiftAmountTy(const DataLayout &, EVT) const {
  return MVT::i8;
}

EVT GBTargetLowering::getSetCCResultType(const DataLayout &DL,
                                         LLVMContext &Context, EVT VT) const {
  return MVT::i8;
}

bool GBTargetLowering::convertSetCCLogicToBitwiseLogic(EVT VT) const {
  return true;
}

bool GBTargetLowering::isSelectSupported(SelectSupportKind) const {
  // We don't have a select instruction: inform the optimizer
  // This will expand selects into brcond... but only with optimizations on.
  return false;
}

bool GBTargetLowering::shouldConvertConstantLoadToIntImm(const APInt &Imm,
                                                         Type *Ty) const {
  return true;
}

bool GBTargetLowering::allowsMisalignedMemoryAccesses(EVT, unsigned AddrSpace,
                                                      Align,
                                                      MachineMemOperand::Flags,
                                                      unsigned *Fast) const {
  if (Fast != nullptr) {
    *Fast = 1;
  }
  return true;
}

void GBTargetLowering::splitValue(SelectionDAG &DAG, SDValue Value, SDValue &Lo,
                                  SDValue &Hi) const {
  assert(Value.getSimpleValueType() == MVT::i16);
  SDLoc DL = Value;
  Lo = DAG.getNode(GBISD::LOWER, DL, MVT::i8, Value);
  Hi = DAG.getNode(GBISD::UPPER, DL, MVT::i8, Value);
}

SDValue GBTargetLowering::mergeValues(SelectionDAG &DAG, SDValue Lo, SDValue Hi,
                                      bool &FreshNode) const {
  assert(Lo.getSimpleValueType() == MVT::i8);
  SDLoc DL = Lo;

  if (Lo.getOpcode() == GBISD::LOWER && Hi->getOpcode() == GBISD::UPPER) {
    if (Lo->getOperand(0) == Hi->getOperand(0)) {
      // Rather than split-then-merge just remove the split
      FreshNode = false;
      return Lo.getOperand(0);
    }
  }
  FreshNode = true;
  return DAG.getNode(GBISD::COMBINE, DL, MVT::i16, Lo, Hi);
}

EVT GBTargetLowering::getTypeToTransformTo(LLVMContext &Context, EVT VT) const {
  if (VT == MVT::i16) {
    return MVT::i8;
  }
  return getTypeConversion(Context, VT).second;
}

GBTargetLowering::LegalizeTypeAction
GBTargetLowering::getTypeActionForOperand(SDNode *N, unsigned Operand) const {
  EVT Cur = N->getOperand(Operand).getValueType();
  if (Cur != MVT::i16) {
    // 16 bit nodes require special legalization
    return getTypeAction(Cur.getSimpleVT());
  }

  if (N->getOpcode() == GBISD::UPPER || N->getOpcode() == GBISD::LOWER) {
    // We use UPPER/ LOWER to legalize arbitrary 16 bit values
    return LegalizeTypeAction::TypeLegal;
  }

  // TODO GB: natively support add
  switch (N->getOperand(Operand)->getOpcode()) {
  case GBISD::COMBINE: // We've already decided this must be 16 bit
    return LegalizeTypeAction::TypeLegal;
  default:
    return LegalizeTypeAction::TypeExpandInteger;
  }
}

GBTargetLowering::LegalizeTypeAction
GBTargetLowering::getTypeActionForResult(SDNode *N, unsigned Result) const {
  EVT Cur = SDValue{N, Result}.getValueType();
  if (Cur != MVT::i16) {
    // 16 bit nodes require special legalization
    return getTypeAction(Cur.getSimpleVT());
  }

  // TODO GB: natively support add
  switch (N->getOpcode()) {
  case GBISD::COMBINE: // We've already decided this must be 16 bit
    return LegalizeTypeAction::TypeLegal;
  default:
    return LegalizeTypeAction::TypeExpandInteger;
  }
}

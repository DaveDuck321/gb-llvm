#include "GBISelLowering.h"
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
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/ModuleSummaryIndex.h"
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
  // SUB
  setOperationAction(ISD::SUB, MVT::i8, Legal);
  setOperationAction(ISD::ADD, MVT::i8, Legal);
  setOperationAction(ISD::ADD, MVT::i16, Legal);
  // MUL, SDIV, UDIV, SREM, UREM
  // SMUL_LOHI, UMUL_LOHI
  // SDIVREM, UDIVREM
  // CARRY_FALSE
  // ADDC                // Expanded
  // SUBC                // Expanded
  // ADDE                // Expanded
  // SUBE                // Expanded
  // UADDO_CARRY         // Expanded
  // USUBO_CARRY         // Expanded
  // SADDO_CARRY         // Expanded
  // SSUBO_CARRY         // Expanded
  // SADDO               // Expanded
  // UADDO               // Expanded
  // SSUBO               // Expanded
  // USUBO               // Expanded
  // SMULO               // Expanded
  // UMULO               // Expanded
  // MULHU, MULHS
  for (const auto &BinaryOp : {ISD::AND, ISD::OR, ISD::XOR}) {
    setOperationAction(BinaryOp, MVT::i8, Legal);
    setOperationAction(BinaryOp, MVT::i16, Custom);
  }
  // SHL, SRA, SRL
  // ROTL, ROTR
  // BSWAP
  // CTTZ, CTLZ
  // CTPOP
  // SETCC, SELECT, SELECT_CC
  setOperationAction(ISD::SETCC, MVT::i8, Custom);
  setOperationAction(ISD::SELECT, MVT::i8, Expand); // -> select_cc
  setOperationAction(ISD::SELECT_CC, MVT::i8, Custom);
  // SETCCCARRY         // Expanded
  // SHL_PARTS, SRA_PARTS, SRL_PARTS
  // SIGN_EXTEND, ZERO_EXTEND, ANY_EXTEND
  setOperationAction(ISD::SIGN_EXTEND, MVT::i16, Custom);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i16, Custom);
  setOperationAction(ISD::ANY_EXTEND, MVT::i16, Custom);
  setOperationAction(ISD::TRUNCATE, MVT::i8, Legal); // i16 to i8
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Custom);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8, Expand); // -> SIGN_EXTEND
  // BITCAST
  // ADDRSPACECAST
  for (const auto &MemoryOp : {ISD::LOAD, ISD::STORE}) {
    setOperationAction(MemoryOp, MVT::i8, Legal);
    setOperationAction(MemoryOp, MVT::i16, Custom);
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
    Op->print(dbgs(), &DAG);
    report_fatal_error("GBTargetLowering::LowerOperation unimplemented!!");
  case ISD::LOAD:
    return LowerLOAD16(Op, DAG);
  case ISD::STORE:
    return LowerSTORE16(Op, DAG);
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
  case ISD::OR:
  case ISD::AND:
  case ISD::XOR:
    return LowerBinaryOp(Op, DAG);
  case ISD::SIGN_EXTEND:
  case ISD::ZERO_EXTEND:
  case ISD::ANY_EXTEND:
    return LowerXExtend(Op, DAG);
  case ISD::SIGN_EXTEND_INREG:
    return LowerSignExtendInReg(Op, DAG);
  }
}

SDValue GBTargetLowering::LowerLOAD16(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *Node = dyn_cast<LoadSDNode>(Op);
  assert(Op.getSimpleValueType() == MVT::i16);
  assert(Node != nullptr);

  SDValue Chain = Node->getChain();
  SDValue BasePtr = Node->getBasePtr();
  SDValue Offset = Node->getOffset();
  SDLoc DL = Op;

  SDValue PtrLower;
  if (Node->isIndexed()) {
    PtrLower = DAG.getNode(ISD::ADD, DL, MVT::i16, BasePtr, Offset);
  } else {
    assert(Offset->isUndef());
    PtrLower = BasePtr;
  }

  SDValue PtrUpper = DAG.getNode(ISD::ADD, DL, MVT::i16, PtrLower,
                                 DAG.getConstant(1, DL, MVT::i16));
  SDValue Lower =
      DAG.getLoad(MVT::i8, DL, Chain, PtrLower, Node->getMemOperand());
  Chain = Lower.getValue(1);

  SDValue Upper =
      DAG.getLoad(MVT::i8, DL, Chain, PtrUpper, Node->getMemOperand());
  Chain = Upper.getValue(1);

  SDVTList VTs = DAG.getVTList(MVT::i16, MVT::Other);
  return DAG.getNode(GBISD::COMBINE_CHAIN, DL, VTs, Chain, Lower, Upper);
}

SDValue GBTargetLowering::LowerSTORE16(SDValue Op, SelectionDAG &DAG) const {
  StoreSDNode *Node = dyn_cast<StoreSDNode>(Op);
  assert(Node != nullptr);
  assert(Node->getValue().getSimpleValueType() == MVT::i16);

  SDValue Chain = Node->getChain();
  SDValue BasePtr = Node->getBasePtr();
  SDValue Offset = Node->getOffset();
  SDValue Value = Node->getValue();
  SDLoc DL = Op;

  SDValue PtrLower;
  if (Node->isIndexed()) {
    PtrLower = DAG.getNode(ISD::ADD, DL, MVT::i16, BasePtr, Offset);
  } else {
    assert(Offset->isUndef());
    PtrLower = BasePtr;
  }

  SDValue PtrUpper = DAG.getNode(ISD::ADD, DL, MVT::i16, PtrLower,
                                 DAG.getConstant(1, DL, MVT::i16));

  SDValue Upper = DAG.getNode(GBISD::UPPER, DL, MVT::i8, Value);
  SDValue Lower = DAG.getNode(GBISD::LOWER, DL, MVT::i8, Value);

  Chain = DAG.getStore(Chain, DL, Lower, PtrLower, Node->getMemOperand());
  return DAG.getStore(Chain, DL, Upper, PtrUpper, Node->getMemOperand());
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
  // sub a, b
  // dec
  // rla
  case ISD::CondCode::SETNE:
    ReverseResult = true;
    [[fallthrough]];
  case ISD::CondCode::SETEQ: {
    SDValue Sub = DAG.getNode(ISD::SUB, DL, LHS.getValueType(), LHS, RHS);
    LHS = Sub;
    RHS = DAG.getConstant(0x01, DL, MVT::i8);
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

  // Operand is ignored
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

SDValue GBTargetLowering::LowerBinaryOp(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDLoc DL = Op;

  SDValue LHSLower = DAG.getNode(GBISD::LOWER, DL, MVT::i8, LHS);
  SDValue RHSLower = DAG.getNode(GBISD::LOWER, DL, MVT::i8, RHS);
  SDValue ResultLower =
      DAG.getNode(Op->getOpcode(), DL, MVT::i8, LHSLower, RHSLower);

  SDValue LHSUpper = DAG.getNode(GBISD::UPPER, DL, MVT::i8, LHS);
  SDValue RHSUpper = DAG.getNode(GBISD::UPPER, DL, MVT::i8, RHS);
  SDValue ResultUpper =
      DAG.getNode(Op->getOpcode(), DL, MVT::i8, LHSUpper, RHSUpper);

  return DAG.getNode(GBISD::COMBINE, DL, MVT::i16, ResultLower, ResultUpper);
}

SDValue GBTargetLowering::LowerXExtend(SDValue Op, SelectionDAG &DAG) const {
  unsigned Opcode = Op.getOpcode();
  SDValue Src = Op.getOperand(0);
  EVT DestType = Op->getValueType(0);
  SDLoc DL = Op;

  assert(Src.getSimpleValueType() == MVT::i8 && DestType == MVT::i16);

  // Zero/ undef extensions are easy, just add correct upper subreg
  if (Opcode == ISD::ZERO_EXTEND || Opcode == ISD::ANY_EXTEND) {
    SDValue Lower = Src;
    SDValue Upper = Opcode == ISD::ZERO_EXTEND ? DAG.getConstant(0, DL, MVT::i8)
                                               : DAG.getUNDEF(MVT::i8);

    return DAG.getNode(GBISD::COMBINE, DL, MVT::i16, Lower, Upper);
  }

  // Sign extensions require a bit of maths
  assert(Opcode == ISD::SIGN_EXTEND);

  SDValue SignBiti1 = DAG.getNode(GBISD::RLCA, DL, MVT::i8, Src);
  SDValue SignBiti8 = DAG.getNode(ISD::AND, DL, MVT::i8, SignBiti1,
                                  DAG.getConstant(1, DL, MVT::i8));

  SDValue Lower = Src;
  SDValue Upper = DAG.getNode(ISD::SUB, DL, MVT::i8,
                              DAG.getConstant(0, DL, MVT::i8), SignBiti8);

  return DAG.getNode(GBISD::COMBINE, DL, MVT::i16, Lower, Upper);
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
  SDValue Result =
      DAG.getNode(ISD::SUB, DL, MVT::i8, DAG.getConstant(0, DL, MVT::i8), And);

  if (Op.getValueType() == MVT::i8) {
    return Result;
  }

  assert(Op.getValueType() == MVT::i16);
  return DAG.getNode(GBISD::COMBINE, DL, MVT::i16, Result, Result);
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
  case GBISD::COMBINE_CHAIN:
    return "GBISD::COMBINE_CHAIN";
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
      int Offset = VA.getLocMemOffset() + 1;
      assert(isInt<8>(Offset));

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

  for (auto &VA : ArgLocs) {
    EVT LocVT = VA.getLocVT();

    SDValue ArgIn;
    if (VA.isRegLoc()) {
      MVT ValVT = VA.getValVT();
      MachineRegisterInfo &RegInfo = MF.getRegInfo();
      const auto *RC = getRegClassFor(ValVT);
      Register VReg = RegInfo.createVirtualRegister(RC);
      RegInfo.addLiveIn(VA.getLocReg(), VReg);
      ArgIn = DAG.getCopyFromReg(Chain, DL, VReg, LocVT);
    } else {
      // Stack
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

  size_t TotalSize = 0;
  for (const auto &Arg : Outs) {
    TotalSize += Arg.VT.getStoreSize();
  }
  return TotalSize <= 2;
}

MachineBasicBlock *
GBTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                              MachineBasicBlock *MBB) const {
  switch (MI.getOpcode()) {
  default:
    assert(!"Instruction emitter implemented!");
  case GB::P_SELECT_CC:
    return emitSelectCCWithCustomInserter(MI, MBB);
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

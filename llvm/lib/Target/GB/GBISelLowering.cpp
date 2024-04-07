#include "GBISelLowering.h"
#include "GBRegisterInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineValueType.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
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
  // CONDCODE
  // RegisterMask
  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);
  // GlobalTLSAddress
  // FrameIndex
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
  // ADD, SUB
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
  // AND, OR, XOR
  // SHL, SRA, SRL
  // ROTL, ROTR
  // BSWAP
  // CTTZ, CTLZ
  // CTPOP
  // SELECT, SELECT_CC
  setOperationAction(ISD::SETCC, MVT::i8, Custom);
  // SETCCCARRY         // Expanded
  // SHL_PARTS, SRA_PARTS, SRL_PARTS
  // SIGN_EXTEND, ZERO_EXTEND, ANY_EXTEND
  // TRUNCATE
  // SIGN_EXTEND_INREG
  // BITCAST
  // ADDRSPACECAST
  setOperationAction(ISD::LOAD, MVT::i8, Legal);
  setOperationAction(ISD::LOAD, MVT::i16, Custom);
  setOperationAction(ISD::STORE, MVT::i8, Legal);
  setOperationAction(ISD::STORE, MVT::i16, Custom);
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
  // TRAP
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
  case ISD::SELECT:
  case ISD::SELECT_CC:
    llvm_unreachable("DAG tried to lower ISD::SELECT_(CC)");
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:
    return LowerBlockAddress(Op, DAG);
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
  return DAG.getNode(GBISD::COMBINE, DL, VTs, Chain, Lower, Upper);
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

SDValue GBTargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CCode =
      dyn_cast<CondCodeSDNode>(Op.getOperand(1).getNode())->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue BB = Op.getOperand(4);
  SDLoc DL = Op;

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
  SDValue Cmp = DAG.getNode(GBISD::CP, DL, MVT::Glue, CC, LHS, RHS);
  return DAG.getNode(GBISD::BR_CC, DL, MVT::Other, Chain, CC, BB, Cmp);
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
    Result = DAG.getNode(ISD::XOR, DL, MVT::i8, Result,
                         DAG.getConstant(-1, DL, MVT::i8));
    return Result;
  }

  SDValue CC = DAG.getCondCode(CCode);
  SDValue Cmp = DAG.getNode(GBISD::CP, DL, MVT::Glue, CC, LHS, RHS);

  // Operand is ignored
  SDValue Result =
      DAG.getNode(GBISD::RLA, DL, MVT::i8, DAG.getUNDEF(MVT::i8), Cmp);
  if (ReverseResult) {
    Result = DAG.getNode(ISD::XOR, DL, MVT::i8, Result,
                         DAG.getConstant(-1, DL, MVT::i8));
  }
  return Result;
}

SDValue GBTargetLowering::LowerGlobalAddress(SDValue Op,
                                             SelectionDAG &DAG) const {
  GlobalAddressSDNode *Node = dyn_cast<GlobalAddressSDNode>(Op);
  SDLoc DL = Op;

  SDValue TargetAddr =
      DAG.getTargetGlobalAddress(Node->getGlobal(), DL, MVT::i16);
  return DAG.getNode(GBISD::ADDR_WRAPPER, DL, MVT::i16, TargetAddr);
}

SDValue GBTargetLowering::LowerBlockAddress(SDValue Op,
                                            SelectionDAG &DAG) const {
  BlockAddressSDNode *Node = dyn_cast<BlockAddressSDNode>(Op);
  SDLoc DL = Op;

  SDValue TargetAddr = DAG.getTargetBlockAddress(Node->getBlockAddress(),
                                                 MVT::i16, Node->getOffset());
  return DAG.getNode(GBISD::ADDR_WRAPPER, DL, MVT::i16, TargetAddr);
}

const char *GBTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((GBISD::NodeType)Opcode) {
  case GBISD::FIRST_NUMBER:
    break;
  case GBISD::BR_CC:
    return "GBISD::BR_CC";
  case GBISD::COMBINE:
    return "GBISD::COMBINE";
  case GBISD::LOWER:
    return "GBISD::LOWER";
  case GBISD::UPPER:
    return "GBISD::UPPER";
  case GBISD::CP:
    return "GBISD::CP";
  case GBISD::RET:
    return "GBISD::RET";
  case GBISD::RLA:
    return "GBISD::RLA";
  case GBISD::RLCA:
    return "GBISD::RLCA";
  case GBISD::ADDR_WRAPPER:
    return "GBISD::ADDR_WRAPPER";
  }
  return nullptr;
}

#include "GBGenCallingConv.inc"

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

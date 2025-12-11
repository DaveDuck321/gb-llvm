#include "GBLegalizerInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/GlobalISel/LegalizerHelper.h"
#include "llvm/CodeGen/GlobalISel/LegalizerInfo.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGenTypes/LowLevelType.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>
#include <cstdint>

using namespace llvm;
using namespace TargetOpcode;

GBLegalizerInfo::GBLegalizerInfo(const GBSubtarget &) {

  const LLT P0 = LLT::pointer(0, 16);

  const LLT S8 = LLT::scalar(8);
  const LLT S16 = LLT::scalar(16);

  getActionDefinitionsBuilder(
      {G_CONSTANT, G_IMPLICIT_DEF, G_FREEZE, G_CONSTANT_FOLD_BARRIER})
      .legalFor({S8, S16, P0})
      .clampScalar(0, S8, S16);

  getActionDefinitionsBuilder({G_AND, G_OR, G_XOR})
      .legalFor({S8})
      .clampScalar(0, S8, S8);

  getActionDefinitionsBuilder({G_ICMP, G_SELECT})
      .clampScalar(0, S8, S8)
      .clampScalar(1, S8, S8)
      .custom();

  // TODO: experiment with S8 only
  getActionDefinitionsBuilder(G_PHI).legalFor({P0, S8, S16});

  // Avoid lowering to G_(U|S)ADD(O|E) and instead lower directly into the
  // native instructions.
  getActionDefinitionsBuilder({G_ADD, G_SUB}).legalFor({S8}).custom();
  getActionDefinitionsBuilder({G_ZEXT, G_SEXT, G_ANYEXT})
      .clampScalar(0, S8, S8)
      .clampScalar(1, S8, S8);

  getActionDefinitionsBuilder(G_SEXT_INREG)
      .customFor({S8})
      .clampScalar(0, S8, S8);

  // TODO: wider 16 bit shifts by a CONSTANT amount should also be custom
  getActionDefinitionsBuilder({G_SHL, G_LSHR, G_ASHR, G_ROTR, G_ROTL})
      .customFor({S8})
      .libcall();

  getActionDefinitionsBuilder({G_SBFX, G_UBFX}).lower();
  getActionDefinitionsBuilder(G_BSWAP).legalFor({S8}).custom();
  getActionDefinitionsBuilder(
      {G_CTPOP, G_CTTZ, G_CTLZ, G_CTTZ_ZERO_UNDEF, G_CTLZ_ZERO_UNDEF})
      .clampScalar(0, S8, S8)
      .clampScalar(1, S8, S8)
      .lower();

  getActionDefinitionsBuilder({G_STORE, G_LOAD})
      .lowerIfMemSizeNotByteSizePow2()
      .legalFor({S8})
      .clampScalar(0, S8, S8)
      .customFor({P0});

  getActionDefinitionsBuilder({G_SEXTLOAD, G_ZEXTLOAD}).lower();

  getActionDefinitionsBuilder(
      {G_MUL, G_SDIV, G_UDIV, G_SREM, G_UREM, G_SDIVREM, G_UDIVREM})
      .libcall();

  getActionDefinitionsBuilder(G_MERGE_VALUES).legalFor({{S16, S8}});
  getActionDefinitionsBuilder(G_UNMERGE_VALUES).customFor({{S8, S16}});
  getActionDefinitionsBuilder(G_EXTRACT).legalFor({{S8, S16}});
  getActionDefinitionsBuilder(G_TRUNC).customFor({{S8, S16}}).lower();

  getActionDefinitionsBuilder({G_INTTOPTR, G_PTRTOINT}).alwaysLegal();

  getActionDefinitionsBuilder(G_PTR_ADD).lower();
  getActionDefinitionsBuilder({G_SCMP, G_UCMP}).lower();

  getActionDefinitionsBuilder(G_BRCOND).legalFor({S8}).clampScalar(0, S8, S8);
  getActionDefinitionsBuilder(G_BRINDIRECT).legalFor({P0});

  getActionDefinitionsBuilder({G_FRAME_INDEX, G_GLOBAL_VALUE, G_BLOCK_ADDR})
      .legalFor({P0});

  getActionDefinitionsBuilder({G_MEMCPY, G_MEMMOVE, G_MEMSET}).libcall();

  getActionDefinitionsBuilder(
      {G_ATOMICRMW_XCHG, G_ATOMIC_CMPXCHG, G_ATOMICRMW_ADD, G_ATOMICRMW_SUB,
       G_ATOMICRMW_AND, G_ATOMICRMW_NAND, G_ATOMICRMW_OR, G_ATOMICRMW_XOR,
       G_ATOMICRMW_MAX, G_ATOMICRMW_MIN, G_ATOMICRMW_UMAX, G_ATOMICRMW_UMIN})
      .libcallFor({S8})
      .unsupported();

  getActionDefinitionsBuilder(G_FENCE).alwaysLegal();
  getActionDefinitionsBuilder(G_ATOMIC_CMPXCHG_WITH_SUCCESS).lower();

  getActionDefinitionsBuilder({G_ATOMICRMW_UINC_WRAP, G_ATOMICRMW_UDEC_WRAP,
                               G_ATOMICRMW_USUB_COND, G_ATOMICRMW_USUB_SAT})
      .unsupported();

  getActionDefinitionsBuilder({G_ATOMICRMW_FADD, G_ATOMICRMW_FSUB,
                               G_ATOMICRMW_FMAX, G_ATOMICRMW_FMIN,
                               G_ATOMICRMW_FMAXIMUM, G_ATOMICRMW_FMINIMUM})
      .unsupported();

  // G_ASSERT_ZEXT
  // G_ASSERT_ALIGN
  // G_ABDS
  // G_ABDU
  // G_PTRAUTH_GLOBAL_VALUE
  // G_CONSTANT_POOL
  // G_INSERT
  // G_BITCAST
  // G_INDEXED_LOAD
  // G_INDEXED_SEXTLOAD
  // G_INDEXED_ZEXTLOAD
  // G_INDEXED_STORE
  // G_PREFETCH
  // G_INVOKE_REGION_START
  // G_FCONSTANT
  // G_VASTART
  // G_VAARG
  // G_FSHL
  // G_FSHR
  // G_FCMP

  // G_UMULO
  // G_SMULO
  // G_UMULH
  // G_SMULH
  // G_UADDSAT
  // G_SADDSAT
  // G_USUBSAT
  // G_SSUBSAT
  // G_USHLSAT
  // G_SSHLSAT
  // G_SMULFIX
  // G_UMULFIX
  // G_SMULFIXSAT
  // G_UMULFIXSAT
  // G_SDIVFIX
  // G_UDIVFIX
  // G_SDIVFIXSAT
  // G_UDIVFIXSAT

  // G_FADD
  // G_FSUB
  // G_FMUL
  // G_FMA
  // G_FMAD
  // G_FDIV
  // G_FREM
  // G_FPOW
  // G_FPOWI
  // G_FEXP
  // G_FEXP2
  // G_FEXP10
  // G_FLOG
  // G_FLOG2
  // G_FLOG10
  // G_FLDEXP
  // G_FFREXP
  // G_FNEG
  // G_FPEXT
  // G_FPTRUNC
  // G_FPTOSI
  // G_FPTOUI
  // G_SITOFP
  // G_UITOFP
  // G_FPTOSI_SAT
  // G_FPTOUI_SAT
  // G_FABS
  // G_FCOPYSIGN
  // G_IS_FPCLASS
  // G_FCANONICALIZE
  // G_FMINNUM
  // G_FMAXNUM
  // G_FMINNUM_IEEE
  // G_FMAXNUM_IEEE
  // G_FMINIMUM
  // G_FMAXIMUM
  // G_FMINIMUMNUM
  // G_FMAXIMUMNUM
  // G_GET_FPENV
  // G_SET_FPENV
  // G_RESET_FPENV
  // G_GET_FPMODE
  // G_SET_FPMODE
  // G_RESET_FPMODE
  // G_PTR_ADD
  // G_PTRMASK
  // G_SMIN
  // G_SMAX
  // G_UMIN
  // G_UMAX
  // G_ABS
  // G_LROUND
  // G_LLROUND
  // G_BR
  // G_BRJT
  // G_BITREVERSE
  // G_FCEIL
  // G_FCOS
  // G_FSIN
  // G_FSINCOS
  // G_FTAN
  // G_FACOS
  // G_FASIN
  // G_FATAN
  // G_FATAN2
  // G_FCOSH
  // G_FSINH
  // G_FTANH
  // G_FSQRT
  // G_FFLOOR
  // G_FRINT
  // G_FNEARBYINT
  // G_ADDRSPACE_CAST
  // G_JUMP_TABLE
  // G_DYN_STACKALLOC
  // G_STACKSAVE
  // G_STACKRESTORE
  // G_STRICT_FADD
  // G_STRICT_FSUB
  // G_STRICT_FMUL
  // G_STRICT_FDIV
  // G_STRICT_FREM
  // G_STRICT_FMA
  // G_STRICT_FSQRT
  // G_STRICT_FLDEXP
  // G_READ_REGISTER
  // G_WRITE_REGISTER
  // G_MEMCPY_INLINE
  // G_BZERO
  // G_TRAP
  // G_DEBUGTRAP
  // G_UBSANTRAP
}

bool GBLegalizerInfo::legalizeCustom(LegalizerHelper &Helper, MachineInstr &MI,
                                     LostDebugLocObserver &LocObserver) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unimplemented custom legalization");
  case TargetOpcode::G_UNMERGE_VALUES:
    return legalizeUnmerge(Helper, MI, LocObserver);
  case TargetOpcode::G_TRUNC:
    return legalizeTrunc(Helper, MI, LocObserver);
  case TargetOpcode::G_ADD:
  case TargetOpcode::G_SUB:
    return legalizeLargeAddSub(Helper, MI, LocObserver);
  case TargetOpcode::G_ICMP:
    return legalizeICmp(Helper, MI, LocObserver);
  case TargetOpcode::G_SELECT:
    return legalizeSelect(Helper, MI, LocObserver);
  case TargetOpcode::G_LOAD:
  case TargetOpcode::G_STORE:
    return legalizeLoadStore(Helper, MI, LocObserver);
  case TargetOpcode::G_SEXT_INREG:
    return legalizeSExt(Helper, MI, LocObserver);
  case TargetOpcode::G_SHL:
  case TargetOpcode::G_LSHR:
  case TargetOpcode::G_ASHR:
  case TargetOpcode::G_ROTR:
  case TargetOpcode::G_ROTL:
    return legalizeShiftRotate(Helper, MI, LocObserver);
  case TargetOpcode::G_BSWAP:
    return legalizeByteSwap(Helper, MI, LocObserver);
  }
}

bool GBLegalizerInfo::legalizeLoadStore(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  // Loading or storing a pointer is not supported and breaks the narrowing.
  // Convert to an int type so the legalizer has something it can split.
  MachineIRBuilder MIB(MI);

  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto P0 = LLT::pointer(0, 16);
  auto S16 = LLT::scalar(16);

  auto PtrReg = MI.getOperand(0).getReg();
  auto AddrReg = MI.getOperand(1).getReg();
  assert(MRI.getType(PtrReg) == P0);
  assert(MRI.getType(AddrReg) == P0);

  if (MI.getOpcode() == TargetOpcode::G_LOAD) {
    // Result goes in PtrReg
    auto ToLoad = MRI.createGenericVirtualRegister(S16);
    MI.getOperand(0).ChangeToRegister(ToLoad, /*isDef=*/true);
    MIB.buildIntToPtr(PtrReg, ToLoad);
    Helper.Observer.changedInstr(MI);
  } else {
    // We're storing PtrReg
    assert(MI.getOpcode() == TargetOpcode::G_STORE);
    auto ToStore = MRI.createGenericVirtualRegister(S16);
    MIB.buildPtrToInt(ToStore, PtrReg);
    MI.getOperand(0).ChangeToRegister(ToStore, /*isDef=*/false);
    Helper.Observer.changedInstr(MI);
  }
  return true;
}

bool GBLegalizerInfo::legalizeUnmerge(LegalizerHelper &Helper, MachineInstr &MI,
                                      LostDebugLocObserver &LocObserver) const {
  assert(MI.getOpcode() == TargetOpcode::G_UNMERGE_VALUES);

  MachineIRBuilder MIB(MI);

  auto LowerOut = MI.getOperand(0);
  auto UpperOut = MI.getOperand(1);
  auto RegIn = MI.getOperand(2);

  MIB.buildExtract(LowerOut, RegIn, 0);
  MIB.buildExtract(UpperOut, RegIn, 8);

  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeTrunc(LegalizerHelper &Helper, MachineInstr &MI,
                                    LostDebugLocObserver &LocObserver) const {
  assert(MI.getOpcode() == TargetOpcode::G_TRUNC);

  MachineIRBuilder MIB(MI);

  auto LowerOut = MI.getOperand(0);
  auto RegIn = MI.getOperand(1);

  MIB.buildExtract(LowerOut, RegIn, 0);

  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeSExt(LegalizerHelper &Helper, MachineInstr &MI,
                                   LostDebugLocObserver &LocObserver) const {
  assert(MI.getOpcode() == TargetOpcode::G_SEXT_INREG);
  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto S8 = LLT::scalar(8);

  DstOp DstReg = MI.getOperand(0);
  SrcOp SrcReg = MI.getOperand(1);
  auto SrcSize = MI.getOperand(2).getImm();
  assert(SrcSize == 1);
  assert(DstReg.getLLTTy(MRI) == S8);
  assert(SrcReg.getLLTTy(MRI) == S8);

  // LLVM would lower to shl 7 here (14 cycles)
  // We lower to: sext_result = ((in & 1) - 1) ^ 0xff
  // Which is only 4 cycles... And only uses the A reg.
  auto MaskedSrc = MRI.createGenericVirtualRegister(S8);
  auto OneConstant = MRI.createGenericVirtualRegister(S8);
  auto AllOnesConstant = MRI.createGenericVirtualRegister(S8);
  auto InvertedRes = MRI.createGenericVirtualRegister(S8);
  MachineIRBuilder MIB(MI);
  MIB.buildConstant(OneConstant, 1);
  MIB.buildConstant(AllOnesConstant, 0xFF);
  MIB.buildAnd(MaskedSrc, SrcReg, OneConstant);
  MIB.buildSub(InvertedRes, MaskedSrc, OneConstant);
  MIB.buildXor(DstReg, InvertedRes, AllOnesConstant);
  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeSelect(LegalizerHelper &Helper, MachineInstr &MI,
                                     LostDebugLocObserver &LocObserver) const {
  MachineFunction &MF = *MI.getMF();
  MachineBasicBlock &MBB = *MI.getParent();

  MachineIRBuilder MIB(MI);

  auto Result = MI.getOperand(0).getReg();
  auto Cond = MI.getOperand(1).getReg();
  auto ValIfTrue = MI.getOperand(2).getReg();
  auto ValIfFalse = MI.getOperand(3).getReg();

  auto *BeforeSelect = &MBB;
  auto *TrueBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *AfterSelect = MBB.splitAt(MI, /*UpdateLiveIns=*/false);

  MF.insert(++BeforeSelect->getIterator(), TrueBB);
  BeforeSelect->addSuccessor(TrueBB);
  TrueBB->addSuccessor(AfterSelect);

  // Setup the compare
  MIB.setInsertPt(*BeforeSelect, BeforeSelect->end());
  MIB.buildBrCond(Cond, *TrueBB);
  MIB.buildBr(*AfterSelect);

  // Setup the (trival) true branch
  MIB.setInsertPt(*TrueBB, TrueBB->begin());
  MIB.buildBr(*AfterSelect);

  // Setup the phi
  MIB.setInsertPt(*AfterSelect, AfterSelect->begin());
  MIB.buildInstr(TargetOpcode::G_PHI)
      .addDef(Result)
      .addUse(ValIfTrue)
      .addMBB(TrueBB)
      .addUse(ValIfFalse)
      .addMBB(BeforeSelect);

  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeICmp(LegalizerHelper &Helper, MachineInstr &MI,
                                   LostDebugLocObserver &LocObserver) const {
  MachineFunction &MF = *MI.getMF();
  MachineBasicBlock &MBB = *MI.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  MachineIRBuilder MIB(MI);

  auto S8 = LLT::scalar(8);

  auto Result = MI.getOperand(0).getReg();
  auto LHS = MI.getOperand(2).getReg();
  auto RHS = MI.getOperand(3).getReg();

  auto Predicate = ICmpInst::Predicate(MI.getOperand(1).getPredicate());
  switch (Predicate) {
  default:
    llvm_unreachable("Unrecognized predicate");
    // Instruction selection is efficient. No need to transform
  case CmpInst::ICMP_UGT:
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_UGE:
  case CmpInst::ICMP_ULE:
    return true;

    // Instruction selection produces intermediate results. Transform early to
    // benefit from the various combiner passes.
  case CmpInst::ICMP_NE: {
    auto SubRes = MRI.createGenericVirtualRegister(S8);
    auto ConstZero = MRI.createGenericVirtualRegister(S8);
    MIB.buildSub(SubRes, LHS, RHS);
    MIB.buildConstant(ConstZero, 0);
    MIB.buildICmp(ICmpInst::ICMP_UGT, Result, SubRes, ConstZero);
    MI.eraseFromParent();
    return true;
  }
  case CmpInst::ICMP_EQ: {
    auto SubRes = MRI.createGenericVirtualRegister(S8);
    auto ConstOne = MRI.createGenericVirtualRegister(S8);
    MIB.buildSub(SubRes, LHS, RHS);
    MIB.buildConstant(ConstOne, 1);
    MIB.buildICmp(ICmpInst::ICMP_ULT, Result, SubRes, ConstOne);
    MI.eraseFromParent();
    return true;
  }

  // Instruction selection is hideously inefficient. Transform into a branch.
  case CmpInst::ICMP_SGE:
  case CmpInst::ICMP_SLE:
  case CmpInst::ICMP_SGT:
  case CmpInst::ICMP_SLT:
    break;
  }

  auto *BeforeCmp = &MBB;
  auto *TrueBB = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *AfterCmp = MBB.splitAt(MI, /*UpdateLiveIns=*/false);

  MF.insert(++BeforeCmp->getIterator(), TrueBB);
  BeforeCmp->addSuccessor(TrueBB);
  TrueBB->addSuccessor(AfterCmp);

  // Setup the compare (via G_JP_CP)
  MIB.setInsertPt(*BeforeCmp, BeforeCmp->end());
  auto False = MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 0);
  MIB.buildInstr(GB::G_JP_CP)
      .addPredicate(Predicate)
      .addUse(LHS)
      .addUse(RHS)
      .addMBB(TrueBB);
  MIB.buildBr(*AfterCmp);

  // Setup the (trival) true branch
  MIB.setInsertPt(*TrueBB, TrueBB->begin());
  auto True = MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 1);
  MIB.buildBr(*AfterCmp);

  // Setup the phi
  MIB.setInsertPt(*AfterCmp, AfterCmp->begin());
  MIB.buildInstr(TargetOpcode::G_PHI)
      .addDef(Result)
      .addUse(True.getReg(0))
      .addMBB(TrueBB)
      .addUse(False.getReg(0))
      .addMBB(BeforeCmp);

  MI.eraseFromParent();
  return true;
}

static unsigned getShiftOpcode(unsigned GOpcode) {
  switch (GOpcode) {
  default:
    llvm_unreachable("Could not get opcode for shift");
  case TargetOpcode::G_SHL:
    return GB::SLA_r;
  case TargetOpcode::G_ASHR:
    return GB::SRA_r;
  case TargetOpcode::G_LSHR:
    return GB::SRL_r;
  case TargetOpcode::G_ROTR:
    return GB::RLC_r;
  case TargetOpcode::G_ROTL:
    return GB::RRC_r;
  }
}

bool GBLegalizerInfo::legalizeConstantShiftRotate(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver, uint8_t Amount) const {
  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto S8 = LLT::scalar(8);
  auto Result = MI.getOperand(0).getReg();
  auto Val = MI.getOperand(1).getReg();
  assert(MRI.getType(Val) == S8);

  MachineIRBuilder MIB(MI);

  auto LastShiftResult = Val;
  MRI.setRegClass(LastShiftResult, &GB::GPR8RegClass);

  // TODO: combine rotates, shifts and masks to produce better code
  for (unsigned I = 0; I < Amount; I += 1) {
    auto ThisShiftResult = MRI.createGenericVirtualRegister(S8);
    MRI.setRegClass(ThisShiftResult, &GB::GPR8RegClass);
    MIB.buildInstr(getShiftOpcode(MI.getOpcode()), {ThisShiftResult},
                   {LastShiftResult});
    LastShiftResult = ThisShiftResult;
  }
  MIB.buildCopy(Result, LastShiftResult);

  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeByteSwap(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  MachineIRBuilder MIB(MI);

  auto Result = MI.getOperand(0).getReg();
  auto Src = MI.getOperand(1).getReg();

  auto UnmergedSrc = MIB.buildUnmerge(LLT::scalar(8), Src);
  llvm::SmallVector<Register, 4> SwappedSrcParts;
  for (auto MO : UnmergedSrc->defs()) {
    SwappedSrcParts.push_back(MO.getReg());
  }
  std::reverse(SwappedSrcParts.begin(), SwappedSrcParts.end());

  MIB.buildMergeValues(Result, {SwappedSrcParts});
  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeShiftRotate(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  MachineFunction &MF = *MI.getMF();
  MachineBasicBlock &MBB = *MI.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto S8 = LLT::scalar(8);
  auto Result = MI.getOperand(0).getReg();
  auto InitVal = MI.getOperand(1).getReg();
  auto InitAmount = MI.getOperand(2).getReg();
  assert(MRI.getType(InitVal) == S8);

  MachineIRBuilder MIB(MI);

  if (auto Constant = getIConstantVRegValWithLookThrough(InitAmount, MRI)) {
    return legalizeConstantShiftRotate(Helper, MI, LocObserver,
                                       Constant->Value.getZExtValue());
  }

  // We don't know the shift amount, generate a loop
  // Normalize Amount into an S8
  if (MRI.getType(InitAmount) != S8) {
    auto Tmp = MRI.createGenericVirtualRegister(S8);
    MIB.buildTrunc(Tmp, InitAmount);
    InitAmount = Tmp;
  }

  auto *BeforeLoop = &MBB;
  auto *LoopCondition = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *LoopBody = MF.CreateMachineBasicBlock(MBB.getBasicBlock());
  auto *AfterLoop = MBB.splitAt(*MIB.getInsertPt(), /*UpdateLiveIns=*/false);

  MachineFunction::iterator I = ++MBB.getIterator();
  MF.insert(I, LoopCondition);
  MF.insert(I, LoopBody);

  BeforeLoop->addSuccessor(LoopCondition);
  BeforeLoop->removeSuccessor(AfterLoop);

  LoopCondition->addSuccessor(LoopBody);
  LoopCondition->addSuccessor(AfterLoop);

  LoopBody->addSuccessor(LoopCondition);

  // Build the entry
  MIB.setInsertPt(*BeforeLoop, BeforeLoop->end());
  MIB.buildBr(*LoopCondition);

  // Build the loop condition
  MIB.setInsertPt(*LoopCondition, LoopCondition->begin());
  auto NextAmount = MRI.createGenericVirtualRegister(S8);
  auto NextValue = MRI.createGenericVirtualRegister(S8);
  auto Amount = MIB.buildInstr(TargetOpcode::G_PHI)
                    .addDef(MRI.createGenericVirtualRegister(S8))
                    .addUse(InitAmount)
                    .addMBB(BeforeLoop)
                    .addUse(NextAmount)
                    .addMBB(LoopBody);

  auto Value = MIB.buildInstr(TargetOpcode::G_PHI)
                   .addDef(MRI.createGenericVirtualRegister(S8))
                   .addUse(InitVal)
                   .addMBB(BeforeLoop)
                   .addUse(NextValue)
                   .addMBB(LoopBody);

  auto Zero = MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 0);
  auto Cmp = MIB.buildICmp(CmpInst::ICMP_EQ,
                           MRI.createGenericVirtualRegister(S8), Amount, Zero);
  MIB.buildBrCond(Cmp, *AfterLoop);
  MIB.buildBr(*LoopBody);

  // Build the loop body
  MIB.setInsertPt(*LoopBody, LoopBody->begin());

  MRI.setRegClass(Value.getReg(0), &GB::GPR8RegClass);
  MRI.setRegClass(NextValue, &GB::GPR8RegClass);
  MIB.buildInstr(getShiftOpcode(MI.getOpcode()), {NextValue}, {Value});

  auto One = MIB.buildConstant(MRI.createGenericVirtualRegister(S8), 1);
  MIB.buildSub(NextAmount, Amount, One);
  MIB.buildBr(*LoopCondition);

  // Build the exit
  MIB.setInsertPt(*AfterLoop, AfterLoop->begin());
  MIB.buildCopy(Result, Value);

  MI.eraseFromParent();
  return true;
}

bool GBLegalizerInfo::legalizeLargeAddSub(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  assert(MI.getOpcode() == TargetOpcode::G_ADD ||
         MI.getOpcode() == TargetOpcode::G_SUB);

  bool IsAdd = MI.getOpcode() == TargetOpcode::G_ADD;
  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  DstOp ResultOp = MI.getOperand(0);
  auto ResultTy = ResultOp.getLLTTy(MRI);

  auto Size = ResultTy.getScalarSizeInBits();

  MachineIRBuilder MIB(MI);

  const LLT S8 = LLT::scalar(8);
  auto Op1Unmerge = MIB.buildUnmerge(S8, MI.getOperand(1));
  auto Op2Unmerge = MIB.buildUnmerge(S8, MI.getOperand(2));

  SmallVector<Register, 8> SplitResult;
  SplitResult.resize(Size / 8);

  for (unsigned I = 0; I < Size / 8; I += 1) {
    auto Opcode = [&] {
      if (I == 0) {
        return IsAdd ? GB::ADD_r : GB::SUB_r;
      }
      return IsAdd ? GB::ADC_r : GB::SBC_r;
    }();

    MIB.buildCopy(Register(GB::A), Op1Unmerge->getOperand(I));
    MRI.setRegClass(Op1Unmerge->getOperand(I).getReg(), &GB::GPR8RegClass);
    MRI.setRegClass(Op2Unmerge->getOperand(I).getReg(), &GB::GPR8RegClass);

    MIB.buildInstr(Opcode, {}, {Op2Unmerge->getOperand(I)});

    SplitResult[I] = MRI.createGenericVirtualRegister(S8);
    MIB.buildCopy(SplitResult[I], Register(GB::A));
    MRI.setRegClass(SplitResult[I], &GB::GPR8RegClass);
  }

  MIB.buildMergeValues(ResultOp, SplitResult);
  MI.eraseFromParent();
  return true;
}

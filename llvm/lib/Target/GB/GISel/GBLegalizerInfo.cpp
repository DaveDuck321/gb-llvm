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
  const LLT S32 = LLT::scalar(32);
  const LLT S64 = LLT::scalar(64);

  getActionDefinitionsBuilder({G_CONSTANT, G_IMPLICIT_DEF, G_FREEZE,
                               G_CONSTANT_FOLD_BARRIER, G_CONSTANT_POOL})
      .legalFor({S8, S16, P0})
      .clampScalar(0, S8, S16);

  getActionDefinitionsBuilder(G_FCONSTANT).lower();

  getActionDefinitionsBuilder({G_AND, G_OR, G_XOR})
      .legalFor({S8})
      .clampScalar(0, S8, S8);

  getActionDefinitionsBuilder({G_ICMP, G_SELECT})
      .clampScalar(0, S8, S8)
      .clampScalar(1, S8, S8)
      .custom();

  getActionDefinitionsBuilder({G_SMIN, G_SMAX, G_UMIN, G_UMAX, G_ABS}).lower();

  // TODO: experiment with S8 only
  getActionDefinitionsBuilder(G_PHI)
      .legalFor({P0, S8, S16})
      .clampScalar(0, S8, S16);
  getActionDefinitionsBuilder({G_TRAP, G_DEBUGTRAP}).alwaysLegal();

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
  // Shifts are narrowed and NOT libcalls since the shift __builtins have an
  // unfortunate circular dependency.
  // TODO: the descending powers of two here should be automatic
  // TODO: this is really fragile, ideally #1 would be legalized to S8 also.
  getActionDefinitionsBuilder({G_SHL, G_LSHR, G_ASHR, G_ROTR, G_ROTL})
      .customFor({S8})
      .customIf([=](const LegalityQuery &Query) {
        if (Query.MI == nullptr || Query.Types[0] != S16 ||
            Query.Opcode == G_ROTR || Query.Opcode == G_ROTL) {
          return false;
        }
        auto &MRI = Query.MI->getMF()->getRegInfo();
        Register Reg = Query.MI->getOperand(2).getReg();
        return getIConstantVRegValWithLookThrough(Reg, MRI).has_value();
      })
      .clampScalar(1, S8, S16)
      .clampScalar(0, S8, S64)
      .clampScalar(0, S8, S32)
      .clampScalar(0, S8, S16)
      .libcallFor({S16});

  getActionDefinitionsBuilder({G_SCMP, G_UCMP, G_SBFX, G_UBFX}).lower();
  getActionDefinitionsBuilder(G_BSWAP).legalFor({S8}).custom();

  // TODO: these should be libcalls
  // TODO: the descending powers of two here should be automatic
  getActionDefinitionsBuilder(
      {G_CTPOP, G_CTTZ, G_CTLZ, G_CTTZ_ZERO_UNDEF, G_CTLZ_ZERO_UNDEF})
      .clampScalar(0, S8, S8)
      .clampScalar(1, S8, S64)
      .clampScalar(1, S8, S32)
      .clampScalar(1, S8, S16)
      .clampScalar(1, S8, S8)
      .lower();

  getActionDefinitionsBuilder({G_UMULO,      G_SMULO,      G_UMULH,   G_SMULH,
                               G_UADDSAT,    G_SADDSAT,    G_USUBSAT, G_SSUBSAT,
                               G_USHLSAT,    G_SSHLSAT,    G_SMULFIX, G_UMULFIX,
                               G_SMULFIXSAT, G_UMULFIXSAT, G_SDIVFIX, G_UDIVFIX,
                               G_SDIVFIXSAT, G_UDIVFIXSAT, G_UADDO,   G_SADDO,
                               G_USUBO,      G_SSUBO})
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

  // Not all MERGEs are legal... But, much like in the DAG implementation, we
  // rely on the combiner (and EVERY other legalization rule) to implicitly
  // eliminate unsupported MERGEs. ISEL will detect, and complain about, MERGEs
  // that we haven't correctly legalized.
  getActionDefinitionsBuilder({G_MERGE_VALUES, G_UNMERGE_VALUES}).alwaysLegal();

  getActionDefinitionsBuilder(G_EXTRACT).lower();
  getActionDefinitionsBuilder(G_TRUNC)
      .legalFor({{S8, S16}})
      .clampScalar(1, S16, S16);

  getActionDefinitionsBuilder({G_INTTOPTR, G_PTRTOINT}).alwaysLegal();
  getActionDefinitionsBuilder(G_PTR_ADD).alwaysLegal();

  getActionDefinitionsBuilder(G_BR).alwaysLegal();
  getActionDefinitionsBuilder(G_BRCOND).legalFor({S8}).clampScalar(0, S8, S8);
  getActionDefinitionsBuilder(G_BRINDIRECT).legalFor({P0});

  getActionDefinitionsBuilder({G_FRAME_INDEX, G_GLOBAL_VALUE, G_BLOCK_ADDR})
      .legalFor({P0});

  getActionDefinitionsBuilder({G_MEMCPY, G_MEMMOVE, G_MEMSET}).libcall();

  getActionDefinitionsBuilder(G_FENCE).alwaysLegal();
  getActionDefinitionsBuilder(
      {G_ATOMICRMW_XCHG, G_ATOMIC_CMPXCHG, G_ATOMICRMW_ADD, G_ATOMICRMW_SUB,
       G_ATOMICRMW_AND, G_ATOMICRMW_NAND, G_ATOMICRMW_OR, G_ATOMICRMW_XOR,
       G_ATOMICRMW_MAX, G_ATOMICRMW_MIN, G_ATOMICRMW_UMAX, G_ATOMICRMW_UMIN})
      .libcallFor({S8})
      .unsupported();

  getActionDefinitionsBuilder(G_ATOMIC_CMPXCHG_WITH_SUCCESS).lower();
  getActionDefinitionsBuilder({G_ATOMICRMW_UINC_WRAP, G_ATOMICRMW_UDEC_WRAP,
                               G_ATOMICRMW_USUB_COND, G_ATOMICRMW_USUB_SAT})
      .unsupported();

  getActionDefinitionsBuilder({G_ATOMICRMW_FADD, G_ATOMICRMW_FSUB,
                               G_ATOMICRMW_FMAX, G_ATOMICRMW_FMIN,
                               G_ATOMICRMW_FMAXIMUM, G_ATOMICRMW_FMINIMUM})
      .unsupported();

  getActionDefinitionsBuilder(
      {G_FADD,    G_FSUB,    G_FMUL,    G_FDIV,  G_FMA,     G_FPOW,
       G_FREM,    G_FCOS,    G_FSIN,    G_FTAN,  G_FACOS,   G_FASIN,
       G_FATAN,   G_FATAN2,  G_FCOSH,   G_FSINH, G_FTANH,   G_FLOG10,
       G_FLOG,    G_FLOG2,   G_FEXP,    G_FEXP2, G_FEXP10,  G_FCEIL,
       G_FFLOOR,  G_FMINNUM, G_FMAXNUM, G_FSQRT, G_FRINT,   G_FNEARBYINT,
       G_FSINCOS, G_FPOWI,   G_FLDEXP,  G_FPEXT, G_FPTRUNC, G_FCMP})
      .libcall();

  getActionDefinitionsBuilder(
      {G_FNEG,        G_FSHL,          G_FSHR,         G_FMAD,
       G_FFREXP,      G_FPTOSI_SAT,    G_FPTOUI_SAT,   G_FCOPYSIGN,
       G_IS_FPCLASS,  G_FCANONICALIZE, G_FMINNUM_IEEE, G_FMAXNUM_IEEE,
       G_FABS,        G_FMINIMUM,      G_FMAXIMUM,     G_FMINIMUMNUM,
       G_FMAXIMUMNUM, G_GET_FPENV,     G_SET_FPENV,    G_RESET_FPENV,
       G_GET_FPMODE,  G_SET_FPMODE,    G_RESET_FPMODE, G_STRICT_FADD,
       G_STRICT_FSUB, G_STRICT_FMUL,   G_STRICT_FDIV,  G_STRICT_FREM,
       G_STRICT_FMA,  G_STRICT_FSQRT,  G_STRICT_FLDEXP})
      .lower();

  getActionDefinitionsBuilder({G_SITOFP, G_UITOFP})
      .clampScalar(1, S32, S64)
      .libcall();

  getActionDefinitionsBuilder({G_FPTOSI, G_FPTOUI})
      .clampScalar(0, S32, S64)
      .libcall();

  // G_ASSERT_ZEXT
  // G_ASSERT_ALIGN
  // G_ABDS
  // G_ABDU
  // G_PTRAUTH_GLOBAL_VALUE
  // G_INSERT
  // G_BITCAST
  // G_INDEXED_LOAD
  // G_INDEXED_SEXTLOAD
  // G_INDEXED_ZEXTLOAD
  // G_INDEXED_STORE
  // G_PREFETCH
  // G_INVOKE_REGION_START
  //
  // G_VASTART
  // G_VAARG

  // G_PTRMASK
  // G_LROUND
  // G_LLROUND
  // G_BRJT
  // G_BITREVERSE
  // G_ADDRSPACE_CAST
  // G_JUMP_TABLE
  // G_DYN_STACKALLOC
  // G_STACKSAVE
  // G_STACKRESTORE
  // G_READ_REGISTER
  // G_WRITE_REGISTER
  // G_MEMCPY_INLINE
  // G_BZERO
  // G_UBSANTRAP
}

bool GBLegalizerInfo::legalizeCustom(LegalizerHelper &Helper, MachineInstr &MI,
                                     LostDebugLocObserver &LocObserver) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unimplemented custom legalization");
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
  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto P0 = LLT::pointer(0, 16);
  auto S16 = LLT::scalar(16);

  auto PtrReg = MI.getOperand(0).getReg();
  auto AddrReg = MI.getOperand(1).getReg();
  assert(MRI.getType(PtrReg) == P0);
  assert(MRI.getType(AddrReg) == P0);

  Helper.Observer.changingInstr(MI);
  if (MI.getOpcode() == TargetOpcode::G_LOAD) {
    // Result goes in PtrReg
    MachineIRBuilder MIB(*MI.getParent(), ++MI.getIterator());
    auto ToLoad = MRI.createGenericVirtualRegister(S16);
    MI.getOperand(0).ChangeToRegister(ToLoad, /*isDef=*/true);
    MIB.buildIntToPtr(PtrReg, ToLoad);
  } else {
    // We're storing PtrReg
    assert(MI.getOpcode() == TargetOpcode::G_STORE);

    MachineIRBuilder MIB(MI);
    auto ToStore = MRI.createGenericVirtualRegister(S16);
    MIB.buildPtrToInt(ToStore, PtrReg);
    MI.getOperand(0).ChangeToRegister(ToStore, /*isDef=*/false);
  }
  Helper.Observer.changedInstr(MI);
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
  MIB.setInsertPt(*BeforeSelect, MI);
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

  auto LHSType = MRI.getType(LHS);
  auto RHSType = MRI.getType(RHS);

  // Decay pointer comparisons into 16-bit integer comparisons -- these will
  // later be legalized down to 8-bit comparisons.
  if (LHSType.isPointer() || RHSType.isPointer()) {
    Helper.Observer.changingInstr(MI);

    if (LHSType.isPointer()) {
      auto NewLHS = MIB.buildPtrToInt(LLT::scalar(16), LHS);
      MI.getOperand(2).setReg(NewLHS.getReg(0));
    }

    if (RHSType.isPointer()) {
      auto NewRHS = MIB.buildPtrToInt(LLT::scalar(16), RHS);
      MI.getOperand(3).setReg(NewRHS.getReg(0));
    }

    Helper.Observer.changedInstr(MI);
    return true;
  }

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
    return GB::RRC_r;
  case TargetOpcode::G_ROTL:
    return GB::RLC_r;
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

  unsigned GenericOpcode = MI.getOpcode();
  unsigned ResultMask = 0;
  if (GenericOpcode == TargetOpcode::G_SHL && Amount > 3) {
    GenericOpcode = TargetOpcode::G_ROTL;
    ResultMask = static_cast<uint8_t>(~((1U << Amount) - 1U));
  }

  if (GenericOpcode == TargetOpcode::G_LSHR && Amount > 3) {
    GenericOpcode = TargetOpcode::G_ROTR;
    ResultMask = static_cast<uint8_t>((0x80U >> (Amount - 1U)) - 1U);
  }

  if (GenericOpcode == TargetOpcode::G_ROTR ||
      GenericOpcode == TargetOpcode::G_ROTL) {
    if (Amount == 3 || Amount == 4 || Amount == 5) {
      Amount = static_cast<uint8_t>(Amount - 4) % 8U;
      auto ThisSwapResult = MRI.createGenericVirtualRegister(S8);
      MRI.setRegClass(ThisSwapResult, &GB::GPR8RegClass);
      MIB.buildInstr(GB::SWAP_r, {ThisSwapResult}, {LastShiftResult});
      LastShiftResult = ThisSwapResult;
    }

    if (Amount >= 6) {
      Amount = 8 - Amount;
      if (GenericOpcode == TargetOpcode::G_ROTR) {
        GenericOpcode = TargetOpcode::G_ROTL;
      } else {
        assert(GenericOpcode == TargetOpcode::G_ROTL);
        GenericOpcode = TargetOpcode::G_ROTR;
      }
    }
  }

  unsigned TargetOpcode = getShiftOpcode(GenericOpcode);
  for (unsigned I = 0; I < Amount; I += 1) {
    auto ThisShiftResult = MRI.createGenericVirtualRegister(S8);
    MRI.setRegClass(ThisShiftResult, &GB::GPR8RegClass);
    MIB.buildInstr(TargetOpcode, {ThisShiftResult}, {LastShiftResult});
    LastShiftResult = ThisShiftResult;
  }

  if (ResultMask != 0) {
    auto Mask = MRI.createGenericVirtualRegister(S8);
    MIB.buildConstant(Mask, ResultMask);
    MIB.buildAnd(Result, LastShiftResult, Mask);
  } else {
    MIB.buildCopy(Result, LastShiftResult);
  }
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
  MachineRegisterInfo &MRI = MF.getRegInfo();

  auto ShiftValue = MI.getOperand(1).getReg();
  if (MRI.getType(ShiftValue) == LLT::scalar(8)) {
    return legalizeShiftRotate8(Helper, MI, LocObserver);
  }

  assert(MRI.getType(ShiftValue) == LLT::scalar(16));
  return legalizeShiftRotate16(Helper, MI, LocObserver);
}

bool GBLegalizerInfo::legalizeShiftRotate8(
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

bool GBLegalizerInfo::legalizeShiftRotate16(
    LegalizerHelper &Helper, MachineInstr &MI,
    LostDebugLocObserver &LocObserver) const {
  MachineFunction &MF = *MI.getMF();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  if (MI.getOpcode() == TargetOpcode::G_ROTL ||
      MI.getOpcode() == TargetOpcode::G_ROTR) {
    return false;
  }

  auto S8 = LLT::scalar(8);
  auto S16 = LLT::scalar(16);
  auto Result = MI.getOperand(0).getReg();
  auto Val = MI.getOperand(1).getReg();
  auto AmountReg = MI.getOperand(2).getReg();
  assert(MRI.getType(Val) == S16);

  auto Constant = getIConstantVRegValWithLookThrough(AmountReg, MRI);
  if (not Constant.has_value()) {
    return false;
  }

  size_t Amount = Constant->Value.getZExtValue();

  MachineIRBuilder MIB(MI);
  auto Unmerge = MIB.buildUnmerge(S8, Val);
  Register Lower = Unmerge.getReg(0);
  Register Upper = Unmerge.getReg(1);
  MRI.setRegClass(Lower, &GB::GPR8RegClass);
  MRI.setRegClass(Upper, &GB::GPR8RegClass);

  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unrecognized opcode");

  case TargetOpcode::G_SHL:
    if (Amount < 8) {
      for (size_t I = 0; I < Amount; I += 1) {
        auto NewLower = MRI.createGenericVirtualRegister(S8);
        auto NewUpper = MRI.createGenericVirtualRegister(S8);
        Lower = MIB.buildInstr(GB::SLA_r, {NewLower}, {Lower}).getReg(0);
        Upper = MIB.buildInstr(GB::RL_r, {NewUpper}, {Upper}).getReg(0);
        MRI.setRegClass(Lower, &GB::GPR8RegClass);
        MRI.setRegClass(Upper, &GB::GPR8RegClass);
      }
    } else {
      Amount -= 8;

      auto ShiftAmountReg = MRI.createGenericVirtualRegister(S8);
      MIB.buildConstant(ShiftAmountReg, Amount);

      Upper = MRI.createGenericVirtualRegister(S8);
      MIB.buildShl(Upper, Lower, ShiftAmountReg);

      Lower = MRI.createGenericVirtualRegister(S8);
      MIB.buildConstant(Lower, 0);
    }
    break;

  case TargetOpcode::G_LSHR:
    if (Amount >= 8) {
      Amount -= 8;

      auto ShiftAmountReg = MRI.createGenericVirtualRegister(S8);
      MIB.buildConstant(ShiftAmountReg, Amount);

      Lower = MRI.createGenericVirtualRegister(S8);
      MIB.buildLShr(Lower, Upper, ShiftAmountReg);

      Upper = MRI.createGenericVirtualRegister(S8);
      MIB.buildConstant(Upper, 0);
      break;
    }
    [[fallthrough]];
  case TargetOpcode::G_ASHR: {
    unsigned Opcode =
        MI.getOpcode() == TargetOpcode::G_ASHR ? GB::SRA_r : GB::SRL_r;

    for (size_t I = 0; I < Amount; I += 1) {
      auto NewLower = MRI.createGenericVirtualRegister(S8);
      auto NewUpper = MRI.createGenericVirtualRegister(S8);
      Upper = MIB.buildInstr(Opcode, {NewUpper}, {Upper}).getReg(0);
      Lower = MIB.buildInstr(GB::RR_r, {NewLower}, {Lower}).getReg(0);
      MRI.setRegClass(Lower, &GB::GPR8RegClass);
      MRI.setRegClass(Upper, &GB::GPR8RegClass);
    }
    break;
  }
  }

  MIB.buildMergeValues(Result, {Lower, Upper});
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

  if (Size == 8 || (Size == 16 && IsAdd)) {
    // Already legal (or we're going to legalize it later)
    return true;
  }

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

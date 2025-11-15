// Immediate types
def to_upper_imm
    : SDNodeXForm<
          imm,
          [{ return CurDAG->getTargetConstant((N->getZExtValue() >> 8) & 0xFF, SDLoc(N), MVT::i8); }]>;
def to_lower_imm
    : SDNodeXForm<
          imm,
          [{ return CurDAG->getTargetConstant(N->getZExtValue() & 0xFF, SDLoc(N), MVT::i8); }]>;

// Helpers: sync with GBMOFlags.hpp
def to_upper_global
    : SDNodeXForm<
          tglobaladdr,
          [{ return CurDAG->getTargetGlobalAddress(N->getGlobal(), SDLoc(N), MVT::i8, N->getOffset(), 1); }]>;
def to_lower_global
    : SDNodeXForm<
          tglobaladdr,
          [{ return CurDAG->getTargetGlobalAddress(N->getGlobal(), SDLoc(N), MVT::i8, N->getOffset(), 2); }]>;

def to_upper_symbol
    : SDNodeXForm<
          texternalsym,
          [{ return CurDAG->getTargetExternalSymbol(N->getSymbol(), MVT::i8, 1); }]>;
def to_lower_symbol
    : SDNodeXForm<
          texternalsym,
          [{ return CurDAG->getTargetExternalSymbol(N->getSymbol(), MVT::i8, 2); }]>;

def ADDR_WRAPPER
    : SDNode<"GBISD::ADDR_WRAPPER",
             SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisPtrTy<0>]>>;

// Combine and split
def SDT_GBCOMBINE
    : SDTypeProfile<1,
                    2, [SDTCisVT<0, i16>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def COMBINE_NODE : SDNode<"GBISD::COMBINE", SDT_GBCOMBINE, []>;

def : Pat<(COMBINE_NODE GPR8:$rs1, GPR8:$rs2),
          (REG_SEQUENCE GPR16, GPR8:$rs1, LowerSubReg, GPR8:$rs2, UpperSubReg)>;

def SDT_GBSPLIT : SDTypeProfile<1, 1, [SDTCisVT<0, i8>, SDTCisVT<1, i16>]>;
def LOWER_NODE : SDNode<"GBISD::LOWER", SDT_GBSPLIT, []>;
def UPPER_NODE : SDNode<"GBISD::UPPER", SDT_GBSPLIT, []>;

def : Pat<(LOWER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;
def : Pat<(UPPER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, UpperSubReg)>;

def : Pat<(trunc GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;

// Constants
def : Pat<(imm8:$imm), (LDI8_r imm8:$imm)>;
def : Pat<(imm16:$imm), (LDI16 imm16:$imm)>;
def : Pat<(LOWER_NODE(ADDR_WRAPPER tglobaladdr:$imm)),
          (LDI8_r(to_lower_global $imm))>;
def : Pat<(UPPER_NODE(ADDR_WRAPPER tglobaladdr:$imm)),
          (LDI8_r(to_upper_global $imm))>;
def : Pat<(ADDR_WRAPPER tglobaladdr:$imm), (LDI16 $imm)>;
def : Pat<(LOWER_NODE(ADDR_WRAPPER texternalsym:$imm)),
          (LDI8_r(to_lower_symbol $imm))>;
def : Pat<(UPPER_NODE(ADDR_WRAPPER texternalsym:$imm)),
          (LDI8_r(to_upper_symbol $imm))>;
def : Pat<(ADDR_WRAPPER texternalsym:$imm), (LDI16 $imm)>;

def : Pat<(ADDR_WRAPPER tblockaddress:$imm), (LDI16 imm16:$imm)>;

def : Pat<(frameindex:$imm), (LD_HL_SP(SDNodeXForm<frameindex, [{
  auto FI = cast<FrameIndexSDNode>(N);
  return CurDAG->getTargetFrameIndex(FI->getIndex(), TLI->getPointerTy(CurDAG->getDataLayout()));
}]> $imm))>;

// Loads/ stores
def : Pat<(load io_imm16:$imm), (LDH_A_iImm(to_lower_imm $imm))>;
def : Pat<(load GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;
def : Pat<(extload GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;

def : Pat<(atomic_load io_imm16:$imm), (LDH_A_iImm(to_lower_imm $imm))>;
def : Pat<(atomic_load GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;

def : Pat<(store A, io_imm16:$imm), (LDH_iImm_A(to_lower_imm $imm))>;
def : Pat<(store A, GPR16:$rs), (LD_iGPR16_A GPR16:$rs)>;

def : Pat<(atomic_store A, io_imm16:$imm), (LDH_iImm_A(to_lower_imm $imm))>;
def : Pat<(atomic_store A, GPR16:$rs), (LD_iGPR16_A GPR16:$rs)>;

// Required when passing arguments on the stack
def LD_HL_SP_NODE
    : SDNode<"GBISD::LD_HL_SP",
             SDTypeProfile<1, 1, [SDTCisVT<0, i16>, SDTCisVT<1, i8>]>, []>;
def : Pat<(LD_HL_SP_NODE imm8:$imm), (LD_HL_SP imm8:$imm)>;

// ALU
//   CP operands: 0) condition code, 1) LHS, 2) RHS
def CP_NODE
    : SDNode<
          "GBISD::CP",
          SDTypeProfile<
              0,
              3, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>,
          [SDNPOutGlue]>;
def : Pat<(CP_NODE SETUGT, GPR8:$rs, A), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETUGE, A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETULT, A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETULE, GPR8:$rs, A), (CP_r GPR8:$rs)>;

def : Pat<(CP_NODE SETEQ, A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETNE, A, GPR8:$rs), (CP_r GPR8:$rs)>;

def : Pat<(CP_NODE SETUGE, A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETULT, A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETEQ, A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETNE, A, imm8:$imm8), (CPI imm8:$imm8)>;

foreach shift = [0, 1, 2, 3, 4, 5, 6, 7] in {
  defvar imm = !shl(1, shift);
  def : Pat<(CP_NODE SETNE, (and GPR8:$rs, imm), 0), (BIT_r GPR8:$rs, shift)>;
  def : Pat<(CP_NODE SETEQ, (and GPR8:$rs, imm), 0), (BIT_r GPR8:$rs, shift)>;

  def : Pat<(CP_NODE SETEQ, (srl GPR8:$rs, (i8 shift)), 0), (BIT_r GPR8:$rs,
                                                                shift)>;
  def : Pat<(CP_NODE SETNE, (srl GPR8:$rs, (i8 shift)), 0), (BIT_r GPR8:$rs,
                                                                shift)>;
}

multiclass ALU_Pat<SDNode node, GBInstr regInstr,
                   bit alreadyHasOutputGlue = 0> {
  def : Pat<!setdagop((? A, GPR8:$rs), node),
            !setdagop((? GPR8:$rs), regInstr)>;

  // ALU ops set the zero flag, CP 0 is redundant in these cases
  // Unfortunately only one glued output is permitted so we can't math this
  // pattern if we need the carry flag
  if !not(alreadyHasOutputGlue) then {
    foreach flag = [SETNE, SETEQ] in {
      def : Pat<(CP_NODE flag, !setdagop((? A, GPR8:$rs), node), 0),
                !setdagop((? GPR8:$rs), regInstr)>;
    }
  }
}

defm : ALU_Pat<add, ADD_r>;
defm : ALU_Pat<addc, ADD_r, /*alreadyHasOutputGlue=*/1>;
defm : ALU_Pat<adde, ADC_r, /*alreadyHasOutputGlue=*/1>;
def : Pat<(add GPR8:$rs, 1), (INC_r GPR8:$rs)>;

defm : ALU_Pat<sub, SUB_r>;
defm : ALU_Pat<subc, SUB_r, /*alreadyHasOutputGlue=*/1>;
defm : ALU_Pat<sube, SBC_r, /*alreadyHasOutputGlue=*/1>;
def : Pat<(add GPR8:$rs, -1), (DEC_r GPR8:$rs)>;

defm : ALU_Pat<and, AND_r>;
defm : ALU_Pat<xor, XOR_r>;
defm : ALU_Pat<or, OR_r>;

def : Pat<(xor A, -1), (CPL)>;

def SDT_GBIncDec16 : SDTypeProfile<1, 1, [SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def INC16_NODE : SDNode<"GBISD::INC16", SDT_GBIncDec16, []>;
def DEC16_NODE : SDNode<"GBISD::DEC16", SDT_GBIncDec16, []>;
def : Pat<(INC16_NODE GPR16:$rd), (INC16 GPR16:$rd)>;
def : Pat<(DEC16_NODE GPR16:$rd), (DEC16 GPR16:$rd)>;

// Rotates
def : Pat<(rotl GPR8:$rs, (i8 1)), (RLC_r GPR8:$rs)>;
def : Pat<(rotr GPR8:$rs, (i8 1)), (RRC_r GPR8:$rs)>;

def SDT_GBSimpleShift : SDTypeProfile<1, 1, [SDTCisVT<0, i8>, SDTCisVT<0, i8>]>;
def RR_NODE : SDNode<"GBISD::RR", SDT_GBSimpleShift, [SDNPInGlue, SDNPOutGlue]>;
def RL_NODE : SDNode<"GBISD::RL", SDT_GBSimpleShift, [SDNPInGlue, SDNPOutGlue]>;
def SHL_NODE : SDNode<"GBISD::SHL", SDT_GBSimpleShift, [SDNPOutGlue]>;
def ASHR_NODE : SDNode<"GBISD::ASHR", SDT_GBSimpleShift, [SDNPOutGlue]>;
def LSHR_NODE : SDNode<"GBISD::LSHR", SDT_GBSimpleShift, [SDNPOutGlue]>;

def : Pat<(RR_NODE GPR8:$rs), (RR_r GPR8:$rs)>;
def : Pat<(RL_NODE GPR8:$rs), (RL_r GPR8:$rs)>;
def : Pat<(SHL_NODE GPR8:$rs), (SLA_r GPR8:$rs)>;
def : Pat<(ASHR_NODE GPR8:$rs), (SRA_r GPR8:$rs)>;
def : Pat<(LSHR_NODE GPR8:$rs), (SRL_r GPR8:$rs)>;

// Atomic rmws special cases
def SDT_ATOMIC_INC_DEC : SDTypeProfile<1, 1, [SDTCisVT<0, i8>, SDTCisPtrTy<1>]>;
def ATOMIC_INC_NODE
    : SDNode<"GBISD::ATOMIC_INC", SDT_ATOMIC_INC_DEC,
             [SDNPHasChain, SDNPMayStore, SDNPMayLoad, SDNPMemOperand]>;
def ATOMIC_DEC_NODE
    : SDNode<"GBISD::ATOMIC_DEC", SDT_ATOMIC_INC_DEC,
             [SDNPHasChain, SDNPMayStore, SDNPMayLoad, SDNPMemOperand]>;

def : Pat<(ATOMIC_INC_NODE HL), (INC_iHL)>;
def : Pat<(ATOMIC_DEC_NODE HL), (DEC_iHL)>;

def SDT_ATOMIC_SET_RESET
    : SDTypeProfile<1, 2, [SDTCisVT<0, i8>, SDTCisPtrTy<1>, SDTCisVT<2, i8>]>;
def ATOMIC_BIT_SET_NODE
    : SDNode<"GBISD::ATOMIC_BIT_SET", SDT_ATOMIC_SET_RESET,
             [SDNPHasChain, SDNPMayStore, SDNPMayLoad, SDNPMemOperand]>;
def ATOMIC_BIT_RESET_NODE
    : SDNode<"GBISD::ATOMIC_BIT_RESET", SDT_ATOMIC_SET_RESET,
             [SDNPHasChain, SDNPMayStore, SDNPMayLoad, SDNPMemOperand]>;

def : Pat<(ATOMIC_BIT_SET_NODE HL, imm8:$imm8), (SET_iHL imm8:$imm8)>;
def : Pat<(ATOMIC_BIT_RESET_NODE HL, imm8:$imm8), (RES_iHL imm8:$imm8)>;

// Calling
def SDT_GBCALL_SEQ : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def GBCallSeqStart
    : SDNode<"ISD::CALLSEQ_START", SDT_GBCALL_SEQ, [SDNPHasChain, SDNPOutGlue]>;
def GBCallSeqEnd
    : SDNode<"ISD::CALLSEQ_END",
             SDT_GBCALL_SEQ, [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def : Pat<(GBCallSeqStart timm:$imm, timm:$out), (ADJCALLSTACKDOWN imm16:$imm,
                                                     imm16:$out)>;
def : Pat<(GBCallSeqEnd timm:$imm, timm:$out), (ADJCALLSTACKUP imm16:$imm,
                                                   imm16:$out)>;

def GBCall : SDNode<"GBISD::CALL", SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>,
                    [SDNPMayStore, SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                     SDNPVariadic]>;
def : Pat<(GBCall imm16:$imm), (CALL imm16:$imm)>;
def : Pat<(GBCall(ADDR_WRAPPER tglobaladdr:$imm)), (CALL imm16:$imm)>;
def : Pat<(GBCall(ADDR_WRAPPER texternalsym:$imm)), (CALL imm16:$imm)>;
def : Pat<(GBCall HL), (CALL_HL)>;

def RET_NODE : SDNode<"GBISD::RET",
                      SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def RETI_NODE : SDNode<"GBISD::RETI",
                       SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def : Pat<(RET_NODE), (RET)>;
def : Pat<(RETI_NODE), (RETI)>;

// Branches
def : Pat<(br bb:$imm), (JP bb_pc_rel:$imm)>;
def : Pat<(brind HL), (JP_HL)>;

def BR_CC_NODE
    : SDNode<
          "GBISD::BR_CC",
          SDTypeProfile<
              0, 2, [SDTCisVT<0, /*CC=*/OtherVT>, SDTCisVT<1, /*BB=*/OtherVT>]>,
          [SDNPInGlue, SDNPHasChain]>;
def : Pat<(BR_CC_NODE SETUGT, bb:$imm), (JP_COND GBFLAG_C.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETUGE, bb:$imm), (JP_COND GBFLAG_NC.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETULT, bb:$imm), (JP_COND GBFLAG_C.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETULE, bb:$imm), (JP_COND GBFLAG_NC.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETEQ, bb:$imm), (JP_COND GBFLAG_Z.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETNE, bb:$imm), (JP_COND GBFLAG_NZ.Encoding, bb:$imm)>;

def SBR_CC_NODE
    : SDNode<"GBISD::SBR_CC",
             SDTypeProfile<0, 4,
                           [SDTCisVT<0, /*CC=*/OtherVT>,
                            SDTCisVT<1, /*BB=*/OtherVT>,
                            SDTCisVT<2, /*LHS=*/i8>, SDTCisVT<3, /*RHS=*/i8>]>,
             [SDNPHasChain]>;

def : Pat<(SBR_CC_NODE SETGT, bb:$bb, GPR8:$rs1, GPR8:$rs2),
          (SBR_CC_r 0, bb:$bb, GPR8:$rs1, GPR8:$rs2)>;
def : Pat<(SBR_CC_NODE SETLT, bb:$bb, GPR8:$rs1, GPR8:$rs2),
          (SBR_CC_r 1, bb:$bb, GPR8:$rs1, GPR8:$rs2)>;
def : Pat<(SBR_CC_NODE SETGE, bb:$bb, GPR8:$rs1, GPR8:$rs2),
          (SBR_CC_r 2, bb:$bb, GPR8:$rs1, GPR8:$rs2)>;
def : Pat<(SBR_CC_NODE SETLE, bb:$bb, GPR8:$rs1, GPR8:$rs2),
          (SBR_CC_r 3, bb:$bb, GPR8:$rs1, GPR8:$rs2)>;

def : Pat<(SBR_CC_NODE SETGT, bb:$bb, GPR8:$rs1, imm8:$imm),
          (SBR_CCI 0, bb:$bb, GPR8:$rs1, imm8:$imm)>;
def : Pat<(SBR_CC_NODE SETLT, bb:$bb, GPR8:$rs1, imm8:$imm),
          (SBR_CCI 1, bb:$bb, GPR8:$rs1, imm8:$imm)>;
def : Pat<(SBR_CC_NODE SETGE, bb:$bb, GPR8:$rs1, imm8:$imm),
          (SBR_CCI 2, bb:$bb, GPR8:$rs1, imm8:$imm)>;
def : Pat<(SBR_CC_NODE SETLE, bb:$bb, GPR8:$rs1, imm8:$imm),
          (SBR_CCI 3, bb:$bb, GPR8:$rs1, imm8:$imm)>;

// Selects
def SELECT_CC_NODE
    : SDNode<"GBISD::SELECT_CC",
             SDTypeProfile<
                 1, 3,
                 [/*Out=*/SDTCisVT<0, i8>, /*CCODE=*/SDTCisVT<1, OtherVT>,
                  /*IfTrue=*/SDTCisVT<2, i8>, /*IfFalse=*/SDTCisVT<3, i8>]>,
             [SDNPInGlue]>;
def : Pat<(SELECT_CC_NODE SETUGT, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_C.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETUGE, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_NC.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETULT, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_C.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETULE, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_NC.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETEQ, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_Z.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETNE, GPR8:$ifTrue, GPR8:$ifFalse),
          (SELECT_CC GBFLAG_NZ.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;

def SSELECT_CC_NODE : SDNode<"GBISD::SSELECT_CC",
                             SDTypeProfile<1, 5,
                                           [
                                               /*Out=*/SDTCisVT<0, i8>,
                                               /*CCODE=*/SDTCisVT<1, OtherVT>,
                                               /*LHS=*/SDTCisVT<2, i8>,
                                               /*RHS=*/SDTCisVT<3, i8>,
                                               /*IfTrue=*/SDTCisVT<4, i8>,
                                               /*IfFalse=*/SDTCisVT<5, i8>]>,
                             []>;
def : Pat<(SSELECT_CC_NODE SETGT, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CC_r 0, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETLT, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CC_r 1, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETGE, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CC_r 2, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETLE, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CC_r 3, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue, GPR8:$ifFalse)>;

def : Pat<(SSELECT_CC_NODE SETGT, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CCI 0, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETLT, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CCI 1, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETGE, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CCI 2, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SSELECT_CC_NODE SETLE, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue,
              GPR8:$ifFalse),
          (SSELECT_CCI 3, GPR8:$lhs, imm8:$imm, GPR8:$ifTrue, GPR8:$ifFalse)>;

// Special instructions
def : Pat<(trap), (TRAP)>;

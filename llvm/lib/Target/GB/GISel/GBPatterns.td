// Immediate types
def p0 : PtrValueType<i16, 0>;

def to_upper_imm
    : SDNodeXForm<
          imm,
          [{ return CurDAG->getTargetConstant((N->getZExtValue() >> 8) & 0xFF, SDLoc(N), MVT::i8); }]>;
def to_lower_imm
    : SDNodeXForm<
          imm,
          [{ return CurDAG->getTargetConstant(N->getZExtValue() & 0xFF, SDLoc(N), MVT::i8); }]>;

def SDT_GBCOMBINE
    : SDTypeProfile<1,
                    2, [SDTCisVT<0, i16>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def COMBINE_NODE : SDNode<"GBISD::COMBINE", SDT_GBCOMBINE, []>;

def : GINodeEquiv<G_MERGE_VALUES, COMBINE_NODE>;
def : Pat<(COMBINE_NODE GPR8:$rs1, GPR8:$rs2),
          (REG_SEQUENCE GPR16, GPR8:$rs1, LowerSubReg, GPR8:$rs2, UpperSubReg)>;

def : Pat<(trunc GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;

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

// ALU
multiclass ALU_Pat<SDNode node, GBInstr regInstr> {
  def : Pat<!setdagop((? A, GPR8:$rs), node),
            !setdagop((? GPR8:$rs), regInstr)>;
}

defm : ALU_Pat<add, ADD_r>;
defm : ALU_Pat<addc, ADD_r>;
defm : ALU_Pat<adde, ADC_r>;
def : Pat<(add GPR8:$rs, 1), (INC_r GPR8:$rs)>;

defm : ALU_Pat<sub, SUB_r>;
defm : ALU_Pat<subc, SUB_r>;
defm : ALU_Pat<sube, SBC_r>;
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

// Branches
def : Pat<(br bb:$imm), (JP $imm)>;
def : Pat<(brind (p0 HL)), (JP_HL)>;
def : GINodeEquiv<G_BRINDIRECT, brind>;

// Special instructions
def : Pat<(trap), (TRAP)>;

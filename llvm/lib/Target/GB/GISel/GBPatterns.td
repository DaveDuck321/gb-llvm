// Immediate types
def p0 : PtrValueType<i16, 0>;

def to_upper_imm
    : SDNodeXForm<imm, [{ (void)N; llvm_unreachable("DAG is unsupported"); }]>;
def to_lower_imm
    : SDNodeXForm<imm, [{ (void)N; llvm_unreachable("DAG is unsupported"); }]>;

def : GISDNodeXFormEquiv<to_upper_imm>,
      GICustomOperandRenderer<"renderUpperImm">;
def : GISDNodeXFormEquiv<to_lower_imm>,
      GICustomOperandRenderer<"renderLowerImm">;

def INT_TO_PTR
    : SDNode<"0", SDTypeProfile<1, 1, [SDTCisPtrTy<0>, SDTCisVT<0, i16>]>, []>;
def : GINodeEquiv<G_INTTOPTR, INT_TO_PTR>;

def COMBINE
    : SDNode<"0",
             SDTypeProfile<
                 1, 2, [SDTCisVT<0, i16>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>,
             []>;
def : GINodeEquiv<G_MERGE_VALUES, COMBINE>;
def : Pat<(COMBINE GPR8:$rs1, GPR8:$rs2),
          (REG_SEQUENCE GPR16, GPR8:$rs1, LowerSubReg, GPR8:$rs2, UpperSubReg)>;

def : Pat<(trunc GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;

// Loads/ stores
def : Pat<(load(INT_TO_PTR io_imm16:$imm)), (LDH_A_iImm(to_lower_imm $imm))>;
def : Pat<(load GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;
def : Pat<(extload GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;

def : Pat<(atomic_load(INT_TO_PTR io_imm16:$imm)),
          (LDH_A_iImm(to_lower_imm $imm))>;
def : Pat<(atomic_load GPR16:$rs), (LD_r_iGPR16 GPR16:$rs)>;

def : Pat<(store A, (INT_TO_PTR io_imm16:$imm)),
          (LDH_iImm_A(to_lower_imm $imm))>;
def : Pat<(store A, GPR16:$rs), (LD_iGPR16_A GPR16:$rs)>;

def : Pat<(atomic_store A, (INT_TO_PTR io_imm16:$imm)),
          (LDH_iImm_A(to_lower_imm $imm))>;
def : Pat<(atomic_store A, GPR16:$rs), (LD_iGPR16_A GPR16:$rs)>;

// ALU
multiclass ALU_Pat<SDNode node, GBInstr regInstr> {
  def : Pat<!setdagop((? A, GPR8:$rs), node),
            !setdagop((? GPR8:$rs), regInstr)>;
}

defm : ALU_Pat<add, ADD_r>;
def : Pat<(add GPR8:$rs, 1), (INC_r GPR8:$rs)>;
def : Pat<(add GPR16:$rs, 1), (INC16 GPR16:$rs)>;
def : Pat<(ptradd GPR16:$rs, (i16 1)), (INC16 GPR16:$rs)>;

defm : ALU_Pat<sub, SUB_r>;
def : Pat<(add GPR8:$rs, -1), (DEC_r GPR8:$rs)>;
def : Pat<(add GPR16:$rs, -1), (DEC16 GPR16:$rs)>;
def : Pat<(ptradd GPR16:$rs, (i16 -1)), (DEC16 GPR16:$rs)>;

defm : ALU_Pat<and, AND_r>;
defm : ALU_Pat<xor, XOR_r>;
defm : ALU_Pat<or, OR_r>;

def : Pat<(xor A, -1), (CPL)>;

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
def : Pat<(brind(p0 HL)), (JP_HL)>;
def : GINodeEquiv<G_BRINDIRECT, brind>;

// Special instructions
def : Pat<(trap), (TRAP)>;

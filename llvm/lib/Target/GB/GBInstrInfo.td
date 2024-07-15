include "GBInstrFormats.td"

def NOP : GBInstrNoOperands<"nop", 0x00>;

defm LD : GBGPR8LD<"ld", 0x40>;
defm LDI8 : GBGPR8LDI8<"ld", 0x06>;
def LDI16 : GBGPR8LDI16<"ld", 0x01>;

let mayStore = true, Uses = [A] in {
  def LDH_iImm_A : _GB_SimpleImm8<"ldh ($imm), a", 0xE0, uimm8, FixupData8>;
  def LD_iImm_A : GBAbsImm16<"ld ($imm), a", 0xEA>;
  def LD_iR16_A : _GB_IR16S16Sequence<"ld ($rs), a", 0x02>;
}

let mayStore = true, Uses = [A, C] in
def LD_iC_A : GBInstrNoOperands<"ld (c), a", 0xE2>;

let mayStore = true, Uses = [A, HL], Defs = [HL] in {
  // TOOD GB: should also accept: ld (hl+-), a
  def LDI_iHL_A : GBInstrNoOperands<"ldi (hl), a", 0x22>;
  def LDD_iHL_A : GBInstrNoOperands<"ldd (hl), a", 0x32>;
}

let mayLoad = true, Defs = [A] in {
  def LDH_A_iImm : _GB_SimpleImm8<"ldh a, ($imm)", 0xF0, uimm8, FixupData8>;
  def LD_A_iImm : GBAbsImm16<"ld a, ($imm)", 0xFA>;
  def LD_A_iR16 : _GB_IR16S16Sequence<"ld a, ($rs)", 0x0A>;

  let Uses = [C] in
  def LD_A_iC : GBInstrNoOperands<"ld a, (c)", 0xF2>;
}

let mayLoad = true, Uses = [HL], Defs = [A, HL] in {
  // TOOD GB: should also accept: ld a, (hl+-)
  def LDI_A_iHL : GBInstrNoOperands<"ldi a, (hl)", 0x2A>;
  def LDD_A_iHL : GBInstrNoOperands<"ldd a, (hl)", 0x3A>;
}

let isMoveReg = true in {
  let Uses = [HL], Defs = [SP] in
  def LD_SP_HL : GBInstrNoOperands<"ld sp, hl", 0xF9>;

  let Uses = [SP], Defs = [HL, F] in
  def LD_HL_SP : _GB_SimpleImm8<"ld hl, sp, $imm", 0xF8, simm8, FixupData8>;
}

let mayStore = true, Uses = [SP] in
def LD_iImm16_SP : GBAbsImm16<"ld ($imm), sp", 0x08>;

let Size = 1 in {
  // TODO: be more specific about which flags are clobbered
  let isAdd = true in
  defm ADD : _GB_GPR8S1Sequence<"add", 0x80, (ins)>;
  defm SUB : _GB_GPR8S1Sequence<"sub", 0x90, (ins)>;
  defm AND : _GB_GPR8S1Sequence<"and", 0xA0, (ins)>;
  defm XOR : _GB_GPR8S1Sequence<"xor", 0xA8, (ins)>;
  defm OR  : _GB_GPR8S1Sequence<"or",  0xB0, (ins)>;

  let Uses = [A], Defs = [F], isCompare = true in
  defm CP  : _GB_GPR8S1Sequence<"cp",  0xB8, (ins)>;
}

let Size = 1, Uses = [A, F] in {
  defm ADC : _GB_GPR8S1Sequence<"adc", 0x88, (ins)>;
  defm SBC : _GB_GPR8S1Sequence<"sbc", 0x98, (ins)>;
}

let Uses = [A], Defs = [A, F] in {
  def ADDI : _GB_SimpleImm8<"add $imm", 0xC6, uimm8, FixupData8>;
  def SUBI : _GB_SimpleImm8<"sub $imm", 0xD6, uimm8, FixupData8>;
  def ANDI : _GB_SimpleImm8<"and $imm", 0xE6, uimm8, FixupData8>;
  def XORI : _GB_SimpleImm8<"xor $imm", 0xEE, uimm8, FixupData8>;
  def ORI  : _GB_SimpleImm8<"or $imm",  0xF6, uimm8, FixupData8>;
}

let Uses = [A, F], Defs = [A, F] in {
  def ADCI : _GB_SimpleImm8<"adc $imm", 0xCE, uimm8, FixupData8>;
  def SBCI : _GB_SimpleImm8<"sbc $imm", 0xDE, uimm8, FixupData8>;
}

let Uses = [A], Defs = [F], isCompare = true in
def CPI  : _GB_SimpleImm8<"cp $imm",  0xFE, uimm8, FixupData8>;

defm INC : GBIncDec8<"inc", 0x04>;
defm DEC : GBIncDec8<"dec", 0x05>;


let Size = 1 in {
  let Constraints = "$rs = $rd", Defs = [F] in {
    // TOOD GB: there's a hardware bug here... maybe find a way to model this?
    def INC16 : _GB_GPR16S16Sequence<"inc", 0x03, (outs GPR16:$rd)>;
    def DEC16 : _GB_GPR16S16Sequence<"dec", 0x0B, (outs GPR16:$rd)>;
  }

  let isAdd = true, Uses = [HL], Defs = [HL, F] in
  def ADD_HL : _GB_GPR16S16Sequence<"add hl,", 0x09, (outs)>;
}

let isAdd = true, Uses = [SP], Defs = [SP, F] in
def ADD_SP : _GB_SimpleImm8<"add sp, $imm", 0xe8, simm8, FixupData8>;

let Uses = [A], Defs = [A, F] in {
  def RLCA : GBInstrNoOperands<"rlca", 0x07>;
  def RRCA : GBInstrNoOperands<"rrca", 0x0F>;
  def CPL  : GBInstrNoOperands<"cpl", 0x2F>;
}

let Uses = [A, F], Defs = [A, F] in {
  def RLA  : GBInstrNoOperands<"rla", 0x17>;
  def RRA  : GBInstrNoOperands<"rra", 0x1F>;
}

let Uses = [A, F], Defs = [A, F] in {
  def DAA  : GBInstrNoOperands<"daa", 0x27>;
}

let Defs = [F] in {
  def SCF  : GBInstrNoOperands<"scf", 0x37>;
  def CCF  : GBInstrNoOperands<"ccf", 0x3F>;
}

// // CB instructions
let Defs = [F] in {
  defm RLC  : _GB_CBGPR8S1<"rlc", 0x00>;
  defm RRC  : _GB_CBGPR8S1<"rrc", 0x08>;
  defm SLA  : _GB_CBGPR8S1<"sla", 0x20>;
  defm SRA  : _GB_CBGPR8S1<"sra", 0x28>;
  defm SWAP : _GB_CBGPR8S1<"swap", 0x30>;
  defm SRL  : _GB_CBGPR8S1<"srl", 0x38>;
  defm BIT : GBCBGPR8S1Imm3<"bit", 0x40>;
}

defm RL   : GBCBGPR8S1AndFlag<"rl", 0x10>;
defm RR   : GBCBGPR8S1AndFlag<"rr", 0x18>;

defm RES : GBCBGPR8S1Imm3<"res", 0x80>;
defm SET : GBCBGPR8S1Imm3<"set", 0xC0>;

let isTerminator = true, isBranch = true in {
  let isBarrier = true in
  def JR : _GB_SimpleImm8<"jr $imm", 0x18, bb_pc_rel, FixupPCRel8>;

  let Uses = [F] in
  def JR_COND : GBFlagPCRel<"jr", 0x20>;
}

let isReturn = true, isTerminator = true in {
  let Uses = [SP], Defs = [SP] in {
    def RET : GBInstrNoOperands<"ret", 0xC9>;

    let hasSideEffects = true in
    def RETI : GBInstrNoOperands<"reti", 0xD9>;
  }

  let Size = 1, Uses = [F, SP], Defs = [SP] in
  def RET_COND : _GB_FlagSequence<"ret", "", 0xC0, (ins), FixupNone>;
}

let mayStore = true, isCall = true in {
  let Uses = [SP], Defs = [SP] in
  def CALL : GBAbsImm16<"call $imm", 0xCD, uimm16>;

  let Uses = [F, SP], Defs = [SP] in
  def CALL_COND : GBAbsFlagImm16<"call", 0xC4>;
}

let isTerminator = true, isBranch = true in {
  let isBarrier = true in
  def JP : GBAbsImm16<"jp $imm", 0xC3>;

  let Uses = [F] in
  def JP_COND : GBAbsFlagImm16<"jp", 0xC2>;

  let isBarrier = true, isIndirectBranch = true, Uses = [HL] in
  def JP_HL : GBInstrNoOperands<"jp (hl)", 0xE9>;
}

let hasSideEffects = true in {
  // TODO GB: add STOP 0 as a PSEUDO instruction
  // NOTE: stop is acually 2 bytes -- officially STOP 0 ...BUT...
  // the gameboy sometimes interprets it as a 1 byte instruction.
  // Lets just interpret that pattern as STOP NOP
  def STOP : GBInstrNoOperands<"stop", 0x10>;
  def HALT : GBInstrNoOperands<"halt", 0x76>;
  def DI   : GBInstrNoOperands<"di", 0xF3>;
  def EI   : GBInstrNoOperands<"ei", 0xFB>;
  def TRAP : GBInstrNoOperands<"trap", 0xD3>;
  def DEBUG_TRAP : GBInstrNoOperands<"debugtrap", 0xE3>;
}

let mayLoad = true in {
  def POP  : GBPushPop16<"pop", 0xC1, (outs SR16:$reg), (ins)>;
  def RST : GBRSTInstr<"rst", 0xC7>;
}

let mayStore = true in {
  def PUSH : GBPushPop16<"push", 0xC5, (outs), (ins SR16:$reg)>;
}

// Immediate types
def imm8 : ImmLeaf<i8, [{return isInt<8>(Imm);}]>;
def imm16 : ImmLeaf<i16, [{return isInt<16>(Imm);}]>;

def to_lower_imm : SDNodeXForm<imm, [{ return CurDAG->getTargetConstant(N->getZExtValue() & 0xFF, SDLoc(N), MVT::i8); }]>;
def to_upper_imm : SDNodeXForm<imm, [{ return CurDAG->getTargetConstant((N->getZExtValue() >> 8) & 0xFF, SDLoc(N), MVT::i8); }]>;

// Combine and split
def SDT_GBCOMBINE : SDTypeProfile<1, 2, [SDTCisVT<0, i16>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def COMBINE_NODE : SDNode<"GBISD::COMBINE", SDT_GBCOMBINE, []>;

def : Pat<(COMBINE_NODE GPR8:$rs1, GPR8:$rs2),
          (REG_SEQUENCE GPR16, GPR8:$rs1, LowerSubReg, GPR8:$rs2, UpperSubReg)>;

def SDT_GBSPLIT : SDTypeProfile<1, 1, [SDTCisVT<0, i8>, SDTCisVT<1, i16>]>;
def LOWER_NODE : SDNode<"GBISD::LOWER", SDT_GBSPLIT, []>;
def UPPER_NODE : SDNode<"GBISD::UPPER", SDT_GBSPLIT, []>;

def : Pat<(LOWER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;
def : Pat<(UPPER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, UpperSubReg)>;

def : Pat<(trunc GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;

// GB-Specific DAG nodes
def RET_NODE : SDNode<"GBISD::RET", SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def : Pat<(RET_NODE), (RET)>;

//   ADDR_WRAPPER exists to match targetaddr against i16
def SDT_GBAddrWrapper : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisPtrTy<0>]>;
def ADDR_WRAPPER : SDNode<"GBISD::ADDR_WRAPPER", SDT_GBAddrWrapper>;

//   CP operands: 0) condition code, 1) LHS, 2) RHS
def SDT_GBCP : SDTypeProfile<0, 3, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def CP_NODE : SDNode<"GBISD::CP", SDT_GBCP, [SDNPOutGlue]>;
def : Pat<(CP_NODE SETUGT, GPR8:$rs, A), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETUGE, A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETULT, A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETULE, GPR8:$rs, A), (CP_r GPR8:$rs)>;

def : Pat<(CP_NODE SETEQ,  A, GPR8:$rs), (CP_r GPR8:$rs)>;
def : Pat<(CP_NODE SETNE,  A, GPR8:$rs), (CP_r GPR8:$rs)>;

def : Pat<(CP_NODE SETUGE, A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETULT, A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETEQ,  A, imm8:$imm8), (CPI imm8:$imm8)>;
def : Pat<(CP_NODE SETNE,  A, imm8:$imm8), (CPI imm8:$imm8)>;

// Rotates the carry into A, used as our setcc
def SDT_GB1i8 : SDTypeProfile<0, 1, [SDTCisVT<0, i8>]>;
def RLA_NODE : SDNode<"GBISD::RLA", SDT_GB1i8, [SDNPInGlue]>;
def : Pat<(RLA_NODE A), (RLA)>;

// Rotates A, useful for %i1 = sign(a)
def RLCA_NODE : SDNode<"GBISD::RLCA", SDT_GB1i8, []>;
def : Pat<(RLCA_NODE A), (RLCA)>;

//   BR_CC operands: 0) condition code, 1) basic block addr
// TODO GB: optimize into JR in another pass
def SDTGBBR_CC  : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, OtherVT>]>;
def BR_CC_NODE : SDNode<"GBISD::BR_CC", SDTGBBR_CC, [SDNPInGlue, SDNPHasChain]>;
def : Pat<(BR_CC_NODE SETUGT, bb:$imm), (JP_COND GBFLAG_C.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETUGE, bb:$imm), (JP_COND GBFLAG_NC.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETULT, bb:$imm), (JP_COND GBFLAG_C.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETULE, bb:$imm), (JP_COND GBFLAG_NC.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETEQ,  bb:$imm), (JP_COND GBFLAG_Z.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETNE,  bb:$imm), (JP_COND GBFLAG_NZ.Encoding, bb:$imm)>;

// Constants for pattern matching
def : Pat<(imm8:$imm), (LDI8_r imm8:$imm)>;
def : Pat<(imm16:$imm), (LDI16 imm16:$imm)>;
def : Pat<(ADDR_WRAPPER tglobaladdr:$imm), (LDI16 imm16:$imm)>;
def : Pat<(ADDR_WRAPPER tblockaddress:$imm), (LDI16 imm16:$imm)>;

// Frame index.
def to_tframeindex : SDNodeXForm<frameindex, [{
  auto FI = cast<FrameIndexSDNode>(N);
  return CurDAG->getTargetFrameIndex(FI->getIndex(), TLI->getPointerTy(CurDAG->getDataLayout()));
}]>;
def : Pat<(frameindex:$imm), (LD_HL_SP (to_tframeindex $imm))>;

// ALU 8-bit
multiclass ALU_Pat<SDNode node, GBInstr immInstr, GBInstr regInstr, GBInstr indInstr> {
  def : Pat<!setdagop((? A, imm8:$imm), node), !setdagop((? imm8:$imm), immInstr)>;
  def : Pat<!setdagop((? A, GPR8:$rs),  node), !setdagop((? GPR8:$rs),  regInstr)>;
  def : Pat<!setdagop((? A, (load HL)), node), !setdagop((?),           indInstr)>;
}

defm : ALU_Pat<add,  ADDI, ADD_r, ADD_iHL>;
defm : ALU_Pat<addc, ADDI, ADD_r, ADD_iHL>;
defm : ALU_Pat<adde, ADCI, ADC_r, ADC_iHL>;

defm : ALU_Pat<sub,  SUBI, SUB_r, SUB_iHL>;
defm : ALU_Pat<subc, SUBI, SUB_r, SUB_iHL>;
defm : ALU_Pat<sube, SBCI, SBC_r, SBC_iHL>;

defm : ALU_Pat<and, ANDI, AND_r, AND_iHL>;
defm : ALU_Pat<xor, XORI, XOR_r, XOR_iHL>;
defm : ALU_Pat<or,  ORI,  OR_r,  OR_iHL>;

def : Pat<(xor A, -1), (CPL)>;

// ALU 16-bit (currently inactive)
def : Pat<(add HL, GPR16:$rs), (ADD_HL GPR16:$rs)>;
def : Pat<(add GPR16:$rs, 1), (INC16 GPR16:$rs)>;
def : Pat<(add GPR16:$rs, -1), (DEC16 GPR16:$rs)>;

// Branches
// TODO GB: optimize into JR in another pass
def : Pat<(br bb:$imm), (JP bb_pc_rel:$imm)>;
def : Pat<(brind HL), (JP_HL)>;

// Memory
def : Pat<(load HL), (LD_r_iHL)>;
def : Pat<(extload HL), (LD_r_iHL)>;
def : Pat<(store GPR8:$rs, HL), (LD_iHL_r GPR8:$rs)>;
def : Pat<(store imm8:$imm, HL), (LDI8_iHL imm8:$imm)>;

// Required when passing arguments on the stack
def SDT_GBLDHLSP : SDTypeProfile<1, 1, [SDTCisVT<0, i16>, SDTCisVT<1, i8>]>;
def LD_HL_SP_NODE : SDNode<"GBISD::LD_HL_SP", SDT_GBLDHLSP, []>;
def : Pat<(LD_HL_SP_NODE imm8:$imm), (LD_HL_SP imm8:$imm)>;

class Pseudo<string name, string args, dag outs, dag ins>
    : GBInstr<"pseudo " # name, args, -1, outs, ins, FixupNone> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

let mayLoad = true, Uses = [SP] in {
  def Load16FromFrameIndex : Pseudo<"#load16", "$i to $r", (outs GPR16:$r), (ins simm8:$i)>;
  def Load8FromFrameIndex : Pseudo<"#load8", "$i to $r", (outs GPR8:$r), (ins simm8:$i)>;
}

let mayStore = true, Uses = [SP] in {
  def Save16ToFrameIndex : Pseudo<"#save16 ", "$r to $i", (outs), (ins GPR16:$r, simm8:$i)>;
  def Save8ToFrameIndex : Pseudo<"#save8 ", "$r to $i", (outs), (ins GPR8:$r, simm8:$i)>;
}

// Select
def P_SELECT_CC
    : Pseudo<"#select_cc", "$rd, $flag, $ifTrue, $ifFalse",
        (outs GPR8:$rd),
        (ins flag:$flag, GPR8:$ifTrue, GPR8:$ifFalse)> {
  let usesCustomInserter = true;
  let Uses = [F];
}

def SDT_GBSELECT_CC : SDTypeProfile<1, 3, [/*Out=*/SDTCisVT<0, i8>, /*CCODE=*/ SDTCisVT<1, OtherVT>, /*IfTrue=*/ SDTCisVT<2, i8>, /*IfFalse=*/ SDTCisVT<3, i8>]>;
def SELECT_CC_NODE : SDNode<"GBISD::SELECT_CC", SDT_GBSELECT_CC, [SDNPInGlue]>;
def : Pat<(SELECT_CC_NODE SETUGT, GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_C.Encoding,  GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETUGE, GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_NC.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETULT, GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_C.Encoding,  GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETULE, GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_NC.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETEQ,  GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_Z.Encoding,  GPR8:$ifTrue, GPR8:$ifFalse)>;
def : Pat<(SELECT_CC_NODE SETNE,  GPR8:$ifTrue, GPR8:$ifFalse), (P_SELECT_CC GBFLAG_NZ.Encoding, GPR8:$ifTrue, GPR8:$ifFalse)>;

// Shifts
// TODO GB: match constant operands
class P_SHIFT<string name> : Pseudo<"#" # name, "$rd, $amount", (outs GPR8:$rd), (ins GPR8:$rs, GPR8:$amount)> {
  let Constraints = "$rs = $rd";
  let Defs = [F];
  let usesCustomInserter = true;
}

def P_SLA : P_SHIFT<"sla">;
def P_SRA : P_SHIFT<"sra">;
def P_SRL : P_SHIFT<"srl">;
def P_ROTL : P_SHIFT<"rotl">;
def P_ROTR : P_SHIFT<"rotr">;

def : Pat<(shl GPR8:$rs, GPR8:$amount),  (P_SLA GPR8:$rs, GPR8:$amount)>;
def : Pat<(sra GPR8:$rs, GPR8:$amount),  (P_SRA GPR8:$rs, GPR8:$amount)>;
def : Pat<(srl GPR8:$rs, GPR8:$amount),  (P_SRL GPR8:$rs, GPR8:$amount)>;
def : Pat<(rotl GPR8:$rs, GPR8:$amount), (P_ROTL GPR8:$rs, GPR8:$amount)>;
def : Pat<(rotr GPR8:$rs, GPR8:$amount), (P_ROTR GPR8:$rs, GPR8:$amount)>;

// Calling
def SDT_GBCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_GBCallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_GBCall : SDTypeProfile<0, -1, [SDTCisVT<0, i16>]>;
def GBcallseq_start : SDNode<"ISD::CALLSEQ_START", SDT_GBCallSeqStart,
                             [SDNPHasChain, SDNPOutGlue]>;
def GBcallseq_end : SDNode<"ISD::CALLSEQ_END", SDT_GBCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def GBcall : SDNode<"GBISD::CALL", SDT_GBCall,
                     [SDNPMayStore, SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;

let Uses = [SP], Defs = [SP, F] in {
  def ADJCALLSTACKDOWN : Pseudo<"adjcallstackdown", "", (outs), (ins uimm16:$imm, uimm16:$out)>;
  def ADJCALLSTACKUP : Pseudo<"adjcallstackup", "", (outs), (ins uimm16:$imm, uimm16:$out)>;
}

def : Pat<(GBcallseq_start timm:$imm, timm:$out), (ADJCALLSTACKDOWN imm16:$imm, imm16:$out)>;
def : Pat<(GBcallseq_end timm:$imm, timm:$out), (ADJCALLSTACKUP imm16:$imm, imm16:$out)>;
def : Pat<(GBcall imm16:$imm), (CALL imm16:$imm)>;
def : Pat<(GBcall (ADDR_WRAPPER tglobaladdr:$imm)), (CALL imm16:$imm)>;

// Special instructions
def : Pat<(trap), (TRAP)>;

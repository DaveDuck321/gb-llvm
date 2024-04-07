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

  defm ADC : _GB_GPR8S1Sequence<"adc", 0x88, (ins)>;
  defm SUB : _GB_GPR8S1Sequence<"sub", 0x90, (ins)>;
  defm SBC : _GB_GPR8S1Sequence<"sbc", 0x98, (ins)>;
  defm AND : _GB_GPR8S1Sequence<"and", 0xA0, (ins)>;
  defm XOR : _GB_GPR8S1Sequence<"xor", 0xA8, (ins)>;
  defm OR  : _GB_GPR8S1Sequence<"or",  0xB0, (ins)>;

  let Uses = [A], Defs = [F], isCompare = true in
  defm CP  : _GB_GPR8S1Sequence<"cp",  0xB8, (ins)>;
}

let Uses = [A], Defs = [A, F] in {
  def ADDI : _GB_SimpleImm8<"add $imm", 0xC6, uimm8, FixupData8>;
  def ADCI : _GB_SimpleImm8<"adc $imm", 0xCE, uimm8, FixupData8>;
  def SUBI : _GB_SimpleImm8<"sub $imm", 0xD6, uimm8, FixupData8>;
  def SBCI : _GB_SimpleImm8<"sbc $imm", 0xDE, uimm8, FixupData8>;
  def ANDI : _GB_SimpleImm8<"and $imm", 0xE6, uimm8, FixupData8>;
  def XORI : _GB_SimpleImm8<"xor $imm", 0xEE, uimm8, FixupData8>;
  def ORI  : _GB_SimpleImm8<"or $imm",  0xF6, uimm8, FixupData8>;
  def CPI  : _GB_SimpleImm8<"cp $imm",  0xFE, uimm8, FixupData8>;
}

let Size = 1 in {
  defm INC : _GB_GPR8S8Sequence<"inc", 0x04, (ins)>;
  defm DEC : _GB_GPR8S8Sequence<"dec", 0x05, (ins)>;
}

let Size = 1 in {
  // TOOD GB: there's a hardware bug here... maybe find a way to model this?
  def INC16 : _GB_GPR16S16Sequence<"inc", 0x03, (ins)>;
  def DEC16 : _GB_GPR16S16Sequence<"dec", 0x0B, (ins)>;

  let isAdd = true, Uses = [HL], Defs = [HL, F] in
  def ADD_HL : _GB_GPR16S16Sequence<"add hl,", 0x09, (ins)>;
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
  defm RL   : _GB_CBGPR8S1<"rl", 0x10>;
  defm RR   : _GB_CBGPR8S1<"rr", 0x18>;
  defm SLA  : _GB_CBGPR8S1<"sla", 0x20>;
  defm SRA  : _GB_CBGPR8S1<"sra", 0x28>;
  defm SWAP : _GB_CBGPR8S1<"swap", 0x30>;
  defm SRL  : _GB_CBGPR8S1<"srl", 0x38>;
  defm BIT : GBCBGPR8S1Imm3<"bit", 0x40>;
}

defm RES : GBCBGPR8S1Imm3<"res", 0x80>;
defm SET : GBCBGPR8S1Imm3<"set", 0xC0>;

let isTerminator = true, isBranch = true, isBarrier = true in {
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
  def CALL : GBAbsImm16<"call $imm", 0xCD>;

  let Uses = [F, SP], Defs = [SP] in
  def CALL_COND : GBAbsFlagImm16<"call", 0xC4>;
}

let isTerminator = true in {
  def JP : GBAbsImm16<"jp $imm", 0xC3>;

  let isBranch = true, Uses = [F] in
  def JP_COND : GBAbsFlagImm16<"jp", 0xC2>;

  let isIndirectBranch = true, Uses = [HL] in
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
  def EI   : GBInstrNoOperands<"ei", 0xFb>;
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

// GB-Specific DAG nodes
def RET_NODE : SDNode<"GBISD::RET", SDTNone, [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def : Pat<(RET_NODE), (RET)>;

//   ADDR_WRAPPER exists to match targetaddr against i16
def SDTGBAddrWrapper : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisPtrTy<0>]>;
def ADDR_WRAPPER : SDNode<"GBISD::ADDR_WRAPPER", SDTGBAddrWrapper>;

//   CP operands: 0) condition code, 1) LHS, 2) RHS
def SDTGBCP : SDTypeProfile<0, 3, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def CP_NODE : SDNode<"GBISD::CP", SDTGBCP, [SDNPOutGlue]>;
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

def : Pat<(CP_NODE SETULT, GPR8:$rs, 1), (DEC_r GPR8:$rs)>;

// Rotates the carry into A, used as our setcc
def SDTGB1i8 : SDTypeProfile<0, 1, [SDTCisVT<0, i8>]>;
def RLA_NODE : SDNode<"GBISD::RLA", SDTGB1i8, [SDNPInGlue]>;
def : Pat<(RLA_NODE A), (RLA)>;

// Rotates A, useful for %i1 = sign(a)
def RLCA_NODE : SDNode<"GBISD::RLCA", SDTGB1i8, []>;
def : Pat<(RLCA_NODE A), (RLCA)>;

//   BR_CC operands: 0) condition code, 1) basic block addr
// TODO GB: generate JP here and optimize into JR in another pass
def SDTGBBR_CC  : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, OtherVT>]>;
def BR_CC_NODE : SDNode<"GBISD::BR_CC", SDTGBBR_CC, [SDNPInGlue, SDNPHasChain]>;
def : Pat<(BR_CC_NODE SETUGT, bb:$imm), (JR_COND GBFLAG_C.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETUGE, bb:$imm), (JR_COND GBFLAG_NC.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETULT, bb:$imm), (JR_COND GBFLAG_C.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETULE, bb:$imm), (JR_COND GBFLAG_NC.Encoding, bb:$imm)>;
def : Pat<(BR_CC_NODE SETEQ,  bb:$imm), (JR_COND GBFLAG_Z.Encoding,  bb:$imm)>;
def : Pat<(BR_CC_NODE SETNE,  bb:$imm), (JR_COND GBFLAG_NZ.Encoding, bb:$imm)>;

// Constants for pattern matching
def : Pat<(imm8:$imm), (LDI8_r imm8:$imm)>;
def : Pat<(imm16:$imm), (LDI16 imm16:$imm)>;
def : Pat<(ADDR_WRAPPER tglobaladdr:$imm), (LDI16 imm16:$imm)>;
def : Pat<(ADDR_WRAPPER tblockaddress:$imm), (LDI16 imm16:$imm)>;

// ALU 8-bit
def : Pat<(add A, imm8:$imm), (ADDI imm8:$imm)>;
def : Pat<(add A, GPR8:$rs), (ADD_r GPR8:$rs)>;
def : Pat<(add A, (load HL)), (ADD_iHL)>;

def : Pat<(sub A, imm8:$imm), (SUBI imm8:$imm)>;
def : Pat<(sub A, GPR8:$rs), (SUB_r GPR8:$rs)>;
def : Pat<(sub A, (load HL)), (SUB_iHL)>;

def : Pat<(and A, imm8:$imm), (ANDI imm8:$imm)>;
def : Pat<(and A, GPR8:$rs), (AND_r GPR8:$rs)>;
def : Pat<(and A, (load HL)), (AND_iHL)>;

def : Pat<(xor A, imm8:$imm), (XORI imm8:$imm)>;
def : Pat<(xor A, GPR8:$rs), (XOR_r GPR8:$rs)>;
def : Pat<(xor A, (load HL)), (XOR_iHL)>;

def : Pat<(or A, imm8:$imm), (ORI imm8:$imm)>;
def : Pat<(or A, GPR8:$rs), (OR_r GPR8:$rs)>;
def : Pat<(or A, (load HL)), (OR_iHL)>;

// ALU 16-bit
def : Pat<(add HL, GPR16:$rs), (ADD_HL GPR16:$rs)>;

// Branches
// TODO GB: generate JP here and optimize into JR in another pass
def : Pat<(br bb:$imm), (JR bb_pc_rel:$imm)>;
def : Pat<(brind HL), (JP_HL)>;

// Memory
def : Pat<(load HL), (LD_r_iHL)>;
def : Pat<(store GPR8:$rs, HL), (LD_iHL_r GPR8:$rs)>;


// Pseudos hack an extra operand for frame lowering
class Pseudo<string name, string args, dag outs, dag ins>
    : GBInstr<"pseudo " # name, args, -1, outs, ins, FixupNone> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

def Save16ToFrameIndex : Pseudo<"save16 ", "$r to $i", (outs), (ins GPR16:$r, simm8:$i)> {
    let mayStore = true;
}

def Load16FromFrameIndex : Pseudo<"load16", "$i to $r", (outs GPR16:$r), (ins simm8:$i)> {
    let mayLoad = true;
}

def Save8ToFrameIndex : Pseudo<"save8 ", "$r to $i", (outs), (ins GPR8:$r, simm8:$i)> {
    let mayStore = true;
}

def Load8FromFrameIndex : Pseudo<"load8", "$i to $r", (outs GPR8:$r), (ins simm8:$i)> {
    let mayLoad = true;
}


// Combine and split
def SDTGBCOMBINE : SDTypeProfile<1, 2, [SDTCisVT<0, i16>, SDTCisVT<1, i8>, SDTCisSameAs<1, 2>]>;
def COMBINE_NODE : SDNode<"GBISD::COMBINE", SDTGBCOMBINE, [SDNPHasChain]>;

def : Pat<(COMBINE_NODE GPR8:$rs1, GPR8:$rs2),
          (REG_SEQUENCE GPR16, GPR8:$rs1, LowerSubReg, GPR8:$rs2, UpperSubReg)>;

def SDTGBSPLIT : SDTypeProfile<1, 1, [SDTCisVT<0, i8>, SDTCisVT<1, i16>]>;
def LOWER_NODE : SDNode<"GBISD::LOWER", SDTGBSPLIT, []>;
def UPPER_NODE : SDNode<"GBISD::UPPER", SDTGBSPLIT, []>;

def : Pat<(LOWER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, LowerSubReg)>;
def : Pat<(UPPER_NODE GPR16:$rs), (EXTRACT_SUBREG GPR16:$rs, UpperSubReg)>;

include "GBInstrFormats.td"

def NOP : GBInstrNoOperands<"nop", 0x00, []>;

defm LD : GBGPR8LD<"ld", 0x40, []>;
defm LDI8 : GBGPR8LDI8<"ld", 0x06, []>;
def LDI16 : GBGPR8LDI16<"ld", 0x01, []>;

let mayStore = true, Uses = [A] in
def LDH_iImm_A : _GB_SimpleImm8<"ldh ($imm), a", 0xE0, [], uimm8, FixupFFXX>;

let mayLoad = true, Defs = [A] in
def LDH_A_iImm : _GB_SimpleImm8<"ldh a, ($imm)", 0xF0, [], uimm8, FixupFFXX>;

let isMoveReg = true in {
  let Uses = [HL], Defs = [SP] in
  def LD_SP_HL : GBInstrNoOperands<"ld sp, hl", 0xF9, []>;

  let Uses = [SP], Defs = [HL, F] in
  def LD_HL_SP : _GB_SimpleImm8<"ld hl, sp, $imm", 0xF8, [], simm8, FixupData8>;
}

let mayStore = true, Uses = [SP] in
def LD_iImm16_SP : _GB_SimpleImm16<"ld ($imm), sp", 0x08, []>;

let Uses = [A], Defs = [A, F], Size = 1 in {
  // TODO: be more specific about which flags are clobbered
  let isAdd = true in
  defm ADD : _GB_GPR8S1Sequence<"add", 0x80, (ins), []>;

  defm ADC : _GB_GPR8S1Sequence<"adc", 0x88, (ins), []>;
  defm SUB : _GB_GPR8S1Sequence<"sub", 0x90, (ins), []>;
  defm SBC : _GB_GPR8S1Sequence<"sbc", 0x98, (ins), []>;
  defm AND : _GB_GPR8S1Sequence<"and", 0xA0, (ins), []>;
  defm XOR : _GB_GPR8S1Sequence<"xor", 0xA8, (ins), []>;
  defm OR  : _GB_GPR8S1Sequence<"or",  0xB0, (ins), []>;
  defm CP  : _GB_GPR8S1Sequence<"cp",  0xB8, (ins), []>;
}

let Uses = [A], Defs = [A, F] in {
  def ADDI : _GB_SimpleImm8<"add $imm", 0xC6, [], dimm8, FixupData8>;
  def ADCI : _GB_SimpleImm8<"adc $imm", 0xCE, [], dimm8, FixupData8>;
  def SUBI : _GB_SimpleImm8<"sub $imm", 0xD6, [], dimm8, FixupData8>;
  def SBCI : _GB_SimpleImm8<"sbc $imm", 0xDE, [], dimm8, FixupData8>;
  def ANDI : _GB_SimpleImm8<"and $imm", 0xE6, [], dimm8, FixupData8>;
  def XORI : _GB_SimpleImm8<"xor $imm", 0xEE, [], dimm8, FixupData8>;
  def ORI  : _GB_SimpleImm8<"or $imm",  0xF6, [], dimm8, FixupData8>;
  def CPI  : _GB_SimpleImm8<"cp $imm",  0xFE, [], dimm8, FixupData8>;
}

let Uses = [A], Defs = [A, F], Size = 1 in {
  defm INC : _GB_GPR8S8Sequence<"inc", 0x04, (ins), []>;
  defm DEC : _GB_GPR8S8Sequence<"dec", 0x05, (ins), []>;
}

let Size = 1 in {
  // TOOD GB: there's a hardware bug here... maybe find a way to model this?
  def INC16 : _GB_GPR16S16Sequence<"inc", 0x03, (ins), []>;
  def DEC16 : _GB_GPR16S16Sequence<"dec", 0x0B, (ins), []>;

  let isAdd = true, Uses = [HL], Defs = [HL, F] in
  def ADD_HL : _GB_GPR16S16Sequence<"add hl,", 0x09, (ins), []>;
}

let isAdd = true, Uses = [SP], Defs = [F] in
def ADD_SP : _GB_SimpleImm8<"add sp, $imm", 0xe8, [], simm8, FixupData8>;

let Uses = [A], Defs = [A, F] in {
  def RLCA : GBInstrNoOperands<"rlca", 0x07, []>;
  def RLA  : GBInstrNoOperands<"rla", 0x17, []>;
  def RRCA : GBInstrNoOperands<"rrca", 0x0F, []>;
  def RRA  : GBInstrNoOperands<"rra", 0x1F, []>;
  def CPL  : GBInstrNoOperands<"cpl", 0x2F, []>;
}

let Uses = [A, F], Defs = [A, F] in {
  def DAA  : GBInstrNoOperands<"daa", 0x27, []>;
}

let Defs = [F] in {
  def SCF  : GBInstrNoOperands<"scf", 0x37, []>;
  def CCF  : GBInstrNoOperands<"ccf", 0x3F, []>;
}

// // CB instructions
let Defs = [F] in {
  defm RLC  : _GB_CBGPR8S1<"rlc", 0x00, []>;
  defm RRC  : _GB_CBGPR8S1<"rrc", 0x08, []>;
  defm RL   : _GB_CBGPR8S1<"rl", 0x10, []>;
  defm RR   : _GB_CBGPR8S1<"rr", 0x18, []>;
  defm SLA  : _GB_CBGPR8S1<"sla", 0x20, []>;
  defm SRA  : _GB_CBGPR8S1<"sra", 0x28, []>;
  defm SWAP : _GB_CBGPR8S1<"swap", 0x30, []>;
  defm SRL  : _GB_CBGPR8S1<"srl", 0x38, []>;
  defm BIT : GBCBGPR8S1Imm3<"bit", 0x40, []>;
}

defm RES : GBCBGPR8S1Imm3<"res", 0x80, []>;
defm SET : GBCBGPR8S1Imm3<"set", 0xC0, []>;

let isTerminator = true in {
  def JR : _GB_SimpleImm8<"jr $imm", 0x18, [], simm8, FixupPCRel8>;

  let Uses = [F] in
  def JR_FLAG : _GB_FlagImm8<"jr", 0x20, []>;
}

let mayLoad = true, isReturn = true, isTerminator = true in {
  let Uses = [SP], Defs = [SP] in {
    def RET : GBInstrNoOperands<"ret", 0xC9, []>;

    let hasSideEffects = true in
    def RETI : GBInstrNoOperands<"reti", 0xD9, []>;
  }

  let Size = 1, Uses = [F, SP], Defs = [SP] in
  def RET_FLAG : _GB_FlagSequence<"ret", "", 0xC0, (ins), [], FixupNone>;
}

let mayStore = true, isCall = true in {
  let Uses = [SP], Defs = [SP] in
  def CALL : _GB_SimpleImm16<"call $imm", 0xCD, []>;

  let Uses = [F, SP], Defs = [SP] in
  def CALL_FLAG : _GB_FlagImm16<"call", 0xC4, []>;
}

let isTerminator = true in {
  def JP : _GB_SimpleImm16<"jp $imm", 0xC3, []>;

  let isBranch = true, Uses = [F] in
  def JP_FLAG : _GB_FlagImm16<"jp", 0xC2, []>;

  let mayLoad = true, isIndirectBranch = true, Uses = [HL] in
  def JP_iHL : GBInstrNoOperands<"jp (hl)", 0xE9, []>;
}

let hasSideEffects = true in {
  // TODO GB: add STOP 0 as a PSEUDO instruction
  // NOTE: stop is acually 2 bytes -- officially STOP 0 ...BUT...
  // the gameboy sometimes interprets it as a 1 byte instruction.
  // Lets just interpret that pattern as STOP NOP
  def STOP : GBInstrNoOperands<"stop", 0x10, []>;
  def HALT : GBInstrNoOperands<"halt", 0x76, []>;
  def DI   : GBInstrNoOperands<"di", 0xF3, []>;
  def EI   : GBInstrNoOperands<"ei", 0xFb, []>;
}

def POP  : GBPushPop16<"pop", 0xC1, (outs SR16:$reg), (ins), []>;
def PUSH : GBPushPop16<"push", 0xC5, (outs), (ins SR16:$reg), []>;

// 8-bit ALU

multiclass GBInstrALU<string mnemonic, bits<8> opcode> {
  def _r : GBInstr<mnemonic#" $rs", opcode, (outs), (ins GPR8:$rs)> {
    list<Register> ExtraUses = [];

    bits<3> rs;
    let Inst{2 - 0} = rs;
    let Uses = !listconcat([A], ExtraUses);
    let Defs = [A, F];
  }

  def _iHL : GBInstr<mnemonic#" (hl)", opcode, (outs), (ins)> {
    list<Register> ExtraUses = [];

    let Inst{2 - 0} = 0b110;
    let mayLoad = true;
    let Uses = !listconcat([A, HL], ExtraUses);
    let Defs = [A, F];
  }

  def I : GBInstrImm8<mnemonic#" $imm", opcode, (outs), (ins uimm8:$imm),
                      FixupData8> {
    list<Register> ExtraUses = [];

    let Inst{2 - 0} = 0b110;
    let Inst{6} = 1;
    let Uses = !listconcat([A], ExtraUses);
    let Defs = [A, F];
  }
}

let isAdd = true in defm ADD : GBInstrALU<"add", 0x80>;
defm SUB : GBInstrALU<"sub", 0x90>;
defm AND : GBInstrALU<"and", 0xA0>;
defm XOR : GBInstrALU<"xor", 0xA8>;
defm OR : GBInstrALU<"or", 0xB0>;

// Unlike all other arithmetic ops, CP does not define A
let Defs = [F], isCompare = true in defm CP : GBInstrALU<"cp", 0xB8>;

let ExtraUses = [F] in {
  defm ADC : GBInstrALU<"adc", 0x88>;
  defm SBC : GBInstrALU<"sbc", 0x98>;
}

multiclass GBInstrIncDec8<string mnemonic, bits<8> opcode> {
  def _r : GBInstr<mnemonic#" $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs)> {
    bits<3> rs;
    let Inst{5 - 3} = rs;
    let Defs = [F];
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBInstr<mnemonic#" (hl)", opcode, (outs), (ins)> {
    let Inst{5 - 3} = 0b110;
    let mayLoad = true;
    let mayStore = true;
    let Uses = [HL];
    let Defs = [F];
  }
}

defm INC : GBInstrIncDec8<"inc", 0x04>;
defm DEC : GBInstrIncDec8<"dec", 0x05>;

// Accumulator bit manipulation
let Uses = [A], Defs = [A, F] in {
  def RLCA : GBInstrNoOperands<"rlca", 0x07>;
  def RRCA : GBInstrNoOperands<"rrca", 0x0F>;
  def CPL : GBInstrNoOperands<"cpl", 0x2F>;
}

let Uses = [A, F], Defs = [A, F] in {
  def RLA : GBInstrNoOperands<"rla", 0x17>;
  def RRA : GBInstrNoOperands<"rra", 0x1F>;
  def DAA : GBInstrNoOperands<"daa", 0x27>;
}

let Defs = [F] in {
  def SCF : GBInstrNoOperands<"scf", 0x37>;
  def CCF : GBInstrNoOperands<"ccf", 0x3F>;
}

// 16-bit ALU

multiclass GBInstrIncDec16<string mnemonic, bits<8> opcode> {
  def "" : GBInstr<mnemonic#" $rs", opcode, (outs GPR16:$rd), (ins GPR16:$rs)> {
    bits<2> rs;
    let Inst{5 - 4} = rs;
    let Constraints = "$rs = $rd";
  }

  // SP gets its own instruction so we can use the GPR16 register class
  def _SP : GBInstr<mnemonic#" sp", opcode, (outs), (ins)> {
    let Inst{5 - 4} = 0b11;
    let Uses = [SP];
    let Defs = [SP];
  }
}

defm INC16 : GBInstrIncDec16<"inc", 0x03>;
defm DEC16 : GBInstrIncDec16<"dec", 0x0B>;

def ADD_HL : GBInstr<"add hl, $rs", 0x09, (outs), (ins GPR16AndSP:$rs)> {
  bits<2> rs;
  let Inst{5 - 4} = rs;
  let Uses = [HL];
  let Defs = [HL, F];

  let isAdd = true;
}

def ADD_SP
    : GBInstrImm8<"add sp, $imm", 0xe8, (outs), (ins simm8:$imm), FixupData8> {
  let Uses = [SP];
  let Defs = [SP, F];
  let isAdd = true;
}

// CB instructions

multiclass GBInstrCBNoImmediate<string mnemonic, bits<8> opcode> {
  def _r : GBInstrCB<mnemonic#" $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs)> {
    list<Register> ExtraUses = [];

    bits<3> rs;
    let Inst{10 - 8} = rs;
    let Uses = ExtraUses;
    let Defs = [F];
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBInstrCB<mnemonic#" (hl)", opcode, (outs), (ins)> {
    list<Register> ExtraUses = [];

    let Inst{10 - 8} = 0b110;
    let Uses = !listconcat([HL], ExtraUses);
    let Defs = [F];

    let mayLoad = true;
    let mayStore = true;
  }
}

defm RLC : GBInstrCBNoImmediate<"rlc", 0x00>;
defm RRC : GBInstrCBNoImmediate<"rrc", 0x08>;
defm SLA : GBInstrCBNoImmediate<"sla", 0x20>;
defm SRA : GBInstrCBNoImmediate<"sra", 0x28>;
defm SWAP : GBInstrCBNoImmediate<"swap", 0x30>;
defm SRL : GBInstrCBNoImmediate<"srl", 0x38>;

let ExtraUses = [F] in {
  defm RL : GBInstrCBNoImmediate<"rl", 0x10>;
  defm RR : GBInstrCBNoImmediate<"rr", 0x18>;
}

multiclass GBInstrCBImmediate<string mnemonic, bits<8> opcode> {
  def _r : GBInstrCB<mnemonic#" $imm, $rs", opcode, (outs GPR8:$rd),
                     (ins GPR8:$rs, uimm3:$imm)> {
    bits<3> rs;
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = rs;
    let Constraints = "$rs = $rd";
  }

  def _iHL
      : GBInstrCB<mnemonic#" $imm, (hl)", opcode, (outs), (ins uimm3:$imm)> {
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = 0b110;

    let Uses = [HL];
    let mayLoad = true;
    let mayStore = true;
  }
}

defm RES : GBInstrCBImmediate<"res", 0x80>;
defm SET : GBInstrCBImmediate<"set", 0xC0>;

def BIT_r
    : GBInstrCB<"bit $imm, $rs", 0x40, (outs), (ins GPR8:$rs, uimm3:$imm)> {
  bits<3> rs;
  bits<3> imm;
  let Inst{13 - 11} = imm;
  let Inst{10 - 8} = rs;

  let Defs = [F];
}

def BIT_iHL : GBInstrCB<"bit $imm, (hl)", 0x40, (outs), (ins uimm3:$imm)> {
  bits<3> imm;
  let Inst{13 - 11} = imm;
  let Inst{10 - 8} = 0b110;

  let Uses = [HL];
  let Defs = [F];
  let mayLoad = true;
}

// Pseudos
multiclass GBPseudoShiftN<SDNode node, string name> {
  def _r : Pseudo<name#" $rd, $amount", (outs GPR8:$rd),
                  (ins GPR8:$rs, GPR8:$amount)> {
    let Defs = [F];
    let Constraints = "$rs = $rd";

    let usesCustomInserter = true;
    let Pattern = [(set GPR8:$rd, !setdagop((? GPR8:$rs, GPR8:$amount), node))];
  }

  def I : Pseudo<name#"$rd, $amount", (outs GPR8:$rd),
                 (ins GPR8:$rs, uimm8:$amount)> {
    let Defs = [F];
    let Constraints = "$rs = $rd";

    let usesCustomInserter = true;
    let Pattern = [(set GPR8:$rd, !setdagop((? GPR8:$rs, imm8:$amount), node))];
  }
}

defm SLA_N : GBPseudoShiftN<shl, "sla">;
defm SRA_N : GBPseudoShiftN<sra, "sra">;
defm SRL_N : GBPseudoShiftN<srl, "srl">;
defm ROTL_N : GBPseudoShiftN<rotl, "rotl">;
defm ROTR_N : GBPseudoShiftN<rotr, "rotr">;

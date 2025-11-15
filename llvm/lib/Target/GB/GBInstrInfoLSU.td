// LD
def LD_rr : GBInstr<"ld $rd, $rs", 0x40, (outs GPR8:$rd), (ins GPR8:$rs)> {
  bits<3> rd;
  bits<3> rs;
  let Inst{5 - 3} = rd;
  let Inst{2 - 0} = rs;
  let isMoveReg = true;
}
def LD_r_iHL : GBInstr<"ld $rd, (hl)", 0x40, (outs GPR8:$rd), (ins)> {
  bits<3> rd;
  let Inst{5 - 3} = rd;
  let Inst{2 - 0} = 0b110;
  let Uses = [HL];
  let mayLoad = true;
}
def LD_iHL_r : GBInstr<"ld (hl), $rs", 0x40, (outs), (ins GPR8:$rs)> {
  bits<3> rs;
  let Inst{5 - 3} = 0b110;
  let Inst{2 - 0} = rs;
  let Uses = [HL];
  let mayStore = true;
}

// LDI8
def LDI8_r : GBInstrImm8<"ld $rd, $imm", 0x06, (outs GPR8:$rd),
                         (ins uimm8:$imm), FixupData8> {
  bits<3> rd;
  let Inst{5 - 3} = rd;
  let isReMaterializable = true;
  let isMoveImm = true;
}

def LDI8_iHL
    : GBInstrImm8<"ld (hl), $imm", 0x06, (outs), (ins uimm8:$imm), FixupData8> {
  let Inst{5 - 3} = 0b110;
  let Uses = [HL];
  let mayStore = true;
}

// LDI16
def LDI16 : GBInstrImm16<"ld $rd, $imm", 0x01, (outs GPR16AndSP:$rd),
                         (ins uimm16:$imm)> {
  bits<2> rd;
  let Inst{23 - 8} = imm;
  let Inst{5 - 4} = rd;
  let isMoveImm = true;
  let isReMaterializable = true;
}

// LD_iX_A
let mayStore = true, Uses = [A] in {
  def LD_iImm_A : GBInstrImm16<"ld ($imm), a", 0xEA, (outs), (ins uimm16:$imm)>;

  def LDH_iImm_A : GBInstrImm8<"ldh ($imm), a", 0xE0, (outs), (ins uimm8:$imm),
                               FixupData8>;

  def LD_iGPR16WithoutHL_A
      : GBInstr<"ld ($rs), a", 0x02, (outs), (ins GPR16WithoutHL:$rs)> {
    bits<1> rs;
    let Inst{4} = rs;
  }
}

def LD_iC_A : GBInstrNoOperands<"ld (c), a", 0xE2> {
  let mayStore = true;
  let Uses = [A, C];
}

// LD_A_iX
let mayLoad = true, Defs = [A] in {
  def LD_A_iImm : GBInstrImm16<"ld a, ($imm)", 0xFA, (outs), (ins uimm16:$imm)>;

  def LDH_A_iImm : GBInstrImm8<"ldh a, ($imm)", 0xF0, (outs), (ins uimm8:$imm),
                               FixupData8>;

  def LD_A_iGPR16WithoutHL
      : GBInstr<"ld a, ($rs)", 0x0A, (outs), (ins GPR16WithoutHL:$rs)> {
    bits<1> rs;
    let Inst{4} = rs;
  }
}

def LD_A_iC : GBInstrNoOperands<"ld a, (c)", 0xF2> {
  let mayLoad = true;
  let Uses = [C];
  let Defs = [A];
}

// LDI/ LDD
let mayLoad = true, Uses = [HL], Defs = [A, HL] in {
  def LDI_A_iHL : GBInstrNoOperands<"ldi a, (hl)", 0x2A>;
  def LDD_A_iHL : GBInstrNoOperands<"ldd a, (hl)", 0x3A>;
}

let mayStore = true, Uses = [A, HL], Defs = [HL] in {
  def LDI_iHL_A : GBInstrNoOperands<"ldi (hl), a", 0x22>;
  def LDD_iHL_A : GBInstrNoOperands<"ldd (hl), a", 0x32>;
}

// LD SP
def LD_SP_HL : GBInstrNoOperands<"ld sp, hl", 0xF9> {
  let Uses = [HL];
  let Defs = [SP];
  let isMoveReg = true;
}

def LD_HL_SP : GBInstrImm8<"ld hl, sp, $imm", 0xF8, (outs), (ins simm8:$imm),
                           FixupData8> {
  let Uses = [SP];
  let Defs = [HL, F];
  let isMoveReg = true;
  let isReMaterializable = true;
}

def LD_iImm16_SP
    : GBInstrImm16<"ld ($imm), sp", 0x08, (outs), (ins uimm16:$imm)> {
  let Uses = [SP];
  let mayStore = true;
}

// Pseudos

// Decay into either L_A_iHL or LD_A_iGPR16WithoutHL after register allocation
def LD_r_iGPR16 : Pseudo<"ld $rd ($rs)", (outs GPR8:$rd), (ins GPR16:$rs)> {
  let Defs = [A];
  let mayLoad = true;
  let canFoldAsLoad = true;
}

def LD_iGPR16_A : Pseudo<"ld ($rs) a", (outs), (ins GPR16:$rs)> {
  let Uses = [A];
  let mayStore = true;
}

// Decay into a push/ pop sequence (or LD_SP_HL)
let mayLoad = true, Uses = [SP] in {
  def Load16FromFrameIndex
      : Pseudo<"load16 $i to $r", (outs GPR16:$r), (ins simm8:$i)>;
  def Load8FromFrameIndex
      : Pseudo<"load8 $i to $r", (outs GPR8:$r), (ins simm8:$i)>;
}

let mayStore = true, Uses = [SP] in {
  def Save16ToFrameIndex
      : Pseudo<"save16 $r to $i", (outs), (ins GPR16:$r, simm8:$i)>;
  def Save8ToFrameIndex
      : Pseudo<"save8 $r to $i", (outs), (ins GPR8:$r, simm8:$i)>;
}

class GBInstFlagImm8<string mnemonic, bits<8> opcode>
    : GBInstrImm8<mnemonic#" $flag, $imm", opcode, (outs),
                  (ins flag:$flag, bb_pc_rel:$imm), FixupPCRel8> {
  bits<2> flag;
  let Inst{4 - 3} = flag;
  let Uses = [F];
}

class GBInstFlagImm16<string mnemonic, bits<8> opcode>
    : GBInstrImm16<mnemonic#" $flag, $imm", opcode, (outs),
                   (ins flag:$flag, bb_absolute:$imm)> {
  bits<2> flag;
  let Inst{4 - 3} = flag;
  let Uses = [F];
}

// Jump
let isTerminator = true, isBranch = true in {
  let isBarrier = true in {
    def JP : GBInstrImm16<"jp $imm", 0xC3, (outs), (ins bb_absolute:$imm)>;
    def JR : GBInstrImm8<"jr $imm", 0x18, (outs), (ins bb_pc_rel:$imm),
                         FixupPCRel8>;

    def JP_HL : GBInstrNoOperands<"jp (hl)", 0xE9> {
      let isIndirectBranch = true;
      let Uses = [HL];
    }
  }
  def JR_COND : GBInstFlagImm8<"jr", 0x20>;
  def JP_COND : GBInstFlagImm16<"jp", 0xC2>;
}

// Calls
let mayStore = true, isCall = true in {
  def CALL : GBInstrImm16<"call $imm", 0xCD, (outs), (ins uimm16:$imm)> {
    let Uses = [SP];
    let Defs = [SP];
  }

  def RST : GBInstr<"rst $vec", 0xC7, (outs), (ins rstvec:$vec)> {
    bits<3> vec;
    let Inst{5 - 3} = vec;
    let Uses = [SP];
    let Defs = [SP];
  }

  def CALL_COND : GBInstFlagImm16<"call", 0xC4> {
    let Uses = [F, SP];
    let Defs = [SP];
  }

  def CALL_HL : GBInstrNoOperands<"call (hl)", 0xC7> {
    let Uses = [HL, SP];
    let Defs = [SP];
  }
}

// Returns
let mayLoad = true, isReturn = true, isTerminator = true in {
  def RET : GBInstrNoOperands<"ret", 0xC9> {
    let Uses = [SP];
    let Defs = [SP];
  }

  def RETI : GBInstrNoOperands<"reti", 0xD9> {
    let Uses = [SP];
    let Defs = [SP];
    let hasSideEffects = true;
  }

  def RET_COND : GBInstr<"ret $flag", 0xC0, (outs), (ins flag:$flag)> {
    bits<2> flag;
    let Inst{4 - 3} = flag;
    let Uses = [F, SP];
    let Defs = [SP];
  }
}

// Pseudo
def SBR_CC_r
    : Pseudo<"sbr_cc $bb, $cond, $rs1, $rs2", (outs),
             (ins simm8:$cond, bb_absolute:$bb, GPR8:$rs1, GPR8:$rs2)> {
  let usesCustomInserter = true;
  let isTerminator = true;
  let isBranch = true;
}

def SBR_CCI
    : Pseudo<"sbr_cc $bb, $cond, $rs1, $imm", (outs),
             (ins simm8:$cond, bb_absolute:$bb, GPR8:$rs1, simm8:$imm)> {
  let usesCustomInserter = true;
  let isTerminator = true;
  let isBranch = true;
}

class GBPseudoSelectCC<string asm, dag outs, dag ins> : Pseudo<asm, outs, ins> {
  let Uses = [F];
  let usesCustomInserter = true;
  let isSelect = true;
}

def SELECT_CC : GBPseudoSelectCC<"select_cc $rd, $flag, $ifTrue, $ifFalse",
                                 (outs GPR8:$rd),
                                 (ins flag:$flag, GPR8:$ifTrue, GPR8:$ifFalse)>;

def SSELECT_CC_r
    : GBPseudoSelectCC<"sselect_cc $rd, $cond, $lhs, $rhs, $ifTrue, $ifFalse",
                       (outs GPR8:$rd),
                       (ins simm8:$cond, GPR8:$lhs, GPR8:$rhs, GPR8:$ifTrue,
                           GPR8:$ifFalse)>;

def SSELECT_CCI
    : GBPseudoSelectCC<"sselect_cc $rd, $cond, $lhs, $imm, $ifTrue, $ifFalse",
                       (outs GPR8:$rd),
                       (ins simm8:$cond, GPR8:$lhs, simm8:$imm, GPR8:$ifTrue,
                           GPR8:$ifFalse)>;

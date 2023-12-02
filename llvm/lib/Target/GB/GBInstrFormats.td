// Operand types
class GBAsmOperand<int width> : AsmOperandClass {
  let Name = "Imm" # width;
  let RenderMethod = "addImmOperands";
}

class GBOperand<int width, ValueType ty> : Operand<ty> {
  let ParserMatchClass = GBAsmOperand<width>;
}

def imm3 : GBOperand<3, i8>;
def imm8 : GBOperand<8, i8>;
def imm16 : GBOperand<16, i16>;
def addr16 : GBOperand<16, i16>;

// Flag conditionals
class GBFlagCondition<bits<8> encoding, string name> {
  string Name = name;
  bits<8> Encoding = encoding;
}

def f_NZ : GBFlagCondition<0x00, "nz">;
def f_NC : GBFlagCondition<0x10, "nc">;
def f_Z : GBFlagCondition<0x08, "z">;
def f_C : GBFlagCondition<0x18, "c">;

class FlagEncoding<bits<8> opcode, GBFlagCondition cond> {
    // TODO: could this be an immediate/ pseudo register for nicer parsing?
  bits<8> Encoding;

  let Encoding{7 - 5} = opcode{7 - 5};
  let Encoding{4 - 3} = cond.Encoding{4 - 3};
  let Encoding{2 - 0} = opcode{2 - 0};
}

// Instruction formats
class GBInstr<string opcodestr, string argstr, dag outs, dag ins, list<dag> pattern> : Instruction {
    let Namespace = "GB";

    let AsmString = opcodestr # "\t" # argstr;
    let OutOperandList = outs;
    let InOperandList = ins;
    let Pattern = pattern;
}

class GBInstr8<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern> {
    bits<8> Inst;
    let Size = 1;
}

class GBInstr16<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern> {
    bits<16> Inst;
    let Size = 2;
}

class GBInstr24<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern> {
    bits<24> Inst;
    let Size = 3;
}

class NopInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern> {
    let Inst{7 - 0} = opcode;
}

class GBCBInstr<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr16<opcodestr, argsstr, outs, ins, pattern> {
    bits<8> Arg;

    let Inst{15 - 8} = Arg{7 - 0};
    let Inst{7 - 0} = 0xCB;
}

// a = a + r
class GBArithR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rs2",
            (outs Acc:$rd),
            (ins Acc:$rs1, GPR8:$rs2),
            pattern> {
    bits<3> rs2;

    let Inst{7 - 3} = opcode{7 - 3};
    let Inst{2 - 0} = rs2;
}

// a = a + imm
class GBArithImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$imm8",
            (outs Acc:$rd),
            (ins Acc:$rs1, imm8:$imm8),
            pattern> {
    bits<8> imm8;

    let Inst{15 - 8} = imm8;
    let Inst{7 - 0} = opcode;
}

// r = r + 1
class GBArithGPR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd",
            (outs GPR8:$rs),
            (ins GPR8:$rd),
            pattern> {
    bits<3> rd;

    let Constraints = "$rd = $rs";

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = opcode{2 - 0};
}

// HL = HL + r16
class GBArithR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd, $rs2",
            (outs Ind:$rd),
            (ins Ind:$rs1, GPR16:$rs2),
            pattern> {
    bits<2> rs2;

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 4} = rs2;
    let Inst{3 - 0} = opcode{3 - 0};
}

// r16 = r16 + 1
class GBArithGPR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd",
            (outs GPR16:$rs),
            (ins GPR16:$rd),
            pattern> {
    bits<2> rd;

    let Constraints = "$rd = $rs";

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 4} = rd;
    let Inst{3 - 0} = opcode{3 - 0};
}

// rd = rs
class GBLDR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd, $rs",
            (outs GPR8:$rd),
            (ins GPR8:$rs),
            pattern> {
    bits<3> rd;
    bits<3> rs;

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = rs;
}

// rd = imm
class GBLDImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$rd, $imm8",
            (outs GPR8:$rd),
            (ins imm8:$imm8),
            pattern> {
    bits<3> rd;
    bits<8> imm8;

    let Inst{15 - 8} = imm8;
    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = opcode{2 - 0};
}

// rd16 = imm16
class GBLDR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr24<opcodestr, "$rd, $imm16",
            (outs GPR16:$rd),
            (ins imm16:$imm16),
            pattern> {
    bits<2> rd;
    bits<16> imm16;

    let Inst{23 - 8} = imm16;
    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 4} = rd;
    let Inst{3 - 0} = opcode{3 - 0};
}

// reg[bit].op()
class GBImm3Instr<bits<8> opcode, string opcodestr, dag outs, dag ins, list<dag> pattern>
  : GBCBInstr<opcodestr, "$imm3, $rs", outs, ins, pattern> {
    bits<3> rs;
    bits<8> imm3;  // Unfortunately i3 is not defined in tablegen

    let Arg{7 - 6} = opcode{7 - 6};
    let Arg{5 - 3} = imm3{2 - 0};
    let Arg{2 - 0} = rs;
}

class GBBitGetInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBImm3Instr<opcode, opcodestr, (outs), (ins GPR8:$rs, imm3:$imm3), pattern> {}

class GBBitSetInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBImm3Instr<opcode, opcodestr, (outs GPR8:$rd), (ins GPR8:$rs, imm3:$imm3), pattern> {
    let Constraints = "$rd = $rs";
}

// reg.op()
class GBShuffleRegInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBCBInstr<opcodestr, "$rs",
            (outs GPR8:$rd),
            (ins GPR8:$rs),
            pattern> {
    bits<3> rs;

    let Constraints = "$rd = $rs";
    let Arg{7 - 3} = opcode{7 - 3};
    let Arg{2 - 0} = rs;
}

// if (condition) return
class GBRetCondInstr<bits<8> opcode, string opcodestr, GBFlagCondition cond, list<dag> pattern>
  : GBInstr8<opcodestr, cond.Name, (outs), (ins), pattern> {
    let Inst{7 - 0} = FlagEncoding<opcode, cond>.Encoding{7 - 0};

    let isReturn = true;
}

// Special Gameboy-specific hardware control instructions
class GBCtlInstruction<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern> {
    let Inst{7 - 0} = opcode;

    let hasSideEffects = true;
}

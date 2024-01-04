// Fixup types
// Sync with GBFixupKinds.h
class FixupKind<bits<3> kind> { bits<3> Kind = kind; }
def FixupNone    : FixupKind<0>;
def FixupData8   : FixupKind<1>;
def FixupData16  : FixupKind<2>;
def FixupPCRel8  : FixupKind<3>;
def FixupPCRel16 : FixupKind<4>;
def FixupFFXX    : FixupKind<5>;
def FixupSPRel8  : FixupKind<6>;

// Operand types
class GBAsmImmOperand<string name> : AsmOperandClass {
  let Name = name;
  let RenderMethod = "addImmOperands";
  let DiagnosticString = name # " is invalid";
  let ParserMethod = "tryParseImmediate";
}

class GBImm<int size, string signedness, ValueType ty> : Operand<ty> {
  let PrintMethod = "printImmediateOperand";
  let ParserMatchClass = GBAsmImmOperand<signedness # "Imm" # size>;
  let DecoderMethod = "Decode" # signedness # "ImmOperand<" # size # ">";
}

def uimm3 : GBImm<3, "U", i8>;
def simm8 : GBImm<8, "S", i8>;
def uimm8 : GBImm<8, "U", i8>;
def uimm16 : GBImm<16, "U", i16>;

def GBAsmFlagOperand : AsmOperandClass {
  let Name = "Flag";
  let RenderMethod = "addFlagOperands";
  let DiagnosticString = "invalid flag";
  let ParserMethod = "tryParseFlag";
}

def flag : Operand<i2> {
  let PrintMethod = "printFlagOperand";
  let ParserMatchClass = GBAsmFlagOperand;
}

// Instruction formats
class GBInstr<string opcodestr, string argstr,
              dag outs, dag ins, list<dag> pattern,
              FixupKind fixup> : Instruction {
    let Namespace = "GB";

    let AsmString = opcodestr # "\t" # argstr;
    let OutOperandList = outs;
    let InOperandList = ins;
    let Pattern = pattern;

    let TSFlags{2 - 0} = fixup.Kind;
}

class GBInstr8<string opcodestr, string argsstr,
               dag outs, dag ins, list<dag> pattern,
               FixupKind fixup>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern, fixup> {
    bits<8> Inst;
    let Size = 1;
}

class GBInstr16<string opcodestr, string argsstr,
                dag outs, dag ins, list<dag> pattern,
                FixupKind fixup>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern, fixup> {
    bits<16> Inst;
    let Size = 2;
}

class GBInstr24<string opcodestr, string argsstr,
                dag outs, dag ins, list<dag> pattern,
                FixupKind fixup>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern, fixup> {
    bits<24> Inst;
    let Size = 3;
}

class NopInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern, FixupNone> {
    let Inst{7 - 0} = opcode;
}

class GBCBInstr<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr16<opcodestr, argsstr, outs, ins, pattern, FixupNone> {
    bits<8> Arg;

    let Inst{15 - 8} = Arg{7 - 0};
    let Inst{7 - 0} = 0xCB;
}

// a = a + r
class GBArithR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rs", (outs), (ins GPR8:$rs), pattern, FixupNone> {
    bits<3> rs;

    let Inst{7 - 3} = opcode{7 - 3};
    let Inst{2 - 0} = rs;
}

class GBAccInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern, FixupNone> {
    let Inst = opcode;
}

// a = a + imm
class GBArithImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$uimm8",
            (outs),
            (ins uimm8:$uimm8),
            pattern,
            FixupData8> {
    bits<8> uimm8;

    let Inst{15 - 8} = uimm8;
    let Inst{7 - 0} = opcode;
}

// r = r + 1
class GBArithGPR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd",
            (outs GPR8:$rs),
            (ins GPR8:$rd),
            pattern,
            FixupNone> {
    bits<3> rd;

    let Constraints = "$rd = $rs";

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = opcode{2 - 0};
}

// HL = HL + r16
class GBArithR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "hl, $rs", (outs), (ins GPR16:$rs), pattern, FixupNone> {
    bits<2> rs;

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 4} = rs;
    let Inst{3 - 0} = opcode{3 - 0};
}

// r16 = r16 + 1
class GBArithGPR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd",
            (outs GPR16:$rs),
            (ins GPR16:$rd),
            pattern,
            FixupNone> {
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
            pattern,
            FixupNone> {
    bits<3> rd;
    bits<3> rs;

    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = rs;
}

// rd = imm
class GBLDImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$rd, $uimm8",
            (outs GPR8:$rd),
            (ins uimm8:$uimm8),
            pattern,
            FixupData8> {
    bits<3> rd;
    bits<8> uimm8;

    let Inst{15 - 8} = uimm8;
    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 3} = rd;
    let Inst{2 - 0} = opcode{2 - 0};
}

// rd16 = imm16
class GBLDR16Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr24<opcodestr, "$rd, $uimm16",
            (outs GPR16:$rd),
            (ins uimm16:$uimm16),
            pattern,
            FixupData16> {
    bits<2> rd;
    bits<16> uimm16;

    let Inst{23 - 8} = uimm16;
    let Inst{7 - 6} = opcode{7 - 6};
    let Inst{5 - 4} = rd;
    let Inst{3 - 0} = opcode{3 - 0};
}

// reg[bit].op()
class GBImm3Instr<bits<8> opcode, string opcodestr, dag outs, dag ins, list<dag> pattern>
  : GBCBInstr<opcodestr, "$uimm3, $rs", outs, ins, pattern> {
    bits<3> rs;
    bits<8> uimm3;  // Unfortunately i3 is not defined in tablegen

    let Arg{7 - 6} = opcode{7 - 6};
    let Arg{5 - 3} = uimm3{2 - 0};
    let Arg{2 - 0} = rs;
}

class GBBitGetInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBImm3Instr<opcode, opcodestr, (outs), (ins GPR8:$rs, uimm3:$uimm3), pattern> {}

class GBBitSetInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBImm3Instr<opcode, opcodestr, (outs GPR8:$rd), (ins GPR8:$rs, uimm3:$uimm3), pattern> {
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
class GBRetCondInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$flag", (outs), (ins flag:$flag), pattern, FixupNone> {
    bits<2> flag;

    let Inst{7 - 5} = opcode{7 - 5};
    let Inst{4 - 3} = flag;
    let Inst{2 - 0} = opcode{2 - 0};

    let isReturn = true;
}


class GBCondImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$flag, $simm8",
             (outs), (ins simm8:$simm8, flag:$flag),
             pattern,
             FixupPCRel8> {
    bits<2> flag;
    bits<8> simm8;

    let Inst{15 - 8} = simm8;
    let Inst{7 - 5} = opcode{7 - 5};
    let Inst{4 - 3} = flag;
    let Inst{2 - 0} = opcode{2 - 0};

    let isBranch = true;
}

// Special Gameboy-specific hardware control instructions
class GBCtlInstruction<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern, FixupNone> {
    let Inst{7 - 0} = opcode;

    let hasSideEffects = true;
}

// TODO: how to mark the flag register as output?
class GBFlagInstr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "", (outs), (ins), pattern, FixupNone> {
    let Inst = opcode;
}

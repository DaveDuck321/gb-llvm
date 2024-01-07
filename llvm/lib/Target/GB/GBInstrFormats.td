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
class GBInstr<string mnemonic, string argstr, bits<8> opcode,
              dag outs, dag ins, list<dag> pattern,
              FixupKind fixup> : Instruction {
    bits<24> Inst;
    let Inst{7 - 0} = opcode;

    let Namespace = "GB";
    let AsmString = mnemonic # "\t" # argstr;

    let OutOperandList = outs;
    let InOperandList = ins;
    let Pattern = pattern;

    // These properties need to be overwritten
    let mayStore = false;
    let mayLoad = false;
    let hasSideEffects = false;

    let TSFlags{2 - 0} = fixup.Kind;
}

class GBInstrNoOperands<string mnemonic, bits<8> opcode, list<dag> pattern>
  : GBInstr<mnemonic, "", opcode, (outs), (ins), pattern, FixupNone>
{
  let Size = 1;
}

// TODO GB: can I combine these sequencese
multiclass _GB_GPR8S1Sequence<string mnemonic, bits<8> opcode, dag inputs, list<dag> pattern> {
  def _r : GBInstr<mnemonic, "$rs", opcode, (outs), !con((ins GPR8:$rs), inputs), pattern, FixupNone> {
    bits<3> rs;
    let Inst{2 - 0} = rs;
  }

  // TODO GB: get HL pattern from gpr instruction pattern
  // FIXME: add Uses = HL
  def _iHL : GBInstr<mnemonic, "(hl)", opcode, (outs), (ins), [], FixupNone> {
    let Inst{2 - 0} = 0b110;
    let mayLoad = true;
  }
}

multiclass _GB_GPR8S8Sequence<string mnemonic, bits<8> opcode, dag inputs, list<dag> pattern> {
  def _r : GBInstr<mnemonic, "$rs", opcode, (outs), !con((ins GPR8:$rs), inputs), pattern, FixupNone> {
    bits<3> rs;
    let Inst{5 - 3} = rs;
  }

  // TODO GB: get HL pattern from gpr instruction pattern
  // FIXME: add Uses = HL
  def _iHL : GBInstr<mnemonic, "(hl)", opcode, (outs), (ins), [], FixupNone> {
    let Inst{5 - 3} = 0b110;
    let mayLoad = true;
  }
}

multiclass GBGPR8LD<string mnemonic, bits<8> opcode, list<dag> pattern> {
  let Size = 1 in {
    def _rr : GBInstr<mnemonic, "$rd, $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs), pattern, FixupNone> {
      bits<3> rd;
      bits<3> rs;
      let Inst{5 - 3} = rd;
      let Inst{2 - 0} = rs;
      let isMoveReg = true;
    }
    def _r_HL : GBInstr<mnemonic, "$rd, (hl)", opcode, (outs GPR8:$rd), (ins), pattern, FixupNone> {
      bits<3> rd;
      let Inst{5 - 3} = rd;
      let Inst{2 - 0} = 0b110;
      let Uses = [HL];
      let mayLoad = true;
    }
    def _HL_r : GBInstr<mnemonic, "(hl), $rs", opcode, (outs), (ins GPR8:$rs), pattern, FixupNone> {
      bits<3> rs;
      let Inst{5 - 3} = 0b110;
      let Inst{2 - 0} = rs;
      let Uses = [HL];
      let mayStore = true;
    }
  }
}

multiclass GBGPR8LDI8<string mnemonic, bits<8> opcode, list<dag> pattern> {
  let Size = 2, isMoveImm = true in {
    def _r : GBInstr<mnemonic, "$rd, $imm", opcode, (outs GPR8:$rd), (ins uimm8:$imm), pattern, FixupData8> {
      bits<3> rd;
      bits<8> imm;
      let Inst{15 - 8} = imm;
      let Inst{5 - 3} = rd;
    }

    // TODO GB: get HL pattern from gpr instruction pattern
    def _iHL : GBInstr<mnemonic, "(hl), $imm", opcode, (outs), (ins uimm8:$imm), [], FixupData8> {
      bits<8> imm;
      let Inst{15 - 8} = imm;
      let Inst{5 - 3} = 0b110;
      let Uses = [HL];
      let mayStore = true;
    }
  }
}

class GBGPR8LDI16<string mnemonic, bits<8> opcode, list<dag> pattern>
  : GBInstr<mnemonic, "$rd, $imm", opcode, (outs GPR16:$rd), (ins uimm16:$imm), pattern, FixupData16>
{
  bits<2> rd;
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Inst{5 - 4} = rd;
  let Size = 3;
  let isMoveImm = true;
}

class _GB_GPR16S16Sequence<string mnemonic, bits<8> opcode, dag inputs, list<dag> pattern>
  : GBInstr<mnemonic, "$rs", opcode, (outs), !con((ins GPR16:$rs), inputs), pattern, FixupNone>
{
  bits<2> rs;
  let Inst{5 - 4} = rs;
}

// TOOD GB: combine these
class _GB_SimpleImm8<string mnemonic, bits<8> opcode, list<dag> pattern, GBImm immType, FixupKind fixup>
  : GBInstr<mnemonic, "$imm", opcode, (outs), (ins immType:$imm), pattern, fixup>
{
  bits<8> imm;
  let Inst{15 - 8} = imm;
}

class _GB_SimpleImm16<string mnemonic, bits<8> opcode, list<dag> pattern>
  : GBInstr<mnemonic, "$imm", opcode, (outs), (ins uimm16:$imm), pattern, FixupData16>
{
  bits<16> imm;
  let Inst{23 - 8} = imm;
}

class GBCBInstr<string mnemonic, string argstr, bits<8> opcode, dag outs, dag ins, list<dag> pattern>
  : GBInstr<mnemonic, argstr, 0xCB, outs, ins, pattern, FixupNone>
{
  let Size = 2;
  let Inst{15 - 11} = opcode{7 - 3};
}

// TODO GB: combine these
multiclass _GB_CBGPR8S1<string mnemonic, bits<8> opcode, list<dag> pattern>
{
  def _r : GBCBInstr<mnemonic, "$rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs), pattern> {
    bits<3> rs;
    let Inst{10 - 8} = rs;
    let Constraints = "$rs = $rd";
  }

  // TODO: derive pattern from _r
  def _iHL : GBCBInstr<mnemonic, "(hl)", opcode, (outs), (ins), []> {
    let Inst{10 - 8} = 0b110;

    let Uses = [HL];
    let mayLoad = true;
    let mayStore = true;
  }
}

multiclass GBCBGPR8S1Imm3<string mnemonic, bits<8> opcode, list<dag> pattern>
{
  def _r : GBCBInstr<mnemonic, "$imm, $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs, uimm3:$imm), pattern> {
    bits<3> rs;
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = rs;
    let Constraints = "$rs = $rd";
  }

  // TODO: derive pattern from _r
  def _iHL : GBCBInstr<mnemonic, "$imm, (hl)", opcode, (outs), (ins uimm3:$imm), []> {
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = 0b110;

    let Uses = [HL];
    let mayLoad = true;
    let mayStore = true;
  }
}

class _GB_FlagSequence<string mnemonic, string arg2, bits<8> opcode, dag inputs, list<dag> pattern, FixupKind fixup>
  : GBInstr<mnemonic, "$flag" # arg2, opcode, (outs), !con((ins flag:$flag), inputs), pattern, fixup>
{
  bits<2> flag;
  let Inst{4 - 3} = flag;
}

class _GB_FlagImm8<string mnemonic, bits<8> opcode, list<dag> pattern>
  : _GB_FlagSequence<mnemonic, ", $imm", opcode, (ins simm8:$imm), pattern, FixupPCRel8>
{
  bits<8> imm;
  let Inst{15 - 8} = imm;
  let Size = 2;
}

class _GB_FlagImm16<string mnemonic, bits<8> opcode, list<dag> pattern>
  : _GB_FlagSequence<mnemonic, ", $imm", opcode, (ins uimm16:$imm), pattern, FixupData16>
{
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Size = 3;
}

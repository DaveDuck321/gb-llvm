// Fixup types
// Sync with GBFixupKinds.h
class FixupKind<bits<3> kind> { bits<3> Kind = kind; }
def FixupNone    : FixupKind<0>;
def FixupData8   : FixupKind<1>;
def FixupData16  : FixupKind<2>;
def FixupPCRel8  : FixupKind<3>;

// Operand types
class GBAsmImmOperand<string name> : AsmOperandClass {
  let Name = name;
  let RenderMethod = "addImmOperands";
  let DiagnosticString = name # " is invalid";
  let ParserMethod = "tryParseImmediate";
}

class GBImm<int size, string signedness, ValueType ty> : Operand<ty> {
  let PrintMethod = "print" # signedness # size # "ImmOperand";
  let ParserMatchClass = GBAsmImmOperand<signedness # "Imm" # size>;
  let DecoderMethod = "Decode" # signedness # "ImmOperand<" # size # ">";
}

let OperandType = "OPERAND_IMMEDIATE" in {
  def uimm3 : GBImm<3, "U", i8>;
  def simm8 : GBImm<8, "S", i8>;
  def uimm8 : GBImm<8, "U", i8>;
  def uimm16 : GBImm<16, "U", i16>;

  def bb_absolute : GBImm<16, "U", OtherVT>;
}

let OperandType = "OPERAND_PCREL" in {
  def bb_pc_rel : GBImm<8, "S", OtherVT> {
    let OperandType = "OPERAND_PCREL";
    let PrintMethod = "printPCRelS8ImmOperand";
  }
}

def rstvec : Operand<i8> {
  let PrintMethod = "printU8ImmOperand";
  let ParserMatchClass = GBAsmImmOperand<"RstVec">;
  let EncoderMethod = "EncodeRstVecOperand";
  let DecoderMethod = "DecodeRstVecOperand";
}

def GBAsmFlagOperand : AsmOperandClass {
  let Name = "Flag";
  let RenderMethod = "addFlagOperands";
  let DiagnosticString = "invalid flag";
  let ParserMethod = "tryParseFlag";
}

class GBFLAG_CC<bits<2> flag> { bits<2> Encoding = flag; }
def GBFLAG_NZ : GBFLAG_CC<0>;
def GBFLAG_Z  : GBFLAG_CC<1>;
def GBFLAG_NC : GBFLAG_CC<2>;
def GBFLAG_C  : GBFLAG_CC<3>;

def flag : Operand<i2> {
  let PrintMethod = "printFlagOperand";
  let ParserMatchClass = GBAsmFlagOperand;
}

// Instruction formats
class GBInstr<string mnemonic, string argstr, bits<8> opcode,
              dag outs, dag ins, FixupKind fixup> : Instruction {
    bits<24> Inst;
    let Inst{7 - 0} = opcode;

    let Namespace = "GB";
    let AsmString = mnemonic # " " # argstr;

    let OutOperandList = outs;
    let InOperandList = ins;

    // Atm I prefer using Pat<> directly...
    // TODO GB: any disadvantages with this?
    let Pattern = [];

    // These properties need to be overwritten
    let mayStore = false;
    let mayLoad = false;
    let hasSideEffects = false;

    let TSFlags{2 - 0} = fixup.Kind;
}

class GBInstrNoOperands<string mnemonic, bits<8> opcode>
  : GBInstr<mnemonic, "", opcode, (outs), (ins), FixupNone>
{
  let Size = 1;
}

// TODO GB: can I combine these sequences?
multiclass _GB_GPR8S1Sequence<string mnemonic, bits<8> opcode, dag inputs> {
  def _r : GBInstr<mnemonic, "$rs", opcode, (outs), !con((ins GPR8:$rs), inputs), FixupNone> {
    bits<3> rs;
    let Inst{2 - 0} = rs;
    let Uses = [A];
    let Defs = [A, F];
  }

  def _iHL : GBInstr<mnemonic, "(hl)", opcode, (outs), (ins), FixupNone> {
    let Inst{2 - 0} = 0b110;
    let mayLoad = true;
    let Uses = [A, HL];
    let Defs = [A, F];
  }
}

multiclass GBIncDec8<string mnemonic, bits<8> opcode> {
  def _r : GBInstr<mnemonic, "$rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs), FixupNone> {
    bits<3> rs;
    let Inst{5 - 3} = rs;
    let Defs = [F];
    let Size = 1;
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBInstr<mnemonic, "(hl)", opcode, (outs), (ins), FixupNone> {
    let Inst{5 - 3} = 0b110;
    let mayLoad = true;
    let mayStore = true;
    let Uses = [HL];
    let Defs = [F];
    let Size = 1;
  }
}

multiclass GBGPR8LD<string mnemonic, bits<8> opcode> {
  let Size = 1 in {
    def _rr : GBInstr<mnemonic, "$rd, $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs), FixupNone> {
      bits<3> rd;
      bits<3> rs;
      let Inst{5 - 3} = rd;
      let Inst{2 - 0} = rs;
      let isMoveReg = true;
    }
    def _r_iHL : GBInstr<mnemonic, "$rd, (hl)", opcode, (outs GPR8:$rd), (ins), FixupNone> {
      bits<3> rd;
      let Inst{5 - 3} = rd;
      let Inst{2 - 0} = 0b110;
      let Uses = [HL];
      let mayLoad = true;
    }
    def _iHL_r : GBInstr<mnemonic, "(hl), $rs", opcode, (outs), (ins GPR8:$rs), FixupNone> {
      bits<3> rs;
      let Inst{5 - 3} = 0b110;
      let Inst{2 - 0} = rs;
      let Uses = [HL];
      let mayStore = true;
    }
  }
}

multiclass GBGPR8LDI8<string mnemonic, bits<8> opcode> {
  let Size = 2, isMoveImm = true in {
    def _r : GBInstr<mnemonic, "$rd, $imm", opcode, (outs GPR8:$rd), (ins uimm8:$imm), FixupData8> {
      bits<3> rd;
      bits<8> imm;
      let Inst{15 - 8} = imm;
      let Inst{5 - 3} = rd;
    }

    def _iHL : GBInstr<mnemonic, "(hl), $imm", opcode, (outs), (ins uimm8:$imm), FixupData8> {
      bits<8> imm;
      let Inst{15 - 8} = imm;
      let Inst{5 - 3} = 0b110;
      let Uses = [HL];
      let mayStore = true;
    }
  }
}

class GBGPR8LDI16<string mnemonic, bits<8> opcode>
  : GBInstr<mnemonic, "$rd, $imm", opcode, (outs GPR16:$rd), (ins uimm16:$imm), FixupData16>
{
  bits<2> rd;
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Inst{5 - 4} = rd;
  let Size = 3;
  let isMoveImm = true;
}

class _GB_GPR16S16Sequence<string mnemonic, bits<8> opcode, dag outputs>
  : GBInstr<mnemonic, "$rs", opcode, outputs, (ins GPR16:$rs), FixupNone>
{
  bits<2> rs;
  let Inst{5 - 4} = rs;
}

class _GB_IR16S16Sequence<string format, bits<8> opcode>
  : GBInstr<format, "", opcode, (outs), (ins IR16:$rs), FixupNone>
{
  bits<1> rs;
  let Inst{4} = rs;
  let Size = 1;
}

// TOOD GB: combine these
class _GB_SimpleImm8<string format, bits<8> opcode, GBImm immType, FixupKind fixup>
  : GBInstr<format, "", opcode, (outs), (ins immType:$imm), fixup>
{
  bits<8> imm;
  let Inst{15 - 8} = imm;
  let Size = 2;
}

class GBAbsImm16<string format, bits<8> opcode, GBImm immType=bb_absolute>
  : GBInstr<format, "", opcode, (outs), (ins immType:$imm), FixupData16>
{
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Size = 3;
}

class GBCBInstr<string mnemonic, string argstr, bits<8> opcode, dag outs, dag ins>
  : GBInstr<mnemonic, argstr, 0xCB, outs, ins, FixupNone>
{
  let Inst{15 - 11} = opcode{7 - 3};
  let Size = 2;
}

// TODO GB: combine these
multiclass _GB_CBGPR8S1<string mnemonic, bits<8> opcode>
{
  def _r : GBCBInstr<mnemonic, "$rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs)> {
    bits<3> rs;
    let Inst{10 - 8} = rs;
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBCBInstr<mnemonic, "(hl)", opcode, (outs), (ins)> {
    let Inst{10 - 8} = 0b110;

    let Uses = [HL];
    let mayLoad = true;
    let mayStore = true;
  }
}

multiclass GBCBGPR8S1AndFlag<string mnemonic, bits<8> opcode>
{
  def _r : GBCBInstr<mnemonic, "$rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs)> {
    bits<3> rs;
    let Inst{10 - 8} = rs;

    let Defs = [F];
    let Uses = [F];
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBCBInstr<mnemonic, "(hl)", opcode, (outs), (ins)> {
    let Inst{10 - 8} = 0b110;

    let Defs = [F];
    let Uses = [HL, F];
    let mayLoad = true;
    let mayStore = true;
  }
}

multiclass GBCBGPR8S1Imm3<string mnemonic, bits<8> opcode>
{
  def _r : GBCBInstr<mnemonic, "$imm, $rs", opcode, (outs GPR8:$rd), (ins GPR8:$rs, uimm3:$imm)> {
    bits<3> rs;
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = rs;
    let Constraints = "$rs = $rd";
  }

  def _iHL : GBCBInstr<mnemonic, "$imm, (hl)", opcode, (outs), (ins uimm3:$imm)> {
    bits<3> imm;
    let Inst{13 - 11} = imm;
    let Inst{10 - 8} = 0b110;

    let Uses = [HL];
    let mayLoad = true;
    let mayStore = true;
  }
}

class _GB_FlagSequence<string mnemonic, string arg2, bits<8> opcode, dag inputs, FixupKind fixup>
  : GBInstr<mnemonic, "$flag" # arg2, opcode, (outs), !con((ins flag:$flag), inputs), fixup>
{
  bits<2> flag;
  let Inst{4 - 3} = flag;
}

class GBFlagPCRel<string mnemonic, bits<8> opcode>
  : _GB_FlagSequence<mnemonic, ", $imm", opcode, (ins bb_pc_rel:$imm), FixupPCRel8>
{
  bits<8> imm;
  let Inst{15 - 8} = imm;
  let Size = 2;
}

class GBRSTInstr<string mnemonic, bits<8> opcode>
  : GBInstr<mnemonic, "$rst", opcode, (outs), (ins rstvec:$rst), FixupNone>
{
  bits<3> rst;
  let Inst{5 - 3} = rst;
  let Size = 1;
}

class GBAbsFlagImm16<string mnemonic, bits<8> opcode>
  : _GB_FlagSequence<mnemonic, ", $imm", opcode, (ins bb_absolute:$imm), FixupData16>
{
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Size = 3;
}

class GBPushPop16<string mnemonic, bits<8> opcode, dag outputs, dag inputs>
  : GBInstr<mnemonic, "$reg", opcode, outputs, inputs, FixupNone>
{
  bits<2> reg;
  let Inst{5 - 4} = reg;
  let Size = 1;
  let Uses = [SP];
  let Defs = [SP];
}

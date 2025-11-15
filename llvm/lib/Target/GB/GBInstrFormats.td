// Fixup types
// Sync with GBFixupCategorys.h
class FixupCategory<bits<3> kind> { bits<3> Kind = kind; }
def FixupNone : FixupCategory<0>;
def FixupData8 : FixupCategory<1>;
def FixupData16 : FixupCategory<2>;
def FixupPCRel8 : FixupCategory<3>;

// Operand types
class GBAsmImmOperand<string name> : AsmOperandClass {
  let Name = name;
  let RenderMethod = "addImmOperands";
  let DiagnosticString = name#" is invalid";
  let ParserMethod = "tryParseImmediate";
}

class GBImm<int size, string signedness, ValueType ty> : Operand<ty> {
  let PrintMethod = "print"#signedness#size#"ImmOperand";
  let ParserMatchClass = GBAsmImmOperand<signedness#"Imm"#size>;
  let DecoderMethod = "Decode"#signedness#"ImmOperand<"#size#">";
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
def GBFLAG_Z : GBFLAG_CC<1>;
def GBFLAG_NC : GBFLAG_CC<2>;
def GBFLAG_C : GBFLAG_CC<3>;

def flag : Operand<i2> {
  let PrintMethod = "printFlagOperand";
  let ParserMatchClass = GBAsmFlagOperand;
}

// Instruction formats
class GBInstrBase<string asm, bits<8> opcode, dag outs, dag ins,
                  FixupCategory fixup> : Instruction {
  bits<24> Inst;
  let Inst{7 - 0} = opcode;

  let Namespace = "GB";
  let AsmString = asm;

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

class GBInstr<string asm, bits<8> opcode, dag outs, dag ins>
    : GBInstrBase<asm, opcode, outs, ins, FixupNone> {
  let Size = 1;
}

class GBInstrNoOperands<string asm, bits<8> opcode>
    : GBInstr<asm, opcode, (outs), (ins)> {}

class GBInstrImm8<string asm, bits<8> opcode, dag outs, dag ins,
                  FixupCategory fixup>
    : GBInstrBase<asm, opcode, outs, ins, fixup> {
  bits<8> imm;
  let Inst{15 - 8} = imm;
  let Size = 2;
}

class GBInstrImm16<string asm, bits<8> opcode, dag outs, dag ins>
    : GBInstrBase<asm, opcode, outs, ins, FixupData16> {
  bits<16> imm;
  let Inst{23 - 8} = imm;
  let Size = 3;
}

class GBInstrCB<string asm, bits<8> opcode, dag outs, dag ins>
    : GBInstrBase<asm, 0xCB, outs, ins, FixupNone> {
  let Inst{15 - 11} = opcode{7 - 3};
  let Size = 2;
}

class Pseudo<string asm, dag outs, dag ins>
    : GBInstrBase<"pseudo: "#asm, -1, outs, ins, FixupNone> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

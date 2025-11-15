include "GBInstrFormats.td"

def imm8 : ImmLeaf<i8, [{return isInt<8>(Imm);}]>;
def imm16 : ImmLeaf<i16, [{return isInt<16>(Imm);}]>;
def io_imm16
    : ImmLeaf<i16, [{return isInt<16>(Imm) && (Imm & 0xff00U) == 0xff00U;}]>;

include "GBInstrInfoALU.td"
include "GBInstrInfoBranch.td"
include "GBInstrInfoLSU.td"

def NOP : GBInstrNoOperands<"nop", 0x00>;

let hasSideEffects = true in {
  def STOP : GBInstrNoOperands<"stop", 0x10>;
  def HALT : GBInstrNoOperands<"halt", 0x76>;
  def DI : GBInstrNoOperands<"di", 0xF3>;
  def EI : GBInstrNoOperands<"ei", 0xFB>;
  def TRAP : GBInstrNoOperands<"trap", 0xD3>;
  def DEBUG_TRAP : GBInstrNoOperands<"debugtrap", 0xE3>;
}

class GBInstrPushPop<string mnemonic, bits<8> opcode, dag outputs, dag inputs>
    : GBInstr<mnemonic#" $reg", opcode, outputs, inputs> {
  bits<2> reg;
  let Inst{5 - 4} = reg;
  let Uses = [SP];
  let Defs = [SP];
}

def POP : GBInstrPushPop<"pop", 0xC1, (outs GPR16AndAF:$reg), (ins)> {
  let mayLoad = true;
}

def PUSH : GBInstrPushPop<"push", 0xC5, (outs), (ins GPR16AndAF:$reg)> {
  let mayStore = true;
}

let Uses = [SP], Defs = [SP, F] in {
  def ADJCALLSTACKDOWN
      : Pseudo<"adjcallstackdown", (outs), (ins uimm16:$imm, uimm16:$out)>;
  def ADJCALLSTACKUP
      : Pseudo<"adjcallstackup", (outs), (ins uimm16:$imm, uimm16:$out)>;
}

include "GBInstrPatterns.td"

// Operand types
def imm8 : Operand<i8>;
def imm16 : Operand<i16>;

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
}

class GBInstr16<string opcodestr, string argsstr, dag outs, dag ins, list<dag> pattern>
  : GBInstr<opcodestr, argsstr, outs, ins, pattern> {
    bits<16> Inst;
}

// a = a + r
class GBArithR8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr8<opcodestr, "$rd, $rs1, $rs2",
            (outs Acc:$rd),
            (ins Acc:$rs1, GPR8:$rs2),
            pattern> {
    bits<3> rs2;

    let Inst{7 - 3} = opcode{7 - 3};
    let Inst{2 - 0} = rs2;
}

// a = a + imm
class GBArithImm8Instr<bits<8> opcode, string opcodestr, list<dag> pattern>
  : GBInstr16<opcodestr, "$rd, $rs1, $imm8",
            (outs Acc:$rd),
            (ins Acc:$rs1, imm8:$imm8),
            pattern> {
    bits<8> imm8;

    let Inst{8 - 15} = imm8;
    let Inst{7 - 0} = opcode;
}

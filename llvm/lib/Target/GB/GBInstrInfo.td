include "GBInstrFormats.td"

def NOP : NopInstr<0x00, "nop", []>;

def LD8  : GBLDR8Instr<0x40, "ld", []>;
def LDI8 : GBLDImm8Instr<0x06, "ld", []>;
def LD16 : GBLDR16Instr<0x01, "ld", []>;

def ADD : GBArithR8Instr<0x80, "add", []>;
def ADC : GBArithR8Instr<0x88, "adc", []>;
def SUB : GBArithR8Instr<0x90, "sub", []>;
def SBC : GBArithR8Instr<0x98, "sbc", []>;
def AND : GBArithR8Instr<0xA0, "and", []>;
def XOR : GBArithR8Instr<0xA8, "xor", []>;
def OR  : GBArithR8Instr<0xB0, "or", []>;
def CP  : GBArithR8Instr<0xB8, "cp", []>;

def ADDI : GBArithImm8Instr<0xC6, "add", []>;
def ADCI : GBArithImm8Instr<0xCE, "adc", []>;
def SUBI : GBArithImm8Instr<0xD6, "sub", []>;
def SBCI : GBArithImm8Instr<0xDE, "sbc", []>;
def ANDI : GBArithImm8Instr<0xE6, "and", []>;
def XORI : GBArithImm8Instr<0xEE, "xor", []>;
def ORI  : GBArithImm8Instr<0xF6, "or", []>;
def CPI  : GBArithImm8Instr<0xFE, "cp", []>;

def INC8 : GBArithGPR8Instr<0x04, "inc", []>;
def DEC8 : GBArithGPR8Instr<0x05, "dec", []>;

def ADD16 : GBArithR16Instr<0x09, "add", []>;
def INC16 : GBArithGPR16Instr<0x03, "inc", []>;
def DEC16 : GBArithGPR16Instr<0x0B, "dec", []>;

def RLCA : GBAccInstr<0x07, "rlca", []>;
def RLA  : GBAccInstr<0x17, "rla", []>;
def RRCA : GBAccInstr<0x0F, "rrca", []>;
def RRA  : GBAccInstr<0x1F, "rra", []>;
def DAA  : GBAccInstr<0x27, "daa", []>;
def CPL  : GBAccInstr<0x2F, "cpl", []>;

def SCF : GBFlagInstr<0x37, "scf", []>;
def CCF : GBFlagInstr<0x3F, "ccf", []>;

// CB instructions
def RLC  : GBShuffleRegInstr<0x00, "rlc", []>;
def RRC  : GBShuffleRegInstr<0x08, "rrc", []>;
def RL   : GBShuffleRegInstr<0x10, "rl", []>;
def RR   : GBShuffleRegInstr<0x18, "rr", []>;
def SLA  : GBShuffleRegInstr<0x20, "sla", []>;
def SRA  : GBShuffleRegInstr<0x28, "sra", []>;
def SWAP : GBShuffleRegInstr<0x30, "swap", []>;
def SRL  : GBShuffleRegInstr<0x38, "srl", []>;

def BIT : GBBitGetInstr<0x40, "bit", []>;
def RES : GBBitSetInstr<0x80, "res", []>;
def SET : GBBitSetInstr<0xC0, "set", []>;

def JR : GBCondImm8Instr<0x20, "jr", []>;
def RET : GBRetCondInstr<0xC0, "ret", []>;

// NOTE: stop is acually 2 bytes -- officially STOP 0 ...BUT...
// the gameboy sometimes interprets it as a 1 byte instruction.
// Lets just interpret that pattern as STOP NOP
// TODO: add STOP 0 as a PSEUDO instruction
def STOP : GBCtlInstruction<0x10, "stop", []>;
def HALT : GBCtlInstruction<0x76, "halt", []>;
def DI   : GBCtlInstruction<0xF3, "di", []>;
def EI   : GBCtlInstruction<0xFb, "ei", []>;

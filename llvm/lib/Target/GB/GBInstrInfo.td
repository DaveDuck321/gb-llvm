include "GBInstrFormats.td"

def LD  : GBLDR8Instr<0x40, "ld", []>;
def LDI : GBLDImm8Instr<0x06, "ld", []>;

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

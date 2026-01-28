class GBReg<string name, bits<16> enc> : Register<name> {
  let Namespace = "GB";
  let HWEncoding = enc;
}

def UpperSubReg : SubRegIndex<8, 8>;
def LowerSubReg : SubRegIndex<8, 0>;

class GBCombinedReg<string name, bits<16> enc, list<GBReg> halves>
    : GBReg<name, enc> {
  let SubRegs = halves;
  let SubRegIndices = [UpperSubReg, LowerSubReg];
  let CoveredBySubRegs = true;
}

// 8-Bit registers
def B : GBReg<"b", 0>;
def C : GBReg<"c", 1>;
def D : GBReg<"d", 2>;
def E : GBReg<"e", 3>;
def H : GBReg<"h", 4>;
def L : GBReg<"l", 5>;
def A : GBReg<"a", 7>;
def F : GBReg<"f", -1>;

// 16-Bit registers
def BC : GBCombinedReg<"bc", 0, [B, C]>, DwarfRegNum<[1]>;
def DE : GBCombinedReg<"de", 1, [D, E]>, DwarfRegNum<[2]>;
def HL : GBCombinedReg<"hl", 2, [H, L]>, DwarfRegNum<[3]>;
def AF : GBCombinedReg<"af", 3, [A, F]>, DwarfRegNum<[0]>;
def SP : GBReg<"sp", 3>, DwarfRegNum<[4]>;
def PC : GBReg<"pc", -1>, DwarfRegNum<[5]>;

// Debug only registers
def DEBUG_REG : GBReg<"DEBUG_REG", -1>;

// Classes
def Flags : RegisterClass<"GB", [i8], 8, (add F)> { let isAllocatable = false; }

def GPR8 : RegisterClass<"GB", [i8], 8, (add A, B, C, D, E, H, L)>;
def GPR16 : RegisterClass<"GB", [i16], 16, (add BC, DE, HL)>;
def ReservedR16 : RegisterClass<"GB", [i16], 16, (add AF, SP)>;

def GPR16AndSP : RegisterClass<"GB", [i16], 16, (add BC, DE, HL, SP)>;
def GPR16AndAF : RegisterClass<"GB", [i16], 16, (add BC, DE, HL, AF)>;
def GPR16WithoutHL : RegisterClass<"GB", [i16], 16, (add BC, DE)>;

def DebugRegs : RegisterClass<"GB", [i128], 128, (add DEBUG_REG)> {
  let isAllocatable = false;
}

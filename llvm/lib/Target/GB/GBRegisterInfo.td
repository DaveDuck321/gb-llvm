class GBReg<string name, bits<16> enc> : Register<name> {
    let Namespace = "GB";
    let HWEncoding = enc;
}

class GBCombinedReg<string name, bits<16> enc, list<GBReg> halves> : GBReg<name, enc> {
    let SubRegs = halves;
    let SubRegIndices = [SubRegIndex<8, 0>, SubRegIndex<8, 8>];
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
def BC : GBCombinedReg<"bc", 0, [B, C]>;
def DE : GBCombinedReg<"de", 1, [D, E]>;
def HL : GBCombinedReg<"hl", 2, [H, L]>;
def AF : GBCombinedReg<"af", 3, [A, F]>;
def SP : GBReg<"sp", 3>;

// Classes
def Flags : RegisterClass<"GB", [i8], 8, (add F)> { let isAllocatable = false; }

def GPR8 : RegisterClass<"GB", [i8], 8, (add A, B, C, D, E, H, L)>;
def GPR16 : RegisterClass<"GB", [i16], 16, (add BC, DE, HL, SP)>;
def SR16 : RegisterClass<"GB", [i16], 16, (add BC, DE, HL, AF)>;
def IR16 : RegisterClass<"GB", [i16], 16, (add BC, DE)>;

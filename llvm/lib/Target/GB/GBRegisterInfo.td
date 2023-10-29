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
def A : GBReg<"A", 0>;
def F : GBReg<"F", 1>;
def B : GBReg<"B", 2>;
def C : GBReg<"C", 3>;
def D : GBReg<"D", 4>;
def E : GBReg<"E", 5>;
def H : GBReg<"H", 6>;
def L : GBReg<"L", 7>;

// 16-Bit registers
def AF : GBCombinedReg<"AF", 0, [A, F]>;
def BC : GBCombinedReg<"BC", 1, [B, C]>;
def DE : GBCombinedReg<"DE", 2, [D, E]>;
def HL : GBCombinedReg<"HL", 3, [H, L]>;

def SP : GBReg<"SP", 8>;

def Accumulator : RegisterClass<"GB", [i8], 8, (add A)>;
def Flags : RegisterClass<"GB", [i8], 8, (add F)> { let isAllocatable = false; }
def GPR8 : RegisterClass<"GB", [i8], 8, (add B, C, D, E, H, L)>;
def GPR16 : RegisterClass<"GB", [i16], 16, (add AF, BC, DE, HL, SP)> { let CopyCost = 2; }

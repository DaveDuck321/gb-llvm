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
def B : GBReg<"B", 0>;
def C : GBReg<"C", 1>;
def D : GBReg<"D", 2>;
def E : GBReg<"E", 3>;
def H : GBReg<"H", 4>;
def L : GBReg<"L", 5>;
def A : GBReg<"A", 7>;
def F : GBReg<"F", -1>;

// 16-Bit registers
def BC : GBCombinedReg<"BC", 0x00, [B, C]>;
def DE : GBCombinedReg<"DE", 0x10, [D, E]>;
def HL : GBCombinedReg<"HL", 0x20, [H, L]>;
def AF : GBCombinedReg<"AF", 0x30, [A, F]>;
def SP : GBReg<"SP", 0x30>;

// Classes
def Acc : RegisterClass<"GB", [i8], 8, (add A)>;
def Flags : RegisterClass<"GB", [i8], 8, (add F)> { let isAllocatable = false; }

def GPR8 : RegisterClass<"GB", [i8], 8, (add A, B, C, D, E, H, L)>;
def GPR16 : RegisterClass<"GB", [i16], 16, (add BC, DE, HL, SP)> { let CopyCost = 2; }

def GPRRegBank : RegisterBank<"GPRB", [GPR8, GPR16, GPR16AndSP, GPR16AndAF,
                                       GPR16WithoutHL]>;

def DebugOnlyRegBank : RegisterBank<"DEBUG_REG_BANK", [DebugRegs]>;

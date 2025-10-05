// Note: I just made this up, its kind of arbitrary.
// TODO GB: benchmark various calling conventions (or find an existing one)

def RetCC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ A ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL ]>>
]>;

def CC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ B, C, D, E ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ BC, DE ]>>,
    CCAssignToStack<0, 0>,
]>;

def CC_GB_Fast : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ B, C, D, E ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL, BC, DE ]>>,
    CCAssignToStack<0, 0>,
]>;

def CC_GB_Interrupt : CallingConv<[CCAssignToStack<0, 0>]>;

def GB_CC_CSRs : CalleeSavedRegs<(add)>;
def GB_Interrupt_CC_CSRs : CalleeSavedRegs<(add BC, DE)>;

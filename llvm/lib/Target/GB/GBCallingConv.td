// Note: I just made this up, its kind of arbitrary.
// TODO GB: benchmark various calling conventions (or find an existing one)

def RetCC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ A ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL ]>>
]>;

def CC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ B, C, D, E ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL, BC, DE ]>>,
    CCAssignToStack<0, 0>,
]>;

def GB_CSRs : CalleeSavedRegs<(add)>;

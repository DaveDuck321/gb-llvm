// Note: I just made this up, its kind of arbitrary.
// TODO GB: benchmark various calling conventions (or find an existing one)

// TODO GB: what if both these matches fail...
// I'm hoping it'll be smart enough to allocate in the
// stack and return a pointer. Verify this!
def RetCC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ A ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL ]>>
]>;

def CC_GB : CallingConv<[
    CCIfType<[ i8 ], CCAssignToReg<[ B, C ]>>,
    CCIfType<[ i16 ], CCAssignToReg<[ HL ]>>,
    CCAssignToStack<0, 0> // TODO GB: confim ABI size is correct here
]>;

def GB_CSRs : CalleeSavedRegs<(add)>;

include "llvm/Target/Target.td"

include "GBRegisterInfo.td"
include "GBInstrInfo.td"

def GBInstrInfo : InstrInfo;

def GB : Target {
    let InstructionSet = GBInstrInfo;
}

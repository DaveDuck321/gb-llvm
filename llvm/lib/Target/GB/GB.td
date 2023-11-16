include "llvm/Target/Target.td"

include "GBRegisterInfo.td"
include "GBInstrInfo.td"

def : ProcessorModel<"Gameboy classic", NoSchedModel, []>;
def : ProcessorModel<"Gameboy color", NoSchedModel, []>;

def GBInstrInfo : InstrInfo;
def GBAsmParser : AsmParser;

def GB : Target {
    let InstructionSet = GBInstrInfo;
    let AssemblyParsers = [GBAsmParser];
}

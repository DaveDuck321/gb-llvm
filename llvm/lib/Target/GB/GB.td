include "llvm/Target/Target.td"

include "GBRegisterInfo.td"
include "GBInstrInfo.td"

def : ProcessorModel<"Gameboy classic", NoSchedModel, []>;
def : ProcessorModel<"Gameboy color", NoSchedModel, []>;

def GBAsmParser : AsmParser;
def GBAsmParserVariant : AsmParserVariant { let TokenizingCharacters = "()[]*!"; }
def GBInstrInfo : InstrInfo { let guessInstructionProperties = false; }

def GB : Target {
  let InstructionSet = GBInstrInfo;
  let AssemblyParsers = [GBAsmParser];
  let AssemblyParserVariants = [GBAsmParserVariant];
}

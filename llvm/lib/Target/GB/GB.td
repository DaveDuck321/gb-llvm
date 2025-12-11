include "llvm/Target/Target.td"

include "GBRegisterInfo.td"

include "GBCallingConv.td"

include "GBInstrInfo.td"
include "GISel/GBPatterns.td"
include "GISel/GBRegisterBanks.td"
include "GISel/GBCombines.td"

def : ProcessorModel<"Gameboy classic", NoSchedModel, []>;
def : ProcessorModel<"Gameboy color", NoSchedModel, []>;

def GBAsmParser : AsmParser;
def GBAsmParserVariant : AsmParserVariant {
  let TokenizingCharacters = "()[]*!";
}
def GBInstrInfo : InstrInfo { let guessInstructionProperties = false; }

def GB : Target {
  let InstructionSet = GBInstrInfo;
  let AssemblyParsers = [GBAsmParser];
  let AssemblyParserVariants = [GBAsmParserVariant];
  let AllowRegisterRenaming = 1;
}

#ifndef LLVM_LIB_TARGET_GB_GBINSTPRINTER_H
#define LLVM_LIB_TARGET_GB_GBINSTPRINTER_H

#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {

class GBInstPrinter : public MCInstPrinter {
public:
  GBInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                const MCRegisterInfo &MRI)
      : MCInstPrinter(MAI, MII, MRI) {}

  void printInst(const MCInst *, uint64_t Address, StringRef Annot,
                 const MCSubtargetInfo &, raw_ostream &) override;

  void printRegName(raw_ostream &, MCRegister) const override;

  // NOTE: Required by tablegen
  void printOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printU3ImmOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printS8ImmOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printU8ImmOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printD8ImmOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printU16ImmOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;
  void printFlagOperand(const MCInst *, unsigned OpNo, raw_ostream &) const;

  // NOTE: these are TableGen'ed
  std::pair<const char *, uint64_t> getMnemonic(const MCInst *) override;
  void printInstruction(const MCInst *, uint64_t Address, raw_ostream &);
  static const char *getRegisterName(MCRegister);
};

} // namespace llvm

#endif

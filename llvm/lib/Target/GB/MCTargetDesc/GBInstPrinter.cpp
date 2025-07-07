#include "GBInstPrinter.h"
#include "GBInstrInfo.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/NativeFormatting.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#include "GBGenAsmWriter.inc"

static bool printIfExpression(const MCAsmInfo &MAI, const MCOperand &Operand,
                              raw_ostream &OS) {
  if (Operand.isExpr()) {
    MAI.printExpr(OS, *Operand.getExpr());
    return true;
  }
  return false;
}

void GBInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                              StringRef Annot, const MCSubtargetInfo &,
                              raw_ostream &OS) {
  printInstruction(MI, Address, OS);
  printAnnotation(OS, Annot);
}

void GBInstPrinter::printRegName(raw_ostream &OS, MCRegister Reg) {
  OS << getRegisterName(Reg);
}

void GBInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                 raw_ostream &OS) {
  const auto &Operand = MI->getOperand(OpNo);
  assert(Operand.isReg());
  printRegName(OS, Operand.getReg());
}

void GBInstPrinter::printU3ImmOperand(const MCInst *MI, unsigned OpNo,
                                      raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(MAI, Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

void GBInstPrinter::printS8ImmOperand(const MCInst *MI, unsigned OpNo,
                                      raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(MAI, Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

void GBInstPrinter::printU8ImmOperand(const MCInst *MI, unsigned OpNo,
                                      raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(MAI, Operand, OS)) {
    assert(Operand.isImm());
    OS << "$";
    llvm::write_hex(OS, (uint8_t)Operand.getImm(), HexPrintStyle::Lower, 2);
  }
}

void GBInstPrinter::printU16ImmOperand(const MCInst *MI, unsigned OpNo,
                                       raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(MAI, Operand, OS)) {
    assert(Operand.isImm());
    OS << "$";
    llvm::write_hex(OS, (uint16_t)Operand.getImm(), HexPrintStyle::Lower, 4);
  }
}

void GBInstPrinter::printFlagOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  assert(Operand.isImm() &&
         (isUInt<2>(Operand.getImm()) || isInt<2>(Operand.getImm())));

  switch (Operand.getImm() & 0b11) {
  case GBFlag::NZ:
    OS << "nz";
    break;
  case GBFlag::Z:
    OS << "z";
    break;
  case GBFlag::NC:
    OS << "nc";
    break;
  case GBFlag::C:
    OS << "c";
    break;
  default:
    llvm_unreachable("Unrecognized flag type");
  }
}

void GBInstPrinter::printPCRelS8ImmOperand(const MCInst *MI, uint64_t Addr,
                                           unsigned OpNo,
                                           raw_ostream &OS) const {
  // TODO GB: should I even use Addr?
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(MAI, Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

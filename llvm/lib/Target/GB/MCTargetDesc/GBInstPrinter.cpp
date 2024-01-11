#include "GBInstPrinter.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/NativeFormatting.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#include "GBGenAsmWriter.inc"

static bool printIfExpression(const MCOperand &Operand, raw_ostream &OS) {
  if (Operand.isExpr()) {
    OS << *Operand.getExpr();
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

void GBInstPrinter::printRegName(raw_ostream &OS, MCRegister Reg) const {
  OS << getRegisterName(Reg);
}

void GBInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                 raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  assert(Operand.isReg());
  printRegName(OS, Operand.getReg());
}

void GBInstPrinter::printU3ImmediateOperand(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

void GBInstPrinter::printS8ImmediateOperand(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

void GBInstPrinter::printU8ImmediateOperand(const MCInst *MI, unsigned OpNo,
                                            raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(Operand, OS)) {
    assert(Operand.isImm());
    OS << Operand.getImm();
  }
}

void GBInstPrinter::printU16ImmediateOperand(const MCInst *MI, unsigned OpNo,
                                             raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  if (not printIfExpression(Operand, OS)) {
    assert(Operand.isImm());
    OS << "$";
    llvm::write_hex(OS, Operand.getImm(), HexPrintStyle::Lower, 4);
  }
}

void GBInstPrinter::printFlagOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &OS) const {
  const auto &Operand = MI->getOperand(OpNo);
  assert(Operand.isImm());

  switch (Operand.getImm()) {
  case 0:
    OS << "nz";
    break;
  case 1:
    OS << "z";
    break;
  case 2:
    OS << "nc";
    break;
  case 3:
    OS << "c";
    break;
  default:
    llvm_unreachable("Unrecognized flag type");
  }
}

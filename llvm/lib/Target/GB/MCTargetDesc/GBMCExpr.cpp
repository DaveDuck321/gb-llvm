#include "GBMCExpr.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCValue.h"

using namespace llvm;

const GBMCExpr *GBMCExpr::create(const MCExpr *Expr, SymbolSpecifier S,
                                 MCContext &Ctx) {
  return new (Ctx) GBMCExpr(Expr, S);
}

void GBMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  SymbolSpecifier S = getSpecifier();
  switch (S) {
  case SPECIFIER_LO_16:
    OS << "%lo ";
    break;
  case SPECIFIER_HI_16:
    OS << "%hi ";
    break;
  default:
    break;
  }
  MAI->printExpr(OS, *Expr);
}

bool GBMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                         const MCAssembler *Asm) const {
  if (!getSubExpr()->evaluateAsRelocatable(Res, Asm)) {
    return false;
  }
  Res.setSpecifier(getSpecifier());

  // TODO: maybe I need to be stricter here and also reject/ handle offsets?
  return !Res.getSubSym();
}

void GBMCExpr::visitUsedExpr(llvm::MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

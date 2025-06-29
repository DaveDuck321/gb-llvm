#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCEXPR_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBMCEXPR_H

#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;

class GBMCExpr : public MCTargetExpr {
public:
  enum SymbolSpecifier : uint16_t {
    SPECIFIER_NONE,
    SPECIFIER_LO_16 = MCSymbolRefExpr::FirstTargetSpecifier,
    SPECIFIER_HI_16,
  };

private:
  const MCExpr *Expr;
  const SymbolSpecifier Specifier;

  explicit GBMCExpr(const MCExpr *Expr, SymbolSpecifier S)
      : Expr(Expr), Specifier(S) {}

public:
  static const GBMCExpr *create(const MCExpr *Expr, SymbolSpecifier S,
                                   MCContext &Ctx);

  SymbolSpecifier getSpecifier() const { return Specifier; }

  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;

  bool evaluateAsRelocatableImpl(MCValue &Res,
                                 const MCAssembler *Asm) const override;

  void visitUsedExpr(MCStreamer &Streamer) const override;

  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }
};
} // end namespace llvm.

#endif

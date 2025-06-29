#include "GBFixupKinds.h"
#include "GBMCExpr.h"
#include "GBMCTargetDesc.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");

namespace {

class GBMCCodeEmitter : public MCCodeEmitter {
  const MCInstrInfo &MCII;
  MCContext &Ctx;

public:
  GBMCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx)
      : MCII{MCII}, Ctx{Ctx} {}
  ~GBMCCodeEmitter() = default;

  void encodeInstruction(const MCInst &Inst, SmallVectorImpl<char> &CB,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

private:
  // NOTE: this is TableGen'ed
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Return binary encoding of operand
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  /// Return binary encoding of a rst vector operand
  unsigned EncodeRstVecOperand(const MCInst &MI, unsigned OpNo,
                               SmallVectorImpl<MCFixup> &Fixups,
                               const MCSubtargetInfo &STI) const;
};

} // namespace

void GBMCCodeEmitter::encodeInstruction(const MCInst &MI,
                                        SmallVectorImpl<char> &CB,
                                        SmallVectorImpl<MCFixup> &Fixups,
                                        const MCSubtargetInfo &STI) const {

  auto Encoding = getBinaryCodeForInstr(MI, Fixups, STI);
  const auto &Desc = MCII.get(MI.getOpcode());

  for (unsigned I = 0; I < Desc.Size; ++I) {
    CB.push_back(static_cast<char>(Encoding & 0xFF));
    Encoding >>= 8;
  }
  ++MCNumEmitted;
}

unsigned GBMCCodeEmitter::getMachineOpValue(const MCInst &MI,
                                            const MCOperand &MO,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  if (MO.isReg()) {
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
  }

  if (MO.isImm()) {
    return MO.getImm();
  }

  if (MO.isExpr()) {
    const GBMCExpr *GBExpr = dyn_cast<GBMCExpr>(MO.getExpr());
    assert(GBExpr != nullptr);
    const MCExpr *SubExpr = GBExpr->getSubExpr();
    assert(SubExpr->getKind() == MCExpr::Binary ||
           SubExpr->getKind() == MCExpr::SymbolRef ||
           SubExpr->getKind() == MCExpr::Unary);

    const auto &Desc = MCII.get(MI.getOpcode());
    const auto GBCategory =
        (GB::GBFixupCategory)(Desc.TSFlags & GB::FixupTSMask);
    const auto Kind = [&]() -> unsigned {
      switch (GBCategory) {
      case GB::FIXUP_CATEGORY_NONE:
        assert(GBExpr->getSpecifier() == GBMCExpr::SPECIFIER_NONE);
        return MCFixupKind::FK_NONE;
      case GB::FIXUP_CATEGORY_DATA_1: {
        switch (GBExpr->getSpecifier()) {
        case GBMCExpr::SPECIFIER_NONE:
          return MCFixupKind::FK_Data_1;
        case GBMCExpr::SPECIFIER_LO_16:
          return GB::FixupKind::FIXUP_LO_16;
        case GBMCExpr::SPECIFIER_HI_16:
          return GB::FixupKind::FIXUP_HI_16;
        }
      }
      case GB::FIXUP_CATEGORY_DATA_2:
        assert(GBExpr->getSpecifier() == GBMCExpr::SPECIFIER_NONE);
        return MCFixupKind::FK_Data_2;
      case GB::FIXUP_CATEGORY_PCREL_1:
        assert(GBExpr->getSpecifier() == GBMCExpr::SPECIFIER_NONE);
        return MCFixupKind::FK_PCRel_1;
      }
    }();

    if (Kind == MCFixupKind::FK_PCRel_1) {
      SubExpr =
          MCBinaryExpr::createSub(SubExpr, MCConstantExpr::create(1, Ctx), Ctx);
      GBExpr = GBMCExpr::create(SubExpr, GBMCExpr::SPECIFIER_NONE, Ctx);
    }

    // Offset = 1 to skip opcode byte
    Fixups.push_back(
        MCFixup::create(1, GBExpr, (MCFixupKind)Kind, MI.getLoc()));
    return 0;
  }

  llvm_unreachable("Unrecognized operand");
}

unsigned
GBMCCodeEmitter::EncodeRstVecOperand(const MCInst &MI, unsigned OpNo,
                                     SmallVectorImpl<MCFixup> &Fixups,
                                     const MCSubtargetInfo &STI) const {
  const auto &MO = MI.getOperand(OpNo);
  const auto Val = MO.getImm();

  assert((Val & ~0b0111000) == 0);
  return Val >> 3;
}

namespace llvm {
MCCodeEmitter *createGBMCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx) {
  // Ooo, much encapsulation
  return new GBMCCodeEmitter(MCII, Ctx);
}
} // namespace llvm

#include "GBGenMCCodeEmitter.inc"

#include "GBMCTargetDesc.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/bit.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");

namespace {

class GBMCCodeEmitter : public MCCodeEmitter {
  const MCInstrInfo &MCII;
  const MCContext &Ctx;

public:
  GBMCCodeEmitter(const MCInstrInfo &MCII, const MCContext &Ctx)
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

  llvm_unreachable("Unrecognized operand");
  return 0;
}

namespace llvm {
MCCodeEmitter *createGBMCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx) {
  // Ooo, much encapsulation
  return new GBMCCodeEmitter(MCII, Ctx);
}
} // namespace llvm

#include "GBGenMCCodeEmitter.inc"

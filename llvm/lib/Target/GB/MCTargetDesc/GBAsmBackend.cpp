#include "GBFixupKinds.h"
#include "GBMCTargetDesc.h"

#include "llvm/ADT/bit.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_os_ostream.h"

#include <cstdint>
#include <memory>

using namespace llvm;

namespace {

class GBAsmBackend : public MCAsmBackend {
public:
  GBAsmBackend() : MCAsmBackend(endianness::little) {}
  ~GBAsmBackend() override = default;

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  unsigned getNumFixupKinds() const override;

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override;
  bool writeNopData(raw_ostream &OS, uint64_t Count,
                    const MCSubtargetInfo *STI) const override;
};

} // namespace

std::unique_ptr<MCObjectTargetWriter>
GBAsmBackend::createObjectTargetWriter() const {
  return createGBELFObjectWriter();
}

unsigned GBAsmBackend::getNumFixupKinds() const {
  return GB::NumTargetFixupKinds;
}

const MCFixupKindInfo &GBAsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  if (Kind < FirstTargetFixupKind) {
    return MCAsmBackend::getFixupKindInfo(Kind);
  }

  assert((unsigned)(Kind - FirstTargetFixupKind) < getNumFixupKinds());
  return GB::FixupKindInfo[Kind];
}

void GBAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                              const MCValue &Target, MutableArrayRef<char> Data,
                              uint64_t Value, bool IsResolved,
                              const MCSubtargetInfo *STI) const {
  // TODO: what does IsResolved do? Should I read it?
  auto &Ctx = Asm.getContext();
  const auto Kind = Fixup.getKind();
  const auto &Info = getFixupKindInfo(Kind);

  assert(Info.TargetOffset == 0);

  const bool IsSigned = [Kind] {
    switch ((unsigned)Kind) {
    default:
      llvm_unreachable("Unknown fixup kind!");
    case GB::fixup_gb_sprel_8:
    case FK_PCRel_1:
    case FK_PCRel_2:
      return true;
    case GB::fixup_gb_lo8_ff00:
    case FK_Data_1:
    case FK_Data_2:
      return false;
    }
  }();

  if ((unsigned)Kind == GB::fixup_gb_lo8_ff00) {
    if ((Value & 0xff00u) != 0xff00u || Value > 0xffffu) {
      Ctx.reportError(Fixup.getLoc(), "lo8_ff00 fixup value out of range");
    }
    // Discard upper byte since it's already encoded in the instruction
    Value &= 0x00ffu;
  }

  if ((IsSigned && !isIntN(Info.TargetSize, Value)) ||
      (!IsSigned && !isUIntN(Info.TargetSize, Value))) {
    Ctx.reportError(Fixup.getLoc(), "fixup value out of range");
  }

  const auto NumBytes = Info.TargetSize / 8;
  const auto Offset = Fixup.getOffset();
  assert(Offset + NumBytes <= Data.size());

  for (unsigned I = 0; I < NumBytes; I++) {
    assert(Data[Offset + I] == 0);
    Data[Offset + I] = static_cast<uint8_t>(Value & 0xffu);
    Value >>= 8;
  }
}

bool GBAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                                        const MCRelaxableFragment *DF,
                                        const MCAsmLayout &Layout) const {
  return false;
}

bool GBAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count,
                                const MCSubtargetInfo *STI) const {
  while (Count-- != 0) {
    OS.write('\00');
  }
  return true;
}

MCAsmBackend *llvm::createGBAsmBackend(const Target &T,
                                       const MCSubtargetInfo &STI,
                                       const MCRegisterInfo &MRI,
                                       const MCTargetOptions &Options) {
  return new GBAsmBackend();
}

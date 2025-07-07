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

  MCFixupKindInfo getFixupKindInfo(MCFixupKind Kind) const override;

  void applyFixup(const MCFragment &, const MCFixup &, const MCValue &,
                  MutableArrayRef<char> Data, uint64_t FixupValue,
                  bool IsResolved) override;

  bool fixupNeedsRelaxation(const MCFixup &Fixup,
                            uint64_t Value) const override;
  bool writeNopData(raw_ostream &OS, uint64_t Count,
                    const MCSubtargetInfo *STI) const override;
};

} // namespace

std::unique_ptr<MCObjectTargetWriter>
GBAsmBackend::createObjectTargetWriter() const {
  return createGBELFObjectWriter();
}

MCFixupKindInfo GBAsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  switch ((unsigned)Kind) {
  case GB::FIXUP_LO_16:
    return {"GB_FIXUP_LO_16", 0, 8, 0};
  case GB::FIXUP_HI_16:
    return {"GB_FIXUP_HI_16", 0, 8, 0};
  }
  return MCAsmBackend::getFixupKindInfo(Kind);
}

void GBAsmBackend::applyFixup(const MCFragment &F, const MCFixup &Fixup,
                              const MCValue &Target, MutableArrayRef<char> Data,
                              uint64_t Value, bool IsResolved) {
  // TODO: what does IsResolved do? Should I read it?
  auto &Ctx = getContext();
  const auto Kind = Fixup.getKind();
  const auto &Info = getFixupKindInfo(Kind);

  assert(Info.TargetOffset == 0);
  maybeAddReloc(F, Fixup, Target, Value, IsResolved);

  const bool IsSigned = [Kind] {
    switch ((unsigned)Kind) {
    default:
      llvm_unreachable("Unknown fixup kind!");
    case FK_PCRel_1:
      return true;
    case FK_Data_1:
    case FK_Data_2:
      return false;
    case GB::FIXUP_HI_16:
    case GB::FIXUP_LO_16:
      return false;
    // For dwarf information
    case FK_Data_4:
      return false;
    }
  }();

  const size_t NumBytes = Info.TargetSize / 8;
  const size_t Offset = Fixup.getOffset();
  assert(Offset + NumBytes <= Data.size());

  switch ((unsigned)Kind) {
  case GB::FIXUP_HI_16:
    assert(isUIntN(16, Value));
    Value >>= 8;
    break;
  case GB::FIXUP_LO_16:
    assert(isUIntN(16, Value));
    Value &= 0xffu;
    break;
  default:
    break; // Else we're a basic relocation, do not transform
  }

  if ((IsSigned && !isIntN(Info.TargetSize, Value)) ||
      (!IsSigned && !isUIntN(Info.TargetSize, Value))) {
    Ctx.reportError(Fixup.getLoc(), "fixup value out of range");
  }

  for (unsigned I = 0; I < NumBytes; I++) {
    assert(Data[Offset + I] == 0);
    Data[Offset + I] = static_cast<uint8_t>(Value & 0xffu);
    Value >>= 8;
  }

  assert(IsSigned || Value == 0ul);
}

bool GBAsmBackend::fixupNeedsRelaxation(const MCFixup &Fixup,
                                        uint64_t Value) const {
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

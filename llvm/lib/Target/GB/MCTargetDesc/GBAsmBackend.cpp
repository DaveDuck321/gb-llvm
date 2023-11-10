#include "GBMCTargetDesc.h"

#include "llvm/ADT/bit.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/raw_os_ostream.h"

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

unsigned GBAsmBackend::getNumFixupKinds() const { return 1; }

void GBAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                              const MCValue &Target, MutableArrayRef<char> Data,
                              uint64_t Value, bool IsResolved,
                              const MCSubtargetInfo *STI) const {
  // TODO GB
  return;
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

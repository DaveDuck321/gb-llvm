#include "GBFixupKinds.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include <memory>

using namespace llvm;

namespace {

class GBELFObjectWriter : public MCELFObjectTargetWriter {

public:
  GBELFObjectWriter()
      : MCELFObjectTargetWriter(false, ELF::ELFOSABI_NONE, ELF::EM_GB, true) {};

  ~GBELFObjectWriter() override = default;

  unsigned getRelocType(const MCFixup &Fixup, const MCValue &Target,
                        bool IsPCRel) const override;
};

} // namespace

unsigned GBELFObjectWriter::getRelocType(const MCFixup &Fixup,
                                         const MCValue &Target,
                                         bool IsPCRel) const {
  unsigned Kind = Fixup.getTargetKind();
  if (mc::isRelocation(Fixup.getKind())) {
    return Kind;
  }

  switch (Kind) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_PCRel_1:
    assert(IsPCRel);
    return ELF::R_GB_PCREL_8;
  case FK_Data_1:
    assert(not IsPCRel);
    return ELF::R_GB_8;
  case FK_Data_2:
    assert(not IsPCRel);
    return ELF::R_GB_16;
  case FK_Data_4:
    assert(not IsPCRel);
    return ELF::R_GB_DWARF_32;
  case GB::FIXUP_HI_16:
    assert(not IsPCRel);
    return ELF::R_GB_HI_16;
  case GB::FIXUP_LO_16:
    assert(not IsPCRel);
    return ELF::R_GB_LO_16;
  }
}

namespace llvm {
std::unique_ptr<MCObjectTargetWriter> createGBELFObjectWriter() {
  return std::make_unique<GBELFObjectWriter>();
}
} // namespace llvm

#include "GBFixupKinds.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

#include <memory>

using namespace llvm;

namespace {

class GBELFObjectWriter : public MCELFObjectTargetWriter {

public:
  GBELFObjectWriter()
      : MCELFObjectTargetWriter(false, ELF::ELFOSABI_STANDALONE, ELF::EM_GB,
                                false){};

  ~GBELFObjectWriter() override = default;

  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};

} // namespace

unsigned GBELFObjectWriter::getRelocType(MCContext &Ctx, const MCValue &Target,
                                         const MCFixup &Fixup,
                                         bool IsPCRel) const {
  switch ((unsigned)Fixup.getKind()) {
  default:
    llvm_unreachable("Unknown fixup kind!");
  case FK_PCRel_1:
    return ELF::R_GB_PCREL_8;
  case FK_Data_1:
    return ELF::R_GB_8;
  case FK_Data_2:
    return ELF::R_GB_16;
  }
}

namespace llvm {
std::unique_ptr<MCObjectTargetWriter> createGBELFObjectWriter() {
  return std::make_unique<GBELFObjectWriter>();
}
} // namespace llvm

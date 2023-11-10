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
                        const MCFixup &Fixup, bool IsPCRel) const override {

    report_fatal_error("getRelocType is not implemented");
  }
};

} // namespace

namespace llvm {
std::unique_ptr<MCObjectTargetWriter> createGBELFObjectWriter() {
  return std::make_unique<GBELFObjectWriter>();
}
} // namespace llvm

#include "Symbols.h"
#include "Target.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/MathExtras.h"

using namespace llvm;
using namespace llvm::object;
using namespace llvm::support::endian;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {
class GB final : public TargetInfo {
public:
  GB();
  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;

  int64_t getImplicitAddend(const uint8_t *buf, RelType type) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;
};
} // namespace

GB::GB() {
  // STOP 0
  trapInstr = {0x10, 0x00};
}

RelExpr GB::getRelExpr(RelType type, const Symbol &s,
                       const uint8_t *loc) const {
  switch (type) {
  case R_GB_PCREL_8:
    return R_PC;
  default:
    return R_ABS;
  }
}

int64_t GB::getImplicitAddend(const uint8_t *buf, RelType type) const {
  switch (type) {
  case R_GB_8:
    return SignExtend64<8>(*buf);
  case R_GB_PCREL_8:
    return *buf;
  case R_GB_16:
    return read16le(buf);
  default:
    error(getErrorLocation(buf) + "unrecognized relocation " + toString(type));
    return 0;
  }
}

void GB::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_GB_8:
    checkIntUInt(loc, val, 8, rel);
    *loc = val;
    break;
  case R_GB_PCREL_8:
    checkInt(loc, val, 8, rel);
    *loc = val;
    break;
  case R_GB_16:
    checkIntUInt(loc, val, 16, rel);
    write16le(loc, val);
    break;
  default:
    error(getErrorLocation(loc) + "unrecognized relocation " +
          toString(rel.type));
  }
}

TargetInfo *elf::getGBTargetInfo() {
  static GB target;
  return &target;
}

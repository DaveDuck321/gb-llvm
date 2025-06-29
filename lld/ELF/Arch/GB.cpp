#include "Symbols.h"
#include "Target.h"
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
  GB(Ctx &);
  RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;

  int64_t getImplicitAddend(const uint8_t *buf, RelType type) const override;
  void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;
};
} // namespace

GB::GB(Ctx &ctx) : TargetInfo{ctx} {
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
  case R_GB_HI_16:
  case R_GB_LO_16:
    return *buf;
  case R_GB_PCREL_8:
    return SignExtend64<8>(*buf);
  case R_GB_16:
    return read16le(buf);
  case R_GB_DWARF_32:
    return SignExtend64<32>(read32(ctx, buf));
  default:
    InternalErr(ctx, buf) << "unrecognized relocation " << type;
    return 0;
  }
}

void GB::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_GB_8:
    checkIntUInt(ctx, loc, val, 8, rel);
    *loc = val;
    break;
  case R_GB_HI_16:
    checkIntUInt(ctx, loc, val >> 8U, 8, rel);
    *loc = (val >> 8U);
    break;
  case R_GB_LO_16:
    checkIntUInt(ctx, loc, val, 16, rel);
    checkIntUInt(ctx, loc, val & 0xFFU, 8, rel);
    *loc = (val & 0xFFU);
    break;
  case R_GB_PCREL_8:
    checkInt(ctx, loc, val, 8, rel);
    *loc = val;
    break;
  case R_GB_16:
    checkIntUInt(ctx, loc, val, 16, rel);
    write16le(loc, val);
    break;
  case R_GB_DWARF_32:
    write32(ctx, loc, val);
    break;
  default:
    InternalErr(ctx, loc) << "unrecognized relocation " << rel.type;
  }
}

void elf::setGBTargetInfo(Ctx &ctx) { ctx.target.reset(new GB(ctx)); }

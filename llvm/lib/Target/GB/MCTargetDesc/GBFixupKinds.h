#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#include <cstdint>

namespace llvm::GB {
enum FixupKind {
  FIXUP_HI_16 = FirstTargetFixupKind,
  FIXUP_LO_16,
};

// Must be synced with tablegen
enum GBFixupCategory {
  FIXUP_CATEGORY_NONE = 0,
  FIXUP_CATEGORY_DATA_1 = 1,
  FIXUP_CATEGORY_DATA_2 = 2,
  FIXUP_CATEGORY_PCREL_1 = 3,
};

static inline constexpr uint64_t FixupTSMask = 0b11;
} // namespace llvm::GB

#endif

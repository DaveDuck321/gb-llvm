#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#include <cstdint>

namespace llvm::GB {
// Must be synced with tablegen
static inline constexpr uint64_t FixupTSMask = 0b11;
static inline constexpr unsigned FixupKindMap[] = {
    FK_NONE,
    FK_Data_1,
    FK_Data_2,
    FK_PCRel_1,
};
} // namespace llvm::GB

#endif

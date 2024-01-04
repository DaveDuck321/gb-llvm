#ifndef LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H
#define LLVM_LIB_TARGET_GB_MCTARGETDESC_GBFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCFixupKindInfo.h"

#include <cstdint>

namespace llvm::GB {
// Target-specific fixups
// TODO GB: these probably aren't even necessary, remove when possible
enum Fixups {
  // 8-bit unsigned fixup for the upper byte of the address space
  // eg. LDH (a8),A
  fixup_gb_lo8_ff00 = FirstTargetFixupKind,

  // 8-bit signed fixup relative to the stack pointer
  // eg. LD HL,SP+r8
  fixup_gb_sprel_8,

  // Sentinel values
  fixup_gb_invalid,
  NumTargetFixupKinds = fixup_gb_invalid - FirstTargetFixupKind
};

static inline constexpr MCFixupKindInfo FixupKindInfo[] = {
    {"fixup_gb_lo8_ff00", 0, 8, 0},
    {"fixup_gb_sprel_8", 0, 8, 0},
};

// Must be synced with tablegen
static inline constexpr uint64_t FixupTSMask = 0b111;
static inline constexpr unsigned FixupKindMap[] = {
    FK_NONE,    FK_Data_1,         FK_Data_2,        FK_PCRel_1,
    FK_PCRel_2, fixup_gb_lo8_ff00, fixup_gb_sprel_8,
};
} // namespace llvm::GB

#endif

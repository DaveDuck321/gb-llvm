#pragma once

#include "llvm/CodeGen/MachineOperand.h"

namespace llvm {
enum class GBMOFlag : unsigned {
  NONE = 0,
  UPPER_PART = 1,
  LOWER_PART = 2,
  _last
};

inline GBMOFlag getGBFlag(MachineOperand const &MO) {
  auto const Flag = MO.getTargetFlags();
  assert(0 <= Flag && Flag <= static_cast<unsigned>(GBMOFlag::_last));
  return static_cast<GBMOFlag>(Flag);
}

inline void setGBFlag(MachineOperand &MO, GBMOFlag Flag) {
  MO.setTargetFlags(static_cast<unsigned>(Flag));
}
} // namespace llvm

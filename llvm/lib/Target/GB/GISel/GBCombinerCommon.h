#pragma once

#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/MachineInstr.h"

#include <functional>

namespace llvm::gb {
bool matchCombineICMPToUnsigned(
    MachineInstr &MI, std::function<void(MachineIRBuilder &)> &MatchInfo);
} // namespace llvm::gb

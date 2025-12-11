#ifndef LLVM_LIB_TARGET_GB_GISEL_GBREGISTERBANKINFO_H
#define LLVM_LIB_TARGET_GB_GISEL_GBREGISTERBANKINFO_H

#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/RegisterBankInfo.h"

#define GET_REGBANK_DECLARATIONS
#include "GBGenRegisterBank.inc"

namespace llvm {

class TargetRegisterInfo;

class GBGenRegisterBankInfo : public RegisterBankInfo {
protected:
#define GET_TARGET_REGBANK_CLASS
#include "GBGenRegisterBank.inc"
};

class GBRegisterBankInfo : public GBGenRegisterBankInfo {
  const InstructionMapping &
  getInstrMapping(const MachineInstr &MI) const override;
};
} // namespace llvm

#endif

#include "GBRegisterBankInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_TARGET_REGBANK_IMPL
#include "GBGenRegisterBank.inc"

using namespace llvm;

namespace {
const RegisterBankInfo::PartialMapping GPR8PartialMapping = {0, 8,
                                                             GB::GPRRegBank};
const RegisterBankInfo::PartialMapping GPR16PartialMapping = {0, 16,
                                                              GB::GPRRegBank};
const RegisterBankInfo::PartialMapping DebugPartialMapping = {
    0, 128, GB::DebugOnlyRegBank};

const RegisterBankInfo::ValueMapping InvalidMapping = {nullptr, 0};
const RegisterBankInfo::ValueMapping GPR8Mapping = {&GPR8PartialMapping, 1};
const RegisterBankInfo::ValueMapping GPR16Mapping = {&GPR16PartialMapping, 1};
const RegisterBankInfo::ValueMapping DebugMapping = {&DebugPartialMapping, 1};
} // namespace

const GBRegisterBankInfo::InstructionMapping &
GBRegisterBankInfo::getInstrMapping(const MachineInstr &MI) const {
  const auto *MF = MI.getMF();
  auto &MRI = MF->getRegInfo();

  if (const InstructionMapping &Mapping = getInstrMappingImpl(MI);
      Mapping.isValid()) {
    return Mapping;
  }

  // We have no register banks: unconditionally map everything to GPRB
  SmallVector<const ValueMapping *, 8> Mapping;
  for (auto Operand : MI.operands()) {
    if (not Operand.isReg()) {
      Mapping.push_back(&InvalidMapping);
      continue;
    }

    auto Type = MRI.getType(Operand.getReg());
    switch (Type.getScalarSizeInBits()) {
    case 8:
      Mapping.push_back(&GPR8Mapping);
      break;
    case 16:
      Mapping.push_back(&GPR16Mapping);
      break;
    default:
      assert(MI.getOpcode() == TargetOpcode::DBG_VALUE);
      Mapping.push_back(&DebugMapping);
      break;
    }
  }

  return getInstructionMapping(DefaultMappingID, 1, getOperandsMapping(Mapping),
                               Mapping.size());
}

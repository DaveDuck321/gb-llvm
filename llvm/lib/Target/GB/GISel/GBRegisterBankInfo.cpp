#include "GBRegisterBankInfo.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_TARGET_REGBANK_IMPL
#include "GBGenRegisterBank.inc"

using namespace llvm;

namespace {
const RegisterBankInfo::PartialMapping GPR8PartialMapping = {0, 8,
                                                             GB::GPRRegBank};
const RegisterBankInfo::PartialMapping GPR16PartialMapping = {0, 16,
                                                              GB::GPRRegBank};

const RegisterBankInfo::ValueMapping InvalidMapping = {nullptr, 0};
const RegisterBankInfo::ValueMapping GPR8Mapping = {&GPR8PartialMapping, 1};
const RegisterBankInfo::ValueMapping GPR16Mapping = {&GPR16PartialMapping, 1};
} // namespace

const GBRegisterBankInfo::InstructionMapping &
GBRegisterBankInfo::getInstrMapping(const MachineInstr &MI) const {
  const unsigned Opc = MI.getOpcode();
  unsigned NumOperands = MI.getNumOperands();

  if (const InstructionMapping &Mapping = getInstrMappingImpl(MI);
      Mapping.isValid()) {
    return Mapping;
  }

  SmallVector<const ValueMapping *, 8> Mapping;
  switch (Opc) {
  default:
    MI.dump();
    llvm_unreachable("Unrecognized opcode");

  case TargetOpcode::G_PTRTOINT:
  case TargetOpcode::G_INTTOPTR:
  case TargetOpcode::G_FREEZE:
    Mapping.push_back(&GPR16Mapping);
    Mapping.push_back(&GPR16Mapping);
    break;

  case TargetOpcode::G_ADD:
  case TargetOpcode::G_SUB:
  case TargetOpcode::G_AND:
  case TargetOpcode::G_OR:
  case TargetOpcode::G_XOR:
  case TargetOpcode::G_SHL:
    // Result
    Mapping.push_back(&GPR8Mapping);

    // Ops
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR8Mapping);
    break;

  case TargetOpcode::G_STORE:
    // Ops
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR16Mapping);
    break;

  case TargetOpcode::G_LOAD:
    // Result
    Mapping.push_back(&GPR8Mapping);

    // Ops
    Mapping.push_back(&GPR16Mapping);
    break;

  case TargetOpcode::G_MERGE_VALUES:
    // Result
    Mapping.push_back(&GPR16Mapping);

    // Ops
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR8Mapping);
    break;

  case TargetOpcode::G_UNMERGE_VALUES:
    // Results
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR8Mapping);

    // Ops
    Mapping.push_back(&GPR16Mapping);
    break;

  case TargetOpcode::G_EXTRACT:
    // Result
    Mapping.push_back(&GPR8Mapping);

    // Ops
    Mapping.push_back(&GPR16Mapping);
    Mapping.push_back(&InvalidMapping);
    break;

  case TargetOpcode::G_ICMP:
    // Result
    Mapping.push_back(&GPR8Mapping);

    // Ops
    Mapping.push_back(&InvalidMapping);
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR8Mapping);
    break;

  case GB::G_JP_CP:
    // Ops
    Mapping.push_back(&InvalidMapping);
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&GPR8Mapping);
    Mapping.push_back(&InvalidMapping);
    break;

  case TargetOpcode::G_BLOCK_ADDR:
  case TargetOpcode::G_GLOBAL_VALUE:
  case TargetOpcode::G_CONSTANT:
  case TargetOpcode::G_FRAME_INDEX:
    // Result
    Mapping.push_back(&GPR16Mapping);

    // Op
    Mapping.push_back(&InvalidMapping);
    break;
  case TargetOpcode::G_BRINDIRECT:
    // Op
    Mapping.push_back(&GPR16Mapping);
    break;
  }

  return getInstructionMapping(DefaultMappingID, 1, getOperandsMapping(Mapping),
                               NumOperands);
}

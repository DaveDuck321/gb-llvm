#include "GB.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/GlobalISel/Combiner.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/ErrorHandling.h"

#include <iterator>
#include <map>
#include <optional>

#define DEBUG_TYPE "gb-increment-serialize"

using namespace llvm;

namespace {
struct ParsedAdd16 {
  MachineInstr *MI;
  Register Dst;
  Register Base;
  Register ImmReg;
  long Imm;
};

class GBIncrementSerialize : public MachineFunctionPass {
public:
  static char ID;

  GBIncrementSerialize() : MachineFunctionPass(ID) {}

  StringRef getPassName() const override { return "GBIncrementSerialize"; }

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  bool parallelizeIncrements(MachineFunction &MF, MachineBasicBlock &MBB);
  bool serializeIncrements(MachineFunction &MF, MachineBasicBlock &MBB);

  bool serializeFromGlobalValueBase(MachineFunction &MF,
                                    MachineRegisterInfo &MRI, Register Base,
                                    std::vector<ParsedAdd16> const &Offsets);
  bool serializeFromRegisterBase(MachineFunction &MF, MachineRegisterInfo &MRI,
                                 Register Base,
                                 std::vector<ParsedAdd16> const &Offsets);
};

std::optional<ParsedAdd16> parseAdd16Immediate(MachineRegisterInfo &MRI,
                                               MachineInstr &MI) {
  if (MI.getOpcode() != TargetOpcode::G_ADD &&
      MI.getOpcode() != TargetOpcode::G_PTR_ADD) {
    return std::nullopt;
  }

  auto Dst = MI.getOperand(0).getReg();
  auto LHS = MI.getOperand(1).getReg();
  auto RHS = MI.getOperand(2).getReg();
  if (MRI.getType(Dst).getScalarSizeInBits() != 16) {
    return std::nullopt;
  }

  auto RHSImm = getIConstantVRegValWithLookThrough(RHS, MRI);
  if (!RHSImm.has_value()) {
    return std::nullopt;
  }

  return ParsedAdd16{
      /*MI=*/&MI,
      /*Dst=*/Dst,
      /*Base=*/LHS,
      /*ImmReg=*/RHSImm->VReg,
      /*Imm=*/RHSImm->Value.getSExtValue(),
  };
}

bool GBIncrementSerialize::parallelizeIncrements(MachineFunction &MF,
                                                 MachineBasicBlock &MBB) {
  auto &MRI = MF.getRegInfo();

  for (auto &MI : MBB) {
    // We want to transform: `(add (add, base, imm1), imm2)`
    // Into:                 `(add, base, imm1), (add base, imm1+imm2)`

    auto ParsedMI = parseAdd16Immediate(MRI, MI);
    if (!ParsedMI.has_value()) {
      continue;
    }

    auto *BaseDefMI = getDefIgnoringCopies(ParsedMI->Base, MRI);
    assert(BaseDefMI != nullptr);
    if (MI.getOpcode() != BaseDefMI->getOpcode()) {
      continue;
    }

    auto ParsedBaseMI = parseAdd16Immediate(MRI, *BaseDefMI);
    if (!ParsedBaseMI.has_value()) {
      continue;
    }

    MachineIRBuilder MIB{MI};
    auto NewImm = MRI.cloneVirtualRegister(ParsedMI->ImmReg);
    MIB.buildConstant(NewImm, ParsedBaseMI->Imm + ParsedMI->Imm);
    MIB.buildInstr(MI.getOpcode(), {ParsedMI->Dst},
                   {ParsedBaseMI->Base, NewImm}, MI.getFlags());
    MI.eraseFromParent();
    return true;
  }
  return false;
}

bool GBIncrementSerialize::serializeFromGlobalValueBase(
    MachineFunction &MF, MachineRegisterInfo &MRI, Register Base,
    std::vector<ParsedAdd16> const &Offsets) {
  auto *BaseDef = MRI.getOneDef(Base);
  if (BaseDef == nullptr) {
    return false;
  }

  auto &BaseDefMI = *BaseDef->getParent();
  if (BaseDefMI.getOpcode() != TargetOpcode::G_GLOBAL_VALUE) {
    return false;
  }

  auto const *GlobalAddress = BaseDefMI.getOperand(1).getGlobal();
  for (auto const &ParsedAdd : Offsets) {
    auto &MI = *ParsedAdd.MI;

    MachineIRBuilder MIB{MI};
    MIB.buildInstr(TargetOpcode::G_GLOBAL_VALUE)
        .addDef(ParsedAdd.Dst)
        .addGlobalAddress(GlobalAddress, ParsedAdd.Imm);
    MI.eraseFromParent();
  }
  return true;
}

bool GBIncrementSerialize::serializeFromRegisterBase(
    MachineFunction &MF, MachineRegisterInfo &MRI, Register Base,
    std::vector<ParsedAdd16> const &Offsets) {
  size_t CurrentOffset = 0;
  Register CurrentReg = Base;
  for (auto const &ParsedAdd : Offsets) {
    auto &MI = *ParsedAdd.MI;

    MachineIRBuilder MIB{MI};
    auto NewImm = MRI.cloneVirtualRegister(ParsedAdd.ImmReg);
    MIB.buildConstant(NewImm, ParsedAdd.Imm - CurrentOffset);
    MIB.buildInstr(MI.getOpcode(), {ParsedAdd.Dst}, {CurrentReg, NewImm},
                   MI.getFlags());
    MI.eraseFromParent();

    CurrentOffset = ParsedAdd.Imm;
    CurrentReg = ParsedAdd.Dst;
  }
  return true;
}

bool GBIncrementSerialize::serializeIncrements(MachineFunction &MF,
                                               MachineBasicBlock &MBB) {
  auto &MRI = MF.getRegInfo();

  std::map<Register, std::vector<ParsedAdd16>> ConstantAdditions;
  for (auto &MI : MBB) {
    auto ParsedMI = parseAdd16Immediate(MRI, MI);
    if (ParsedMI.has_value()) {
      ConstantAdditions[ParsedMI->Base].push_back(*ParsedMI);
    }
  }

  bool HasMadeChanges = false;
  for (auto &[Base, OffsetsFromBase] : ConstantAdditions) {
    if (serializeFromGlobalValueBase(MF, MRI, Base, OffsetsFromBase)) {
      HasMadeChanges = true;
      continue;
    }

    if (serializeFromRegisterBase(MF, MRI, Base, OffsetsFromBase)) {
      HasMadeChanges = true;
      continue;
    }
  }
  return HasMadeChanges;
}

bool GBIncrementSerialize::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "======== GBIncrementSerialize ========\n";
             dbgs() << "Starting with: "; MF.dump());

  bool DidMakeChanges = false;

  while (true) {
    bool DidParallelize = false;
    for (auto &MBB : MF) {
      DidParallelize |= parallelizeIncrements(MF, MBB);
    }

    DidMakeChanges |= DidParallelize;
    if (not DidParallelize) {
      break;
    }
  }

  for (auto &MBB : MF) {
    DidMakeChanges |= serializeIncrements(MF, MBB);
  }

  LLVM_DEBUG(
      dbgs() << "After GBIncrementSerialize:\n";
      if (DidMakeChanges) { MF.dump(); } else { dbgs() << "No change!\n"; });

  return DidMakeChanges;
}
} // namespace

char GBIncrementSerialize::ID = 0;
FunctionPass *llvm::createGBIncrementSerialize(CodeGenOptLevel) {
  return new GBIncrementSerialize();
}

#include "GB.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCDwarf.h"

#include <map>
#include <optional>

using namespace llvm;

#define DEBUG_TYPE "gb-cfi-inserter"

namespace {
class GBCFIInserter final : public MachineFunctionPass {
public:
  static char ID;

  GBCFIInserter() : MachineFunctionPass(ID) {}

  StringRef getPassName() const override { return "GB CFI inserter"; }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

MachineInstr::MIFlag copyFlagsFrom(MachineInstr &MI) {
  static constexpr unsigned MIFlagMask =
      MachineInstr::FrameSetup | MachineInstr::FrameDestroy;
  return static_cast<MachineInstr::MIFlag>(MI.getFlags() & MIFlagMask);
}

void buildCFI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
              const DebugLoc &DL, const MCCFIInstruction &CFIInst,
              MachineInstr::MIFlag Flag) {
  MachineFunction &MF = *MBB.getParent();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const TargetInstrInfo &TII = *STI.getInstrInfo();

  unsigned CFIIndex = MF.addFrameInst(CFIInst);
  BuildMI(MBB, MBBI, DL, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex)
      .setMIFlag(Flag);
}

struct MBBStackOffset {
  long StartStackOffset;
  std::optional<long> EndStackOffset = std::nullopt;
};
} // namespace

bool GBCFIInserter::runOnMachineFunction(MachineFunction &MF) {
  static constexpr long StackGrowth = -2;

  std::map<MachineBasicBlock *, MBBStackOffset> OffsetMapping;
  std::vector<MachineBasicBlock *> Worklist;

  MachineBasicBlock *EntryPoint = &*MF.begin();
  OffsetMapping.emplace(EntryPoint, MBBStackOffset{StackGrowth});
  Worklist.push_back(EntryPoint);

  while (!Worklist.empty()) {
    auto *MBB = Worklist.back();
    Worklist.pop_back();

    auto *StackOffset = &OffsetMapping.at(MBB);

    // Add CFIs to the BB if the stack offset changes
    long CurrentOffset = StackOffset->StartStackOffset;
    for (auto &MI : *MBB) {
      if (MI.isTerminator()) {
        break; // Nothing is allowed after the first terminator
      }
      switch (MI.getOpcode()) {
      default:
        continue;
      case GB::ADD_SP:
        CurrentOffset += static_cast<int8_t>(MI.getOperand(0).getImm());
        break;
      case GB::PUSH:
        CurrentOffset += StackGrowth;
        break;
      case GB::POP:
        CurrentOffset -= StackGrowth;
        break;
      }
      buildCFI(*MBB, ++MI.getIterator(), MI.getDebugLoc(),
               MCCFIInstruction::cfiDefCfaOffset(nullptr, -CurrentOffset),
               copyFlagsFrom(MI));
    }

    if (MBB->begin() != MBB->end()) {
      MachineInstr &FirstMI = *MBB->begin();
      if (!FirstMI.isTerminator()) {
        // Order this initial CFI before any CFIs inserted in the above loop
        buildCFI(*MBB, ++FirstMI.getIterator(), FirstMI.getDebugLoc(),
                 MCCFIInstruction::cfiDefCfaOffset(
                     nullptr, -StackOffset->StartStackOffset),
                 copyFlagsFrom(FirstMI));
      }
    }

    StackOffset->EndStackOffset = CurrentOffset;
    for (MachineBasicBlock *Successor : MBB->successors()) {
      auto Iter = OffsetMapping.find(Successor);
      if (Iter == OffsetMapping.end()) {
        // This Successor hasn't been in our worklist, add it
        OffsetMapping.emplace(Successor, MBBStackOffset{CurrentOffset});
        Worklist.push_back(Successor);
      } else {
        // Otherwise confirm that everything is consistent
        assert(Iter->second.StartStackOffset == CurrentOffset);
      }
    }
  }
  return true;
}

char GBCFIInserter::ID = 0;
FunctionPass *llvm::createGBCFIInserter() { return new GBCFIInserter(); }

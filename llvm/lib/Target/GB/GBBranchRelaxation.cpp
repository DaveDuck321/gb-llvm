#include "GB.h"
#include "GBSubtarget.h"
#include "GBTargetMachine.h"

#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "gb-branch-relaxation"

static cl::opt<bool>
    GBDisableBranchRelaxation("gb-disable-branch-relaxation", cl::Hidden,
                              cl::desc("Disables GBBranchRelaxation"));

namespace {
// Unlike the LLVM branch relaxation, we will never reorder blocks (since we
// don't have to). Instead we will materialize jumps to absolute addresses.
// Instruction selection logic will ONLY produce `jp (cc)` since this is always
// valid. We attempt to tighten these absolute jumps into relative jumps.
class GBBranchRelaxation final : public MachineFunctionPass {
  GBTargetMachine const &TM;

  GBSubtarget const *ST = nullptr;
  TargetInstrInfo const *TII = nullptr;
  MachineFunction *MF = nullptr;

  struct BasicBlockInfo {
    int64_t Offset = 0; // From MF
    int64_t Size = 0;

    // Straight from llvm/lib/CodeGen/BranchRelaxation.cpp
    /// Compute the offset immediately following this block. \p MBB is the next
    /// block.
    int64_t postOffset(const MachineBasicBlock &MBB) const {
      const int64_t PO = Offset + Size;
      const Align Alignment = MBB.getAlignment();
      const Align ParentAlign = MBB.getParent()->getAlignment();
      if (Alignment <= ParentAlign)
        return alignTo(PO, Alignment);

      // The alignment of this MBB is larger than the function's alignment, so
      // we can't tell whether or not it will insert nops. Assume that it will.
      return alignTo(PO, Alignment) + Alignment.value() - ParentAlign.value();
    }
  };

  SmallVector<BasicBlockInfo, 16> BlockInfo;

public:
  static char ID;

  GBBranchRelaxation(GBTargetMachine &TM, CodeGenOptLevel OptLevel)
      : MachineFunctionPass(ID), TM{TM} {}

  StringRef getPassName() const override { return "GB Branch Relaxation"; }

  bool runOnMachineFunction(MachineFunction &MF) override;

  int64_t computeBlockSize(MachineBasicBlock &MBB) const {
    int64_t Size = 0;
    for (const MachineInstr &MI : MBB) {
      Size += TII->getInstSizeInBytes(MI);
    }
    return Size;
  }

  void scanFunction();
  void adjustBlockOffsets(MachineFunction::iterator);
  void checkBranchesAreLegal();
  void checkInvariantsHold();

  bool tightenBranches();
};
} // namespace

void GBBranchRelaxation::scanFunction() {
  BlockInfo.clear();
  BlockInfo.resize(MF->getNumBlockIDs());

  for (MachineBasicBlock &MBB : *MF) {
    BlockInfo[MBB.getNumber()].Size = computeBlockSize(MBB);
  }

  adjustBlockOffsets(MF->begin());
}

void GBBranchRelaxation::adjustBlockOffsets(MachineFunction::iterator Start) {
  auto End = MF->end();

  unsigned PrevBlockNum = Start->getNumber();
  for (auto &MBB : make_range(std::next(Start), End)) {
    unsigned ThisBlockNum = MBB.getNumber();
    // Get the offset and known bits at the end of the layout predecessor.
    // Include the alignment of the current block.
    BlockInfo[ThisBlockNum].Offset = BlockInfo[PrevBlockNum].postOffset(MBB);

    PrevBlockNum = ThisBlockNum;
  }
}

void GBBranchRelaxation::checkBranchesAreLegal() {
  for (auto &MBB : *MF) {
    int64_t ThisMBBOffset = BlockInfo[MBB.getNumber()].Offset;

    int64_t OffsetInBB = 0;
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (!MI.isBranch() || MI.isIndirectBranch()) {
        OffsetInBB += TII->getInstSizeInBytes(MI);
        continue;
      }

      auto *Target = TII->getBranchDestBlock(MI);
      int64_t TargetOffset = BlockInfo[Target->getNumber()].Offset;
      int64_t ThisOffset = ThisMBBOffset + OffsetInBB;
      int64_t BranchOffset = TargetOffset - ThisOffset;
      assert(TII->isBranchOffsetInRange(MI.getOpcode(), BranchOffset));
      OffsetInBB += TII->getInstSizeInBytes(MI);
    }
    assert(BlockInfo[MBB.getNumber()].Size == OffsetInBB);
  }
}

bool GBBranchRelaxation::tightenBranches() {
  bool DidTighten = false;
  for (auto &MBB : *MF) {
    bool DidUpdateBlock = false;
    int64_t ThisMBBOffset = BlockInfo[MBB.getNumber()].Offset;
    int64_t OffsetInBB = 0;
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (!MI.isBranch() || MI.isIndirectBranch()) {
        OffsetInBB += TII->getInstSizeInBytes(MI);
        continue;
      }

      auto *TargetBlock = TII->getBranchDestBlock(MI);
      int64_t TargetOffset = BlockInfo[TargetBlock->getNumber()].Offset;
      int64_t ThisOffset = ThisMBBOffset + OffsetInBB;
      int64_t BranchPCRelativeOffset = TargetOffset - ThisOffset;

      int64_t SizeBefore = TII->getInstSizeInBytes(MI);
      bool DidUpdate = TII->tightenBranchIfPossible(MI, BranchPCRelativeOffset);
      int64_t SizeAfter = TII->getInstSizeInBytes(MI);

      OffsetInBB += SizeAfter;

      if (DidUpdate) {
        DidTighten = true;
        DidUpdateBlock = true;
        assert(SizeBefore - SizeAfter > 0);
        BlockInfo[MBB.getNumber()].Size -= (SizeBefore - SizeAfter);
      } else {
        assert(SizeBefore == SizeAfter);
      }
    }
    assert(BlockInfo[MBB.getNumber()].Size == OffsetInBB);

    if (DidUpdateBlock) {
      adjustBlockOffsets(MBB.getIterator());
    }
  }
  return DidTighten;
}

void GBBranchRelaxation::checkInvariantsHold() {
  SmallVector<BasicBlockInfo, 16> BlockInfoAfterAnalysis = BlockInfo;
  scanFunction();

  assert(BlockInfoAfterAnalysis.size() == BlockInfo.size());
  for (size_t I = 0; I < BlockInfo.size(); I += 1) {
    assert(BlockInfo[I].Size == BlockInfoAfterAnalysis[I].Size);
    assert(BlockInfo[I].Offset == BlockInfoAfterAnalysis[I].Offset);
  }
}

bool GBBranchRelaxation::runOnMachineFunction(
    MachineFunction &MachineFunction) {
  if (GBDisableBranchRelaxation) {
    return false;
  }

  MF = &MachineFunction;
  ST = TM.getGBSubtargetImpl(MF->getFunction());
  TII = ST->getInstrInfo();

  // Renumber all of the machine basic blocks in the function, guaranteeing that
  // the numbers agree with the position of the block in the function.
  MF->RenumberBlocks();

  bool MadeChanges = false;

  scanFunction();
  checkBranchesAreLegal();
  while (tightenBranches()) {
    MadeChanges = true;
  }
  checkBranchesAreLegal();
  checkInvariantsHold();

  return MadeChanges;
}

char GBBranchRelaxation::ID = 0;
FunctionPass *llvm::createGBBranchRelaxation(GBTargetMachine &TM,
                                             CodeGenOptLevel OptLevel) {
  return new GBBranchRelaxation(TM, OptLevel);
}

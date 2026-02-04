#include "GB.h"
#include "GBInstrInfo.h"

#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/RDFLiveness.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "gb-post-ra-compare-merge"

static cl::opt<bool>
    GBDisablePostRACompareMerge("gb-disable-post-ra-compare-merge", cl::Hidden,
                                cl::desc("Disables GBPostRACompareMerge"));

namespace {
class GBPostRACompareMerge final : public MachineFunctionPass {
public:
  static char ID;

  GBPostRACompareMerge(CodeGenOptLevel OptLevel) : MachineFunctionPass(ID) {}

  StringRef getPassName() const override { return "GBPostRACompareMerge"; }

  bool optimizeCompareInstructions(MachineFunction &MF, MachineBasicBlock &MBB);
  bool runOnMachineFunction(MachineFunction &MF) override;
};

bool GBPostRACompareMerge ::optimizeCompareInstructions(
    MachineFunction &MF, MachineBasicBlock &MBB) {
  const auto &TII =
      *static_cast<const GBInstrInfo *>(MF.getSubtarget().getInstrInfo());

  auto &TRI = *MF.getSubtarget().getRegisterInfo();

  // We're run very late CP_ZERO has already been lowered...
  // Try to detect and remove redundant alu, inc, dec, br
  Register Reg = {};
  bool HasSeenInc = false;
  MachineInstr *Focus = nullptr;
  MachineInstr *Inc = nullptr;
  MachineInstr *Dec = nullptr;

  auto ResetState = [&] {
    Reg = Register();
    HasSeenInc = false;
    Focus = nullptr;
    Inc = nullptr;
    Dec = nullptr;
  };

  for (auto &MI : MBB) {
    if (Reg.isValid()) {
      if (not HasSeenInc) {
        if (MI.getOpcode() == GB::INC_r && MI.getOperand(0).getReg() == Reg) {
          HasSeenInc = true;
          Inc = &MI;
          continue;
        }
      } else {
        if (MI.getOpcode() == GB::DEC_r && MI.getOperand(0).getReg() == Reg) {
          Dec = &MI;
          LLVM_DEBUG(dbgs() << "Matched op, inc, dec!\n");
          break; // Found full pattern
        }

        if (MI.getOpcode() == GB::INC_r && MI.getOperand(0).getReg() == Reg) {
          // If we see a second inc, the pattern has failed. But we don't need
          // to reset all the way back to the beginning.
          Focus = Inc;
          Inc = &MI;
          continue;
        }
      }
    }

    if (auto MaybeReg = TII.doesSetZeroFlag(MI)) {
      ResetState();
      Reg = *MaybeReg;
      Focus = &MI;
    } else if (MI.definesRegister(GB::F, &TRI) ||
               MI.hasUnmodeledSideEffects()) {
      ResetState();
    }
  }

  if (Dec == nullptr || Inc == nullptr || Focus == nullptr) {
    return false;
  }

  // Confirm that the first instruction to use the flag is a Z or NZ branch
  bool DidMatch = false;
  for (auto I = Dec->getIterator(); I != MBB.end(); ++I) {
    if (&*I == Dec) {
      continue;
    }
    if (I->isConditionalBranch()) {
      auto Flag = I->getOperand(0).getImm();
      if (Flag == GBFlag::Z || Flag == GBFlag::NZ) {
        LLVM_DEBUG(dbgs() << " Matched branch!\n");
        DidMatch = true;
        break;
      }
      LLVM_DEBUG(dbgs() << " Failed match on branch!\n");
      return false;
    }

    if (I->readsRegister(GB::F, &TRI)) {
      LLVM_DEBUG(dbgs() << " Failed match on instruction!\n");
      return false;
    }
  }

  if (not DidMatch) {
    return false;
  }

  LLVM_DEBUG(dbgs() << " Removing Inc, Dec\n");
  Dec->removeFromParent();
  Inc->removeFromParent();
  for (auto &Operand : Focus->implicit_operands()) {
    if (Operand.isDef() && Operand.isReg() && Operand.getReg() == GB::F) {
      Operand.setIsDead(false);
    }
  }
  return true;
}

bool GBPostRACompareMerge::runOnMachineFunction(MachineFunction &MF) {
  if (GBDisablePostRACompareMerge) {
    return false;
  }

  LLVM_DEBUG(dbgs() << "******* GBPostRACompareMerge ****** \n";
             MF.print(dbgs()));

  bool MadeChanges = false;
  bool DidChangeAnyMBB = false;
  do {
    DidChangeAnyMBB = false;
    for (auto &MBB : MF) {
      DidChangeAnyMBB |= optimizeCompareInstructions(MF, MBB);
    }
    MadeChanges |= DidChangeAnyMBB;
  } while (DidChangeAnyMBB);

  if (MadeChanges) {
    LLVM_DEBUG(MF.print(dbgs()));
  }
  return MadeChanges;
}
} // namespace

char GBPostRACompareMerge::ID = 0;
FunctionPass *llvm::createGBPostRACompareMerge(CodeGenOptLevel OptLevel) {
  return new GBPostRACompareMerge(OptLevel);
}

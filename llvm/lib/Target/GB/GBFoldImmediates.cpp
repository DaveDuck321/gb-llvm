#include "GB.h"
#include "GBSubtarget.h"

#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/Register.h"

#include <map>

using namespace llvm;

#define DEBUG_TYPE "gb-fold-immediates"

static cl::opt<bool>
    GBDisableBranchRelaxation("gb-disable-fold-immediates", cl::Hidden,
                              cl::desc("Disables GBFoldImmediates"));

namespace {
class GBFoldImmediates : public MachineFunctionPass {
public:
  static char ID;

  GBFoldImmediates() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return "GB Fold Immediates"; }
};
} // end anonymous namespace

bool GBFoldImmediates::runOnMachineFunction(MachineFunction &MF) {
  const auto &Subtarget = MF.getSubtarget();
  const auto *TII = Subtarget.getInstrInfo();
  auto &MRI = MF.getRegInfo();

  bool DidMakeChanges = false;

  // Collect the sources for all move immediate instructions
  std::map<Register, MachineInstr *> Immediates;
  for (auto &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (not MI.isMoveImmediate()) {
        continue;
      }

      auto Reg = [&]() -> std::optional<Register> {
        Register Reg;
        for (auto &Use : MI.all_defs()) {
          if (Use.isImplicit() || Use.getReg().isPhysical()) {
            return std::nullopt;
          }
          Reg = Use.getReg();
        }
        return Reg;
      }();

      if (!Reg.has_value()) {
        continue;
      }

      assert(Immediates.count(Reg.value()) == 0);
      Immediates[Reg.value()] = &MI;
    }
  }

  // Unconditionally fold all immediates into their uses
  for (auto &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      for (auto &Use : MI.explicit_uses()) {
        if (not Use.isReg()) {
          continue;
        }

        Register Reg = Use.getReg();
        if (Reg.isPhysical()) {
          continue;
        }
        if (Immediates.count(Reg) == 0) {
          // This register was not defined by a move immediate
          continue;
        }

        auto *DefMI = Immediates[Reg];

        // foldImmediate will delete MI on success and delete DefMI only if this
        // is the last use.
        bool IsDeleted = false;
        DidMakeChanges |= TII->foldImmediate(MI, *DefMI, Reg, &MRI, IsDeleted);
      }
    }
  }
  return DidMakeChanges;
}

char GBFoldImmediates::ID = 0;
FunctionPass *llvm::createGBFoldImmediates(GBTargetMachine &TM,
                                           CodeGenOptLevel OptLevel) {
  return new GBFoldImmediates();
}

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
#include "llvm/MC/MCRegister.h"

using namespace llvm;

#define DEBUG_TYPE "gb-push-pop-combine"

static cl::opt<bool>
    GBDisablePushPopCombine("gb-disable-push-pop-combine", cl::Hidden,
                            cl::desc("Disables GBPushPopCombine"));

namespace {
class GBPushPopCombine final : public MachineFunctionPass {
public:
  static char ID;

  GBPushPopCombine(GBTargetMachine &TargetMachine, CodeGenOptLevel OptLevel);

  StringRef getPassName() const override;

  bool runOnMachineFunction(MachineFunction &MF) override;

  bool combinePopThenPush(MachineFunction &MF);
};
} // namespace

GBPushPopCombine::GBPushPopCombine(GBTargetMachine &TargetMachine,
                                   CodeGenOptLevel OptLevel)
    : MachineFunctionPass(ID) {}

StringRef GBPushPopCombine::getPassName() const {
  return "GB Push Pop Combine";
}

bool GBPushPopCombine::combinePopThenPush(MachineFunction &MF) {
  const auto &TRI = *MF.getSubtarget().getRegisterInfo();

  auto DefinesTargetReg = [&](MachineInstr &MI, Register Reg) -> bool {
    auto Upper = TRI.getSubReg(Reg, 1);
    auto Lower = TRI.getSubReg(Reg, 2);

    return MI.definesRegister(Lower, &TRI) || MI.definesRegister(Upper, &TRI);
  };

  auto UsesTargetReg = [&](MachineInstr &MI, Register Reg) -> bool {
    auto Upper = TRI.getSubReg(Reg, 1);
    auto Lower = TRI.getSubReg(Reg, 2);

    if (MI.hasRegisterImplicitUseOperand(Reg) ||
        MI.hasRegisterImplicitUseOperand(Upper) ||
        MI.hasRegisterImplicitUseOperand(Lower)) {
      return true;
    }

    return MI.readsRegister(Lower, &TRI) || MI.readsRegister(Upper, &TRI);
  };

  auto KillsTargetReg = [&](MachineInstr &MI, Register Reg) -> bool {
    auto Upper = TRI.getSubReg(Reg, 1);
    auto Lower = TRI.getSubReg(Reg, 2);

    return MI.killsRegister(Lower, &TRI) || MI.killsRegister(Upper, &TRI);
  };

  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    MachineInstr *CurrentPop = nullptr;

    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (MI.getOpcode() == GB::POP) {
        CurrentPop = &MI;
        continue;
      }

      if (CurrentPop == nullptr) {
        continue;
      }

      Register CurrentPopReg = CurrentPop->getOperand(0).getReg();
      if (MI.getOpcode() == GB::PUSH) {
        Register PushReg = MI.getOperand(0).getReg();
        if (CurrentPopReg == PushReg && KillsTargetReg(MI, CurrentPopReg)) {
          LLVM_DEBUG(dbgs() << "Combining redundant pop-push sequence\n";
                     CurrentPop->dump(); MI.dump(); dbgs() << "\n");
          CurrentPop->removeFromParent();
          MI.removeFromParent();
          MadeChanges = true;

          CurrentPop = nullptr;
          continue;
        }

        // TODO: optimize this too
        LLVM_DEBUG(dbgs() << "Failing to combine redundant pop-push sequence, "
                             "blocked by unrelated push\n";
                   CurrentPop->dump(); MI.dump(); dbgs() << "\n");
        CurrentPop = nullptr;
        continue;
      }

      if (MI.readsRegister(GB::SP, &TRI)) {
        LLVM_DEBUG(dbgs() << "Failing to combine redundant pop-push sequence, "
                             "blocked by unrelated stack read\n";
                   CurrentPop->dump(); MI.dump(); dbgs() << "\n");
        CurrentPop = nullptr;
        continue;
      }

      if (UsesTargetReg(MI, CurrentPopReg) ||
          DefinesTargetReg(MI, CurrentPopReg)) {
        CurrentPop = nullptr;
      }
    }
  }

  return MadeChanges;
}

bool GBPushPopCombine::runOnMachineFunction(MachineFunction &MF) {
  if (GBDisablePushPopCombine) {
    return false;
  }

  bool MadeChanges = false;
  while (combinePopThenPush(MF)) {
  }
  return MadeChanges;
}

char GBPushPopCombine::ID = 0;
FunctionPass *llvm::createGBPushPopCombine(GBTargetMachine &TM,
                                           CodeGenOptLevel OptLevel) {
  return new GBPushPopCombine(TM, OptLevel);
}

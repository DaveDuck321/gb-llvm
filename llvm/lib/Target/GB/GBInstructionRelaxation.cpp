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

#define DEBUG_TYPE "gb-instruction-relaxation"

static cl::opt<bool> GBDisableInstructionRelaxation(
    "gb-disable-instruction-relaxation", cl::Hidden,
    cl::desc("Disables GBInstructionRelaxation"));

namespace {
class GBInstructionRelaxation final : public MachineFunctionPass {
public:
  static char ID;

  GBInstructionRelaxation(GBTargetMachine &TargetMachine,
                          CodeGenOptLevel OptLevel);

  StringRef getPassName() const override;

  bool runOnMachineFunction(MachineFunction &MF) override;

  bool mergeLoadStoreIncrementIntoLDI(MachineFunction &MF);
  bool mergeLoadStoreIncrementIntoLDI(MachineFunction &MF,
                                      MachineBasicBlock &MBB);
  bool foldRedundantCopies(MachineFunction &MF);
  bool relaxRotatesThroughA(MachineFunction &MF);
};

auto definesTargetReg(const MachineInstr &MI, Register Reg,
                      const TargetRegisterInfo &TRI) -> bool {
  auto Upper = TRI.getSubReg(Reg, 1);
  auto Lower = TRI.getSubReg(Reg, 2);

  return MI.definesRegister(Lower, &TRI) || MI.definesRegister(Upper, &TRI);
}

bool usesTargetReg(const MachineInstr &MI, Register Reg,
                   const TargetRegisterInfo &TRI) {
  auto Upper = TRI.getSubReg(Reg, 1);
  auto Lower = TRI.getSubReg(Reg, 2);

  if (MI.hasRegisterImplicitUseOperand(Reg) ||
      MI.hasRegisterImplicitUseOperand(Upper) ||
      MI.hasRegisterImplicitUseOperand(Lower)) {
    return true;
  }

  return MI.readsRegister(Lower, &TRI) || MI.readsRegister(Upper, &TRI);
}

bool killsTargetReg(const MachineInstr &MI, Register Reg,
                    const TargetRegisterInfo &TRI) {
  auto Upper = TRI.getSubReg(Reg, 1);
  auto Lower = TRI.getSubReg(Reg, 2);

  return MI.killsRegister(Lower, &TRI) || MI.killsRegister(Upper, &TRI);
}

} // namespace

GBInstructionRelaxation::GBInstructionRelaxation(GBTargetMachine &TargetMachine,
                                                 CodeGenOptLevel OptLevel)
    : MachineFunctionPass(ID) {}

StringRef GBInstructionRelaxation::getPassName() const {
  return "GB Instruction Relaxation";
}

bool GBInstructionRelaxation::mergeLoadStoreIncrementIntoLDI(
    MachineFunction &MF, MachineBasicBlock &MBB) {
  const auto &TRI = *MF.getSubtarget().getRegisterInfo();
  const auto &TII = *MF.getSubtarget().getInstrInfo();

  for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
    // Is this a load from (hl) or a store to (hl)?
    if ((MI.getOpcode() == GB::LD_iHL_r || MI.getOpcode() == GB::LD_r_iHL) &&
        MI.getOperand(0).getReg() == GB::A &&
        !killsTargetReg(MI, GB::HL, TRI)) {

      // Look ahead, is the next use of HL an increment?
      MachineInstr *Inc = nullptr;
      for (auto MBBI = ++MachineBasicBlock::iterator{MI}; MBBI != MBB.end();
           ++MBBI) {
        MachineInstr &NextMI = *MBBI;
        if (NextMI.getOpcode() == GB::INC16 &&
            usesTargetReg(NextMI, GB::HL, TRI)) {
          Inc = &NextMI;
          break;
        }

        if (usesTargetReg(NextMI, GB::HL, TRI) ||
            definesTargetReg(NextMI, GB::HL, TRI) ||
            killsTargetReg(NextMI, GB::HL, TRI)) {
          break;
        }
      }

      // Have we found a compatible load/ store, inc pattern?
      if (Inc != nullptr) {
        Inc->removeFromParent();
        auto NewOpcode =
            MI.getOpcode() == GB::LD_r_iHL ? GB::LDI_A_iHL : GB::LDI_iHL_A;
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(NewOpcode));
        MI.removeFromParent();
        return true; // We've just invalidated the iterator
      }
    }
  }
  return false;
}

bool GBInstructionRelaxation::mergeLoadStoreIncrementIntoLDI(
    MachineFunction &MF) {
  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    while (mergeLoadStoreIncrementIntoLDI(MF, MBB)) {
      MadeChanges = true;
    }
  }
  return MadeChanges;
}

bool GBInstructionRelaxation::foldRedundantCopies(MachineFunction &MF) {
  const auto &TRI = *MF.getSubtarget().getRegisterInfo();

  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (MI.getOpcode() == GB::LD_rr) {
        // Special case for self-copies
        Register DstReg = MI.getOperand(0).getReg();
        Register SrcReg = MI.getOperand(1).getReg();
        if (DstReg == SrcReg) {
          MI.removeFromParent();
          MadeChanges = true;
          continue;
        }
      }

      if (!(MI.getOpcode() == GB::LD_r_iHL || MI.getOpcode() == GB::LD_rr)) {
        continue;
      }

      Register DstReg = MI.getOperand(0).getReg();
      Register MoveToReg;
      MachineInstr *MoveToOmit = nullptr;

      // Is there a move immediately following the copy?
      for (auto MBBI = ++MachineBasicBlock::iterator{MI}; MBBI != MBB.end();
           ++MBBI) {
        if (MBBI->getOpcode() == GB::LD_rr &&
            MBBI->getOperand(1).getReg() == DstReg &&
            MBBI->killsRegister(DstReg, &TRI)) {
          MoveToReg = MBBI->getOperand(0).getReg();
          MoveToOmit = &(*MBBI);
          break;
        }

        if (MBBI->killsRegister(DstReg, &TRI) ||
            MBBI->readsRegister(DstReg, &TRI) ||
            MBBI->definesRegister(DstReg, &TRI)) {
          break;
        }
      }

      // Are we free to fold the move into the copy?
      for (auto MBBI = ++MachineBasicBlock::iterator{MI}; MBBI != MBB.end();
           ++MBBI) {
        if (&(*MBBI) == MoveToOmit) {
          break;
        }

        if (MBBI->killsRegister(MoveToReg, &TRI) ||
            MBBI->readsRegister(MoveToReg, &TRI) ||
            MBBI->definesRegister(MoveToReg, &TRI)) {
          MoveToOmit = nullptr;
          break;
        }
      }

      // We've passed all the checks, fold
      if (MoveToOmit != nullptr) {
        MoveToOmit->removeFromParent();
        MI.getOperand(0).ChangeToRegister(MoveToReg, true);
        MadeChanges = true;
        break;
      }
    }
  }
  return MadeChanges;
}

bool GBInstructionRelaxation::relaxRotatesThroughA(MachineFunction &MF) {
  const auto &TII = *MF.getSubtarget().getInstrInfo();

  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (MI.getOpcode() == GB::RRC_r && MI.getOperand(0).getReg() == GB::A) {
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::RRCA));
        MI.removeFromParent();

        MadeChanges = true;
        continue;
      }

      if (MI.getOpcode() == GB::RLC_r && MI.getOperand(0).getReg() == GB::A) {
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::RLCA));
        MI.removeFromParent();

        MadeChanges = true;
        continue;
      }
    }
  }
  return MadeChanges;
}

bool GBInstructionRelaxation::runOnMachineFunction(MachineFunction &MF) {
  if (GBDisableInstructionRelaxation) {
    return false;
  }

  bool MadeChanges = false;
  MadeChanges |= mergeLoadStoreIncrementIntoLDI(MF);
  MadeChanges |= relaxRotatesThroughA(MF);
  while (foldRedundantCopies(MF)) {
    MadeChanges = true;
  }
  return MadeChanges;
}

char GBInstructionRelaxation::ID = 0;
FunctionPass *llvm::createGBInstructionRelaxation(GBTargetMachine &TM,
                                                  CodeGenOptLevel OptLevel) {
  return new GBInstructionRelaxation(TM, OptLevel);
}

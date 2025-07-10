#include "GB.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominanceFrontier.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/RDFLiveness.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/Debug.h"

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

  void getAnalysisUsage(AnalysisUsage &AU) const override;

  bool runOnMachineFunction(MachineFunction &MF) override;

  bool mergeLoadImmStore(MachineFunction &MF);
  bool mergeLoadImmStore(MachineFunction &MF, MachineBasicBlock &MBB);
  bool mergeLoadStoreIncrementIntoLDI(MachineFunction &MF);
  bool mergeLoadStoreIncrementIntoLDI(MachineFunction &MF,
                                      MachineBasicBlock &MBB);
  bool foldRedundantCopies(MachineFunction &MF);
  bool relaxRotatesThroughA(MachineFunction &MF);
  bool simplifyCp00(MachineFunction &MF);
};

auto definesTargetReg(const MachineInstr &MI, Register Reg,
                      const TargetRegisterInfo &TRI) -> bool {
  auto Upper = TRI.getSubReg(Reg, 1);
  auto Lower = TRI.getSubReg(Reg, 2);

  return MI.definesRegister(Reg, &TRI) || MI.definesRegister(Lower, &TRI) ||
         MI.definesRegister(Upper, &TRI);
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

  return MI.readsRegister(Reg, &TRI) || MI.readsRegister(Lower, &TRI) ||
         MI.readsRegister(Upper, &TRI);
}

bool killsTargetReg(const MachineInstr &MI, Register Reg,
                    const TargetRegisterInfo &TRI) {
  auto Upper = TRI.getSubReg(Reg, 1);
  auto Lower = TRI.getSubReg(Reg, 2);

  return MI.killsRegister(Reg, &TRI) || MI.killsRegister(Lower, &TRI) ||
         MI.killsRegister(Upper, &TRI);
}

} // namespace

GBInstructionRelaxation::GBInstructionRelaxation(GBTargetMachine &TargetMachine,
                                                 CodeGenOptLevel OptLevel)
    : MachineFunctionPass(ID) {}

StringRef GBInstructionRelaxation::getPassName() const {
  return "GB Instruction Relaxation";
}

void GBInstructionRelaxation::getAnalysisUsage(AnalysisUsage &AU) const {
  MachineFunctionPass::getAnalysisUsage(AU);
  AU.addRequired<MachineDominatorTreeWrapperPass>();
  AU.addRequired<MachineDominanceFrontier>();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool GBInstructionRelaxation::mergeLoadImmStore(MachineFunction &MF,
                                                MachineBasicBlock &MBB) {
  const auto &TRI = *MF.getSubtarget().getRegisterInfo();
  const auto &TII = *MF.getSubtarget().getInstrInfo();

  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (MI.getOpcode() != GB::LDI8_r) {
        continue;
      }

      // Look ahead, is the next use `ld (hl), reg`?
      Register Target = MI.getOperand(0).getReg();
      MachineInstr *LD_iHL_r = nullptr;
      for (auto MBBI = ++MachineBasicBlock::iterator{MI}; MBBI != MBB.end();
           ++MBBI) {
        MachineInstr &NextMI = *MBBI;
        if (NextMI.getOpcode() == GB::LD_iHL_r &&
            usesTargetReg(NextMI, Target, TRI) &&
            killsTargetReg(NextMI, Target, TRI)) {
          LD_iHL_r = &NextMI;
          break;
        }

        if (usesTargetReg(NextMI, Target, TRI) ||
            killsTargetReg(NextMI, Target, TRI)) {
          break;
        }
      }

      if (LD_iHL_r != nullptr) {
        BuildMI(MBB, *LD_iHL_r, MI.getDebugLoc(), TII.get(GB::LDI8_iHL))
            .add(MI.getOperand(1))
            .addReg(GB::HL,
                    getImplRegState(true) |
                        getKillRegState(LD_iHL_r->killsRegister(GB::HL, &TRI)));
        MI.eraseFromParent();
        LD_iHL_r->eraseFromParent();
        return true;
      }
    }
  }
  return false;
}

bool GBInstructionRelaxation::mergeLoadImmStore(MachineFunction &MF) {
  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    while (mergeLoadImmStore(MF, MBB)) {
      MadeChanges = true;
    }
  }
  return MadeChanges;
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
        Inc->eraseFromParent();
        auto NewOpcode =
            MI.getOpcode() == GB::LD_r_iHL ? GB::LDI_A_iHL : GB::LDI_iHL_A;
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(NewOpcode));
        MI.eraseFromParent();
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
          MI.eraseFromParent();
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
        MoveToOmit->eraseFromParent();
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
        MI.eraseFromParent();

        MadeChanges = true;
        continue;
      }

      if (MI.getOpcode() == GB::RR_r && MI.getOperand(0).getReg() == GB::A) {
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::RRA));
        MI.eraseFromParent();

        MadeChanges = true;
        continue;
      }

      if (MI.getOpcode() == GB::RLC_r && MI.getOperand(0).getReg() == GB::A) {
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::RLCA));
        MI.eraseFromParent();

        MadeChanges = true;
        continue;
      }

      if (MI.getOpcode() == GB::RL_r && MI.getOperand(0).getReg() == GB::A) {
        assert(MI.getOperand(1).getReg() == GB::A);
        auto Added = BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::RLA));

        // We may shift the f flag into a bool value. Here A might be undefined
        assert(Added->getOperand(2).getReg() == GB::A);
        Added->getOperand(2).setIsUndef(MI.getOperand(1).isUndef());

        MI.eraseFromParent();

        MadeChanges = true;
        continue;
      }
    }
  }
  return MadeChanges;
}

bool GBInstructionRelaxation::simplifyCp00(MachineFunction &MF) {
  // TODO: look for the (CP (AND ...)) pattern while we've still got a DAG
  // Even better (CP (AND (1 << N))) can be (BIT N) to reduce register pressure.
  const auto &TII = *MF.getSubtarget().getInstrInfo();

  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      // CP 00 also sets the subtract flag.
      // This is REALLY hard to observe. We'll certainly never generate code
      // that relies on this.
      if (MI.getOpcode() == GB::CPI && MI.getOperand(0).getImm() == 0) {
        BuildMI(MBB, MI, MI.getDebugLoc(), TII.get(GB::OR_r)).addReg(GB::A);
        MI.eraseFromParent();

        MadeChanges = true;
      }
    }
  }
  return MadeChanges;
}

bool GBInstructionRelaxation::runOnMachineFunction(MachineFunction &MF) {
  if (GBDisableInstructionRelaxation) {
    return false;
  }

  LLVM_DEBUG(dbgs() << "******* GBInstructionRelaxation ****** \n";
             MF.print(dbgs()));

  auto &Subtarget = MF.getSubtarget<GBSubtarget>();
  auto *InstrInfo = Subtarget.getInstrInfo();
  auto *RegisterInfo = Subtarget.getRegisterInfo();

  // Update the liveness annotations to include kills
  const auto &MDF = getAnalysis<MachineDominanceFrontier>();
  auto *MDT = &getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree();

  rdf::DataFlowGraph Graph(MF, *InstrInfo, *RegisterInfo, *MDT, MDF);
  rdf::Liveness Liveness(MF.getRegInfo(), Graph);
  Liveness.resetKills();

  bool MadeChanges = false;
  MadeChanges |= mergeLoadStoreIncrementIntoLDI(MF);
  MadeChanges |= relaxRotatesThroughA(MF);
  MadeChanges |= simplifyCp00(MF);
  while (foldRedundantCopies(MF)) {
    MadeChanges = true;
  }
  MadeChanges |= mergeLoadImmStore(MF);

  if (MadeChanges) {
    LLVM_DEBUG(MF.print(dbgs()));
  }
  return MadeChanges;
}

char GBInstructionRelaxation::ID = 0;
FunctionPass *llvm::createGBInstructionRelaxation(GBTargetMachine &TM,
                                                  CodeGenOptLevel OptLevel) {
  return new GBInstructionRelaxation(TM, OptLevel);
}

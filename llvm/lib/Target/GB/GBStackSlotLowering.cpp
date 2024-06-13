#include "GB.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"

#include <algorithm>
#include <cstddef>

using namespace llvm;

namespace {

class GBStackSlotLowering final : public MachineFunctionPass {
public:
  static char ID;

  GBStackSlotLowering(GBTargetMachine &TargetMachine, CodeGenOptLevel OptLevel);

  StringRef getPassName() const override;

  void saveReg8ToStackSlot(MachineInstr &MI,
                           const LivePhysRegs &RegsImmediatelyAfter) const;
  void saveReg16ToStackSlot(MachineInstr &MI,
                            const LivePhysRegs &RegsImmediatelyAfter) const;

  void loadReg8FromStackSlot(MachineInstr &MI,
                             const LivePhysRegs &RegsImmediatelyAfter) const;
  void loadReg16FromStackSlot(MachineInstr &MI,
                              const LivePhysRegs &RegsImmediatelyAfter) const;
  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // namespace

GBStackSlotLowering::GBStackSlotLowering(GBTargetMachine &TargetMachine,
                                         CodeGenOptLevel OptLevel)
    : MachineFunctionPass(ID) {}

StringRef GBStackSlotLowering::getPassName() const {
  return "GB Stack Slot lowering";
}

void GBStackSlotLowering::saveReg8ToStackSlot(
    MachineInstr &MI, const LivePhysRegs &RegsImmediatelyAfter) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();
  auto &TRI = *MF.getSubtarget().getRegisterInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();
  const MachineRegisterInfo &MRI = MF.getRegInfo();

  // Storing to stack slot
  // Case 1
  //  HL is NOT live after, HL is NOT the operand
  // Case 2
  //  HL is live after, HL is NOT the operand
  //   - push HL, [Case 1], pop HL
  // Case 3
  //  HL is NOT live after, HL is the operand
  //   - Allocate tmp reg
  //   - [Case 1]
  //   - Deallocate tmp reg
  // Case 4
  //  HL is live after, HL is the operand
  //   - push HL, [Case 3], pop HL

  SmallVector<MCRegister, 4> Stack;
  size_t SPOffest = MI.getOperand(1).getImm();
  auto SrcReg = MI.getOperand(0).getReg();

  bool HLLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::HL);
  bool HLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::H);
  bool LLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::L);
  bool HLIsOperand =
      MI.readsRegister(GB::H, &TRI) || MI.readsRegister(GB::H, &TRI);

  if (HLIsOperand) {
    assert(SrcReg == GB::H || SrcReg == GB::L);

    bool MustSaveH = HLiveAfter || (SrcReg == GB::H);
    bool MustSaveL = LLiveAfter || (SrcReg == GB::L);

    MCRegister CopyIntoH;
    MCRegister CopyIntoL;

    // For correctness, we MUST use the scratch reg for either H or L
    // Might as well use it to avoid the push/pop HL
    size_t RequiredScratchRegs = MustSaveH && MustSaveL ? 2 : 1;

    SmallVector<MCRegister, 8> Available;
    for (const auto &Reg : {GB::A, GB::B, GB::C, GB::D, GB::E}) {
      if (RegsImmediatelyAfter.available(MRI, Reg)) {
        Available.push_back(Reg);
      }
    }

    // Ensure scratch registers are available
    if (Available.size() < RequiredScratchRegs) {
      // Couldn't find a scratch register
      SPOffest += 2;
      Stack.push_back(GB::BC);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
          .addReg(GB::BC, getKillRegState(true));
      Available.clear();
      Available.push_back(GB::B);
      Available.push_back(GB::C);
    }

    std::reverse(Available.begin(), Available.end());

    // Copy H/ L into scratch registers
    if (MustSaveH) {
      if (HLiveAfter) {
        CopyIntoH = Available.back();
      }
      if (SrcReg == GB::H) {
        SrcReg = Available.back();
      }
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), Available.back())
          .addReg(GB::H, getKillRegState(true));
      Available.pop_back();
    }
    if (MustSaveL) {
      if (LLiveAfter) {
        CopyIntoL = Available.back();
      }
      if (SrcReg == GB::L) {
        SrcReg = Available.back();
      }
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), Available.back())
          .addReg(GB::L, getKillRegState(true));
      Available.pop_back();
    }

    // Finally do the store
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP))
        .addImm(SPOffest);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcReg, getKillRegState(true));

    if (CopyIntoL.isValid()) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::L)
          .addReg(CopyIntoL, getKillRegState(true));
    }
    if (CopyIntoH.isValid()) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::H)
          .addReg(CopyIntoH, getKillRegState(true));
    }
  } else {
    // Otherwise we just push/ pop
    // TODO GB: optimize this in a later pass 1) remove unnecessary stack manip,
    // 2) coalesce
    if (HLLiveAfter) {
      SPOffest += 2;
      Stack.push_back(GB::HL);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
          .addReg(GB::HL, getKillRegState(true));
    }
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP))
        .addImm(SPOffest);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcReg, getKillRegState(
                            not RegsImmediatelyAfter.contains(SrcReg)));
  }

  // Pop everything back off stack
  while (not Stack.empty()) {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::POP), Stack.back());
    Stack.pop_back();
  }
}

void GBStackSlotLowering::saveReg16ToStackSlot(
    MachineInstr &MI, const LivePhysRegs &RegsImmediatelyAfter) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();
  auto &TRI = *MF.getSubtarget().getRegisterInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();
  const MachineRegisterInfo &MRI = MF.getRegInfo();

  // Storing to stack slot
  // Case 1
  //  HL is NOT live after, HL is NOT the operand
  // Case 2
  //  HL is live after, HL is NOT the operand
  //   - push HL, [Case 1], pop HL
  // Case 3
  //  HL is NOT live after, HL is the operand
  //   - Allocate tmp reg
  //   - [Case 1]
  //   - Deallocate tmp reg
  // Case 4
  //  HL is live after, HL is the operand
  //   - push HL, [Case 3], pop HL

  SmallVector<MCRegister, 4> Stack;
  size_t SPOffest = MI.getOperand(1).getImm();
  auto SrcReg = MI.getOperand(0).getReg();
  auto SrcLower = TRI.getSubReg(SrcReg, 1);
  auto SrcUpper = TRI.getSubReg(SrcReg, 2);

  bool HLLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::HL);
  bool HLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::H);
  bool LLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::L);
  bool HLIsOperand = MI.readsRegister(GB::HL, &TRI);

  if (HLIsOperand) {
    assert(SrcReg == GB::HL);
    MCRegister CopyIntoH;
    MCRegister CopyIntoL;

    // For correctness, we MUST use the scratch reg for either H or L
    // Might as well use it to avoid the push/pop HL
    size_t RequiredScratchRegs = 2;

    SmallVector<MCRegister, 8> Available;
    for (const auto &Reg : {GB::A, GB::B, GB::C, GB::D, GB::E}) {
      if (RegsImmediatelyAfter.available(MRI, Reg)) {
        Available.push_back(Reg);
      }
    }

    // Ensure scratch registers are available
    if (Available.size() < RequiredScratchRegs) {
      // Couldn't find a scratch register
      SPOffest += 2;
      Stack.push_back(GB::BC);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
          .addReg(GB::BC, getKillRegState(true));
      Available.clear();
      Available.push_back(GB::B);
      Available.push_back(GB::C);
    }

    SrcLower = Available[0];
    SrcUpper = Available[1];

    // Copy H/ L into scratch registers
    if (HLiveAfter) {
      CopyIntoH = SrcUpper;
    }
    if (LLiveAfter) {
      CopyIntoL = SrcLower;
    }
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), SrcUpper)
        .addReg(GB::H, getKillRegState(true));
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), SrcLower)
        .addReg(GB::L, getKillRegState(true));

    // Finally do the store
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP))
        .addImm(SPOffest);

    if (SrcLower == GB::A) {
      // Might as well use the free increment of HL if we've been assigned A
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LDI_iHL_A));
    } else {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
          .addReg(SrcLower);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
          .addReg(GB::HL);
    }
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcUpper);

    if (CopyIntoL.isValid()) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::L)
          .addReg(CopyIntoL, getKillRegState(true));
    }
    if (CopyIntoH.isValid()) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::H)
          .addReg(CopyIntoH, getKillRegState(true));
    }
  } else {
    // Otherwise we just push/ pop
    // TODO GB: optimize this in a later pass 1) remove unnecessary stack manip,
    // 2) coalesce
    if (HLLiveAfter) {
      SPOffest += 2;
      Stack.push_back(GB::HL);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
          .addReg(GB::HL, getKillRegState(true));
    }

    // TODO GB: maybe copy lower to 8 to save the increment
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP))
        .addImm(SPOffest);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcLower,
                getKillRegState(not RegsImmediatelyAfter.contains(SrcLower)));
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
        .addReg(GB::HL);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcUpper,
                getKillRegState(not RegsImmediatelyAfter.contains(SrcUpper)));
  }

  // Pop everything back off stack
  while (not Stack.empty()) {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::POP), Stack.back());
    Stack.pop_back();
  }
}

void GBStackSlotLowering::loadReg8FromStackSlot(
    MachineInstr &MI, const LivePhysRegs &RegsImmediatelyAfter) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();
  auto &TRI = *MF.getSubtarget().getRegisterInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();
  const MachineRegisterInfo &MRI = MF.getRegInfo();

  // Loading from stack slot
  // Case 1:
  //  HL is NOT live after
  //   - Nothing special, clobber HL
  // Case 2:
  //  HL is live after, HL is NOT result
  //   - Push HL, [Case 1], Pop HL
  // Case 3:
  //  HL is result
  //   - Just [Case 1]
  auto DestReg = MI.getOperand(0).getReg();
  size_t SPOffest = MI.getOperand(1).getImm();

  bool ResultIsHL =
      MI.definesRegister(GB::H, &TRI) || MI.definesRegister(GB::L, &TRI);
  bool AllowedToClobberHL =
      ResultIsHL || RegsImmediatelyAfter.available(MRI, GB::HL);

  if (not AllowedToClobberHL) {
    // TODO GB: either use a temporary register here or optimize push patterns
    SPOffest += 2;
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
        .addReg(GB::HL, getKillRegState(true));
  }

  assert(SPOffest <= 127 && "TODO GB: support larger stack offsets");
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP)).addImm(SPOffest);
  // TODO GB: do I need to mark HL as killed after this?
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), DestReg);

  if (not AllowedToClobberHL) {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::POP), GB::HL);
  }
}

void GBStackSlotLowering::loadReg16FromStackSlot(
    MachineInstr &MI, const LivePhysRegs &RegsImmediatelyAfter) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();
  auto &TRI = *MF.getSubtarget().getRegisterInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();
  const MachineRegisterInfo &MRI = MF.getRegInfo();

  // Loading from stack slot
  // Case 1:
  //  HL is NOT live after
  //   - Nothing special, clobber HL
  // Case 2:
  //  HL is live after, HL is NOT result
  //   - Push HL, [Case 1], Pop HL
  // Case 3:
  //  HL is result
  //   - Need a working register r8 (lets say GB::A)
  //   - Load to r8 (LD (HL+), A) looking good
  //   - Load to GB::H
  //   - GB::H <- GB::A

  // Case 1 example
  //   opt 1) Needs just A
  //    PUSH AF      12
  //    LD SP, r8    16
  //    LD (HL+), A  8
  //    LD (HL), C   8
  //    LD B, A      4
  //    POP AF       16
  // total: 36-64 (preserves flag also)

  //   opt 2) Needs nothing
  //    LD SP, r8    16
  //    LD (HL), B   8
  //    inc HL       8
  //    LD (HL), C   8
  // total: 40

  // Case 3 example
  //   opt 1) Needs just A
  //    PUSH AF      12
  //    LD SP, r8    16
  //    LD (HL+), A  8
  //    LD (HL), L   8
  //    LD H, A      4
  //    POP AF       16
  //    total: 36-64 (preserves flag also)

  //   opt 2) Needs any register
  //    LD SP, r8    16
  //    LD (HL), B   8
  //    INC HL       8
  //    LD (HL), L   8
  //    LD H, B      4
  //    total: 44 (if registers available)

  SmallVector<MCRegister, 4> Stack;
  size_t SPOffest = MI.getOperand(1).getImm();
  auto DestReg = MI.getOperand(0).getReg();

  bool ResultIsHL =
      MI.definesRegister(GB::H, &TRI) || MI.definesRegister(GB::L, &TRI);
  bool AllowedToClobberHL =
      ResultIsHL || RegsImmediatelyAfter.available(MRI, GB::HL);

  if (not AllowedToClobberHL) {
    // TODO GB: either use a temporary register here or optimize push patterns
    SPOffest += 2;
    Stack.push_back(GB::HL);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
        .addReg(GB::HL, getKillRegState(true));
  }

  auto CopyLowerTo = TRI.getSubReg(DestReg, 1);
  auto CopyUpperTo = TRI.getSubReg(DestReg, 2);

  if (ResultIsHL) {
    assert(CopyLowerTo == GB::L);
    assert(CopyUpperTo == GB::H);

    MCRegister ScratchReg;
    for (const auto &Reg : {GB::A, GB::B, GB::C, GB::D, GB::E}) {
      if (RegsImmediatelyAfter.available(MRI, Reg)) {
        ScratchReg = Reg;
        break;
      }
    }
    if (not ScratchReg.isValid()) {
      // Couldn't find a scratch register
      SPOffest += 2;
      Stack.push_back(GB::AF);
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
          .addReg(GB::AF, getKillRegState(true));
      ScratchReg = GB::A;
    }

    assert(ScratchReg.isValid());
    CopyLowerTo = ScratchReg;
  }

  // Finally do the copy:
  assert(SPOffest + 1 <= 127 && "TODO GB: support larger stack offsets");

  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_HL_SP)).addImm(SPOffest);

  // TODO GB: can this be a generic pattern so we get the benefit everywhere?
  // TODO GB: always scavenge GB::A (saves 4 cycles if its available)
  if (CopyLowerTo == GB::A) {
    // Might as well use the free increment of HL if we've been assigned A
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LDI_A_iHL));
  } else {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), CopyLowerTo);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
        .addReg(GB::HL);
  }

  // TODO GB: do I need to mark HL as killed after this?
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), CopyUpperTo);

  if (ResultIsHL) {
    assert(CopyLowerTo != GB::L);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::L)
        .addReg(CopyLowerTo, getKillRegState(true));
  }

  // Pop everything back off stack
  while (not Stack.empty()) {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::POP), Stack.back());
    Stack.pop_back();
  }
}

bool GBStackSlotLowering::runOnMachineFunction(MachineFunction &MF) {
  const auto &TRI = *MF.getSubtarget().getRegisterInfo();

  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      switch (MI.getOpcode()) {
      case GB::Save8ToFrameIndex:
      case GB::Save16ToFrameIndex:
      case GB::Load8FromFrameIndex:
      case GB::Load16FromFrameIndex:
        break;
      default:
        continue;
      }
      // Calculate live registers
      LivePhysRegs LiveRegsAfter(TRI);
      LiveRegsAfter.addLiveOuts(MBB);
      const auto E =
          MachineBasicBlock::const_iterator(MI.getIterator()).getReverse();
      for (auto I = MBB.rbegin(); I != E; ++I) {
        LiveRegsAfter.stepBackward(*I);
      }

      switch (MI.getOpcode()) {
      case GB::Save8ToFrameIndex:
        saveReg8ToStackSlot(MI, LiveRegsAfter);
        break;
      case GB::Save16ToFrameIndex:
        saveReg16ToStackSlot(MI, LiveRegsAfter);
        break;
      case GB::Load8FromFrameIndex:
        loadReg8FromStackSlot(MI, LiveRegsAfter);
        break;
      case GB::Load16FromFrameIndex:
        loadReg16FromStackSlot(MI, LiveRegsAfter);
        break;
      }

      MI.removeFromParent();
      MadeChanges = true;
    }
  }
  return MadeChanges;
}

char GBStackSlotLowering::ID = 0;
FunctionPass *llvm::createGBStackSlotLowering(GBTargetMachine &TM,
                                              CodeGenOptLevel OptLevel) {
  return new GBStackSlotLowering(TM, OptLevel);
}

#include "GB.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"

#include <algorithm>
#include <cstddef>

using namespace llvm;

static cl::opt<bool>
    GBDisableStackSlotLowering("gb-disable-stack-slot-lowering", cl::Hidden,
                               cl::desc("Disables GBStackSlotLowering"));

namespace {

void loadHLWithStackOffset(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator &MBBI, DebugLoc DL,
                           const TargetInstrInfo &TII, size_t TargetOffset) {
  if (TargetOffset > 127) {
    BuildMI(MBB, MBBI, DL, TII.get(GB::LDI16), GB::HL).addImm(TargetOffset);
    BuildMI(MBB, MBBI, DL, TII.get(GB::ADD_HL)).addReg(GB::SP);
  } else {
    BuildMI(MBB, MBBI, DL, TII.get(GB::LD_HL_SP)).addImm(TargetOffset);
  }
}

class StackAllocator {
  MachineBasicBlock &MBB;
  const TargetInstrInfo &TII;
  MachineBasicBlock::iterator MBBI;
  SmallVector<Register, 4> ToPop;
  SmallVector<MCRegister, 8> Available;

  void markRegAvailable(MCRegister Reg) {
    if (Reg != GB::F && not available(Reg)) {
      Available.push_back(Reg);
    }
  }

public:
  StackAllocator(const LivePhysRegs &RegsImmediatelyAfter,
                 const TargetInstrInfo &TII, MachineBasicBlock::iterator MBBI)
      : MBB(*MBBI->getParent()), TII(TII), MBBI(MBBI) {
    const MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();

    for (const auto &Reg : {GB::A, GB::B, GB::C, GB::D, GB::E}) {
      if (RegsImmediatelyAfter.available(MRI, Reg)) {
        Available.push_back(Reg);
      }
    }
    std::reverse(Available.begin(), Available.end());
  }
  StackAllocator(const StackAllocator &) = delete;
  StackAllocator &operator=(const StackAllocator &) = delete;
  ~StackAllocator() {
    while (not ToPop.empty()) {
      pop(ToPop.back());
    }
  }

  void save(MCRegister Reg, bool AlsoKill = true) {
    const MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();
    auto &MI = *MBBI;
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::PUSH))
        .addReg(Reg, getKillRegState(AlsoKill));
    ToPop.push_back(Reg);

    if (AlsoKill) {
      markRegAvailable(MRI.getTargetRegisterInfo()->getSubReg(Reg, 1));
      markRegAvailable(MRI.getTargetRegisterInfo()->getSubReg(Reg, 2));
    }
  }

  void pop(MCRegister Reg) {
    auto &MI = *MBBI;
    auto &MBB = *MI.getParent();

    assert(ToPop.back() == Reg);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::POP), ToPop.back());
    ToPop.pop_back();
  }

  size_t availableCount() { return Available.size(); }

  bool available(MCRegister Reg) {
    return std::find(Available.begin(), Available.end(), Reg) !=
           Available.end();
  }

  Register reserveReg() {
    Register Reg = Available.back();
    Available.pop_back();
    return Reg;
  }

  size_t currentOffset() const { return 2 * ToPop.size(); }
};

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

  size_t SPOffest = MI.getOperand(1).getImm();
  auto SrcReg = MI.getOperand(0).getReg();

  bool HLLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::HL);
  bool HLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::H);
  bool LLiveAfter = not RegsImmediatelyAfter.available(MRI, GB::L);
  bool HLIsOperand =
      MI.readsRegister(GB::H, &TRI) || MI.readsRegister(GB::L, &TRI);

  StackAllocator Stack{RegsImmediatelyAfter, TII, MBBI};
  if (not RegsImmediatelyAfter.available(MRI, GB::F)) {
    // ld HL, SP + 1 clobbers F
    Stack.save(GB::AF, /*AlsoKill=*/SrcReg != GB::A);
  }

  if (HLIsOperand) {
    assert(SrcReg == GB::H || SrcReg == GB::L);

    bool MustSaveH = HLiveAfter || (SrcReg == GB::H);
    bool MustSaveL = LLiveAfter || (SrcReg == GB::L);

    MCRegister CopyIntoH;
    MCRegister CopyIntoL;

    // For correctness, we MUST use the scratch reg for either H or L
    // Might as well use it to avoid the push/pop HL
    size_t RequiredScratchRegs = (MustSaveH && MustSaveL) ? 2 : 1;

    // Ensure scratch registers are available
    if (Stack.availableCount() < RequiredScratchRegs) {
      // Couldn't find a scratch register
      Stack.save(GB::BC);
    }

    // Copy H/ L into scratch registers
    if (MustSaveH) {
      Register Reg = Stack.reserveReg();
      if (HLiveAfter) {
        CopyIntoH = Reg;
      }
      if (SrcReg == GB::H) {
        SrcReg = Reg;
      }
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), Reg)
          .addReg(GB::H, getKillRegState(true));
    }
    if (MustSaveL) {
      Register Reg = Stack.reserveReg();
      if (LLiveAfter) {
        CopyIntoL = Reg;
      }
      if (SrcReg == GB::L) {
        SrcReg = Reg;
      }
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), Reg)
          .addReg(GB::L, getKillRegState(true));
    }

    // Finally do the store
    loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                          SPOffest + Stack.currentOffset());

    bool KillSrcReg = not(SrcReg == CopyIntoL || SrcReg == CopyIntoH);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcReg, getKillRegState(KillSrcReg));

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
      Stack.save(GB::HL);
    }

    loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                          SPOffest + Stack.currentOffset());

    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcReg,
                getKillRegState(not RegsImmediatelyAfter.contains(SrcReg)));
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

  size_t SPOffest = MI.getOperand(1).getImm();
  auto SrcReg = MI.getOperand(0).getReg();

  StackAllocator Stack{RegsImmediatelyAfter, TII, MBBI};
  if (not RegsImmediatelyAfter.available(MRI, GB::F)) {
    // ld HL, SP + 1 clobbers F
    Stack.save(GB::AF);
    assert(SrcReg != GB::AF);
  }

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
    // Ensure scratch registers are available
    if (Stack.availableCount() < 2) {
      // Couldn't find a scratch register
      Stack.save(GB::BC);
    }

    SrcLower = Stack.reserveReg();
    SrcUpper = Stack.reserveReg();

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
    loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                          SPOffest + Stack.currentOffset());

    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcLower);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
        .addReg(GB::HL);

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
      Stack.save(GB::HL);
    }

    loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                          SPOffest + Stack.currentOffset());

    // If A is available we can blindly copy into it and eliminate the
    // increment.
    bool CanClobberA = RegsImmediatelyAfter.available(MRI, GB::A);
    if (CanClobberA) {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::A)
          .addReg(SrcLower,
                  getKillRegState(not RegsImmediatelyAfter.contains(SrcLower)));
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
          .addReg(GB::A, getKillRegState(true));
    } else {
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
          .addReg(SrcLower,
                  getKillRegState(not RegsImmediatelyAfter.contains(SrcLower)));
    }
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
        .addReg(GB::HL);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r))
        .addReg(SrcUpper,
                getKillRegState(not RegsImmediatelyAfter.contains(SrcUpper)));
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

  StackAllocator Stack{RegsImmediatelyAfter, TII, MBBI};
  bool ResultIsH = MI.definesRegister(GB::H, &TRI);
  bool ResultIsL = MI.definesRegister(GB::L, &TRI);

  bool HasToSaveH = (!RegsImmediatelyAfter.available(MRI, GB::H) && !ResultIsH);
  bool HasToSaveL = (!RegsImmediatelyAfter.available(MRI, GB::L) && !ResultIsL);

  bool CopyBackToA = false;
  bool FSaved = false;

  Register AfterLoadRestoreFrom;
  Register AfterLoadRestoreTo;

  if (HasToSaveH || HasToSaveL) {
    if (ResultIsH || ResultIsL) {
      // We've got to save H or L, the other is the result
      // Find temporary storage for H/L
      assert(DestReg != GB::A);
      if (Stack.availableCount() == 0) {
        Stack.save(GB::AF);
      }
      AfterLoadRestoreFrom = Stack.reserveReg();
      AfterLoadRestoreTo = HasToSaveH ? GB::H : GB::L;
      FSaved = true;

      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr),
              AfterLoadRestoreFrom)
          .addReg(AfterLoadRestoreTo, getKillRegState(true));
    } else {
      // We've got to save either H or L, since neither are our results we can
      // simply push here
      // TODO: GB use temporary register here or coalesce push/ pop
      Stack.save(GB::HL);
    }
  }

  // Make sure we're not clobbering the flag.
  if (!FSaved && !RegsImmediatelyAfter.available(MRI, GB::F)) {
    // ld HL, SP + 1 clobbers F
    Stack.save(GB::AF);
    if (DestReg == GB::A) {
      // :-| this will clobber A, we'll need to copy into a temporary location
      // first
      DestReg = GB::H;
      CopyBackToA = true;
    }
  }

  loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                        SPOffest + Stack.currentOffset());
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), DestReg);

  if (AfterLoadRestoreFrom.isValid() || AfterLoadRestoreTo.isValid()) {
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), AfterLoadRestoreTo)
        .addReg(AfterLoadRestoreFrom, getKillRegState(true));
  }

  if (CopyBackToA) {
    Stack.pop(GB::AF);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::A)
        .addReg(DestReg, getKillRegState(true));
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

  StackAllocator Stack{RegsImmediatelyAfter, TII, MBBI};
  size_t SPOffest = MI.getOperand(1).getImm();
  auto DestReg = MI.getOperand(0).getReg();

  bool ResultIsHL =
      MI.definesRegister(GB::H, &TRI) || MI.definesRegister(GB::L, &TRI);
  bool AllowedToClobberHL =
      ResultIsHL || RegsImmediatelyAfter.available(MRI, GB::HL);

  if (not AllowedToClobberHL) {
    // TODO GB: either use a temporary register here or optimize push patterns
    Stack.save(GB::HL);
  }

  if (not RegsImmediatelyAfter.available(MRI, GB::F)) {
    // ld HL, SP + 1 clobbers F
    Stack.save(GB::AF);
    assert(DestReg != GB::AF);
  }

  auto CopyLowerTo = TRI.getSubReg(DestReg, 1);
  auto CopyUpperTo = TRI.getSubReg(DestReg, 2);

  if (ResultIsHL) {
    assert(CopyLowerTo == GB::L);
    assert(CopyUpperTo == GB::H);

    if (Stack.availableCount() == 0) {
      Stack.save(GB::AF);
    }

    MCRegister ScratchReg = Stack.reserveReg();
    assert(ScratchReg.isValid());
    CopyLowerTo = ScratchReg;
  }

  // Finally do the copy:
  loadHLWithStackOffset(MBB, MBBI, MI.getDebugLoc(), TII,
                        SPOffest + Stack.currentOffset());

  // TODO GB: always scavenge GB::A (saves 4 cycles if its available)
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), CopyLowerTo);
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::INC16), GB::HL)
      .addReg(GB::HL);

  // TODO GB: do I need to mark HL as killed after this?
  BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL), CopyUpperTo);

  if (ResultIsHL) {
    assert(CopyLowerTo != GB::L);
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_rr), GB::L)
        .addReg(CopyLowerTo, getKillRegState(true));
  }
}

bool GBStackSlotLowering::runOnMachineFunction(MachineFunction &MF) {
  if (GBDisableStackSlotLowering) {
    return false;
  }

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

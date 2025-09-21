#include "GBFrameLowering.h"
#include "GBMachineFunctionInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

#include <cstdint>

using namespace llvm;

GBFrameLowering::GBFrameLowering(const GBSubtarget &STI)
    : TargetFrameLowering(StackGrowsDown, Align(2),
                          /* LocalAreaOffset = */ -2) {}

void GBFrameLowering::emitPrologue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {

  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const GBMachineFunctionInfo *GBFI = MF.getInfo<GBMachineFunctionInfo>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  assert(&MF.front() == &MBB);
  assert(not MFI.hasVarSizedObjects());

  MachineBasicBlock::iterator MBBI = MBB.getFirstNonDebugInstr();

  uint64_t StackSize = MFI.getStackSize() - GBFI->getCalleeSavedFrameSize();
  if (StackSize == 0) {
    return; // Nothing to do
  }

  // The following is valid:
  // push hl          (16)
  // ld hl, $0086     (12)
  // add hl, sp       (8)
  // ld sp, hl        (8)
  // ld hl, -$0086    (12)
  // add hl, sp       (8)
  // ld hl+, a        (8)
  // ld hl, h         (8)
  // ld a, h          (4)
  // Total = 80 cycles, 12 bytes

  // Or alternatively:
  // add sp, 127    (16)
  // add sp, 127    (16)
  // Total = 16 * (offset / 128) cycles, 2 * (offset / 127) bytes
  // Cheaper for 0x27b offsets

  // Let's just generate the `add sp, r8` sequence. An offset larger than 0x27b
  // is a sign of another underlying problem.
  // This is also optimal for small offsets
  assert(getStackAlign() == 2 &&
         "Stack alignments larger than the ABI are not currently supported");
  size_t AdjustedSize = alignTo(StackSize, getStackAlign());
  MFI.setStackSize(AdjustedSize);
  assert(isUInt<16>(AdjustedSize));
  for (int RemainingAmount = AdjustedSize; RemainingAmount > 0;
       RemainingAmount -= 128) {
    int Adjust = std::min(RemainingAmount, 128);
    BuildMI(MBB, MBBI, DebugLoc{}, TII.get(GB::ADD_SP)).addImm(-Adjust);
  }
}

void GBFrameLowering::emitEpilogue(MachineFunction &MF,
                                   MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const GBMachineFunctionInfo *GBFI = MF.getInfo<GBMachineFunctionInfo>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  assert(not MFI.hasVarSizedObjects());

  DebugLoc DL;
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  if (MBB.end() != MBBI) {
    DL = MBBI->getDebugLoc();
  }

  uint64_t StackSize = MFI.getStackSize() - GBFI->getCalleeSavedFrameSize();
  if (StackSize == 0) {
    return; // Nothing to do
  }

  // StackSize has already been adjusted by the prologue inserter
  assert(isUInt<16>(StackSize));
  for (int RemainingSize = StackSize; RemainingSize > 0; RemainingSize -= 127) {
    int Adjust = std::min(RemainingSize, 127);
    BuildMI(MBB, MBBI, DebugLoc{}, TII.get(GB::ADD_SP)).addImm(Adjust);
  }
}

bool GBFrameLowering::hasFPImpl(const MachineFunction &MF) const {
  // FIXME GB: alloca
  return false;
}

bool GBFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // TOOD GB: setting this false and using a PUSH/POP sequence would likely be
  // very fast.
  return true;
}

MachineBasicBlock::iterator GBFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  if (hasReservedCallFrame(MF)) {
    return MBB.erase(MI);
  }

  llvm_unreachable("eliminateCallFramePseudoInstr");

  // TODO GB: convert this to push/ pop sequence
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  DebugLoc DL = MI->getDebugLoc();
  GBMachineFunctionInfo *GBFI = MF.getInfo<GBMachineFunctionInfo>();

  int Amount = TII.getFrameSize(*MI) + GBFI->getCalleeSavedFrameSize();
  if (Amount == 0) {
    return MBB.erase(MI);
  }

  if (MI->getOpcode() == GB::ADJCALLSTACKDOWN) {
    Amount *= -1;
  }
  BuildMI(MBB, MI, DL, TII.get(GB::ADD_SP)).addImm(Amount);
  return MBB.erase(MI);
}

bool GBFrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    ArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.size() == 0) {
    return false;
  }

  DebugLoc DL = MBB.findDebugLoc(MI);
  MachineFunction &MF = *MBB.getParent();
  GBMachineFunctionInfo *GBFI = MF.getInfo<GBMachineFunctionInfo>();
  const GBSubtarget &STI = MF.getSubtarget<GBSubtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();

  unsigned CalleeFrameSize = 0;
  for (const auto &Info : CSI) {
    MCRegister Reg = Info.getReg();
    assert(Reg.isPhysical());
    assert(TRI->getRegSizeInBits(*TRI->getMinimalPhysRegClass(Reg)) == 16);

    bool IsNotLiveIn = !MBB.isLiveIn(Reg);

    // A subregister might be live-in (if passed as an argument)
    if (IsNotLiveIn) {
      for (const auto &LiveIn : MBB.liveins()) {
        if (STI.getRegisterInfo()->isSubRegister(Reg, LiveIn.PhysReg)) {
          IsNotLiveIn = false;
          MBB.addLiveIn(Reg);
          break;
        }
      }
    }

    // We're going to reference this register in an instruction, mark it as a
    // liveIn. Don't mark it twice if its already passed as an argument.
    if (IsNotLiveIn) {
      MBB.addLiveIn(Reg);
    }

    BuildMI(MBB, MI, DL, TII.get(GB::PUSH))
        .addReg(Reg, getKillRegState(IsNotLiveIn))
        .setMIFlag(MachineInstr::FrameSetup);
    CalleeFrameSize += 2;
  }

  GBFI->setCalleeSavedFrameSize(CalleeFrameSize);

  return true;
}

bool GBFrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    MutableArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.size() == 0) {
    return false;
  }

  DebugLoc DL = MBB.findDebugLoc(MI);
  MachineFunction &MF = *MBB.getParent();
  const GBSubtarget &STI = MF.getSubtarget<GBSubtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();

  for (const auto &Info : llvm::reverse(CSI)) {
    MCRegister Reg = Info.getReg();
    assert(Reg.isPhysical());
    assert(TRI->getRegSizeInBits(*TRI->getMinimalPhysRegClass(Reg)) == 16);

    BuildMI(MBB, MI, DL, TII.get(GB::POP))
        .addDef(Reg)
        .setMIFlag(MachineInstr::FrameDestroy);
  }

  return true;
}

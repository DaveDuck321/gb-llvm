#ifndef LLVM_LIB_TARGET_GB_GBREGISTERINFO_H
#define LLVM_LIB_TARGET_GB_GBREGISTERINFO_H

#define GET_REGINFO_HEADER
#include "GBGenRegisterInfo.inc"

namespace llvm {

struct GBRegisterInfo : public GBGenRegisterInfo {
  GBRegisterInfo();

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;

  const uint32_t *getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  bool requiresRegisterScavenging(const MachineFunction &MF) const override;

  bool eliminateStackSlotFrameIndex(MachineBasicBlock::iterator MI, int Offset,
                                    RegScavenger &RS) const;

  bool eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;

  Register getFrameRegister(const MachineFunction &MF) const override;
};

} // namespace llvm

#endif

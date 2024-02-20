#include "GBInstrInfo.h"
#include "GB.h"
#include "GBRegisterInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/Support/ErrorHandling.h"
#include <iterator>

#define GET_INSTRINFO_CTOR_DTOR
#include "GBGenInstrInfo.inc"

using namespace llvm;

GBInstrInfo::GBInstrInfo() : GBGenInstrInfo() {}

void GBInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MBBI,
                              const DebugLoc &DL, MCRegister DestReg,
                              MCRegister SrcReg, bool KillSrc) const {

  // 8-bit copy
  if (GB::GPR8RegClass.contains(SrcReg, DestReg)) {
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestReg).addReg(SrcReg);
    return;
  }

  // 16-bit copy (copy lower half then upper half)
  // Applies to BC, DE, HL, (but not AF, SP)
  const auto IsSimpleCombinedGPR16 = [](MCRegister Reg, unsigned &Top,
                                        unsigned &Bottom) {
    switch (Reg) {
    case GB::BC:
      Top = GB::B;
      Bottom = GB::C;
      return true;
    case GB::DE:
      Top = GB::D;
      Bottom = GB::E;
      return true;
    case GB::HL:
      Top = GB::H;
      Bottom = GB::L;
      return true;
    default:
      return false;
    }
  };

  if (unsigned SrcA, SrcB, DestA, DestB;
      IsSimpleCombinedGPR16(SrcReg, SrcA, SrcB) &&
      IsSimpleCombinedGPR16(DestReg, DestA, DestB)) {

    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestA).addReg(SrcA);
    BuildMI(MBB, MBBI, DL, get(GB::LD_rr), DestB).addReg(SrcB);
    return;
  }

  // Special case: 16-bit copy from HL to SP
  if (SrcReg == GB::HL && DestReg == GB::SP) {
    BuildMI(MBB, MBBI, DL, get(GB::LD_SP_HL), DestReg).addReg(SrcReg);
  }

  llvm_unreachable("Unsupported register copy!");
}

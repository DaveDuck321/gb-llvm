#include "GB.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
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
    GBDisableStackSlotLowering("gb-disable-ld-lowering", cl::Hidden,
                               cl::desc("Disables GBLDLowering"));

namespace {

class GBLDLowering final : public MachineFunctionPass {
public:
  static char ID;

  GBLDLowering(GBTargetMachine &TargetMachine, CodeGenOptLevel OptLevel);

  StringRef getPassName() const override;

  bool runOnMachineFunction(MachineFunction &MF) override;

  void convertToConcreteLoad(MachineInstr &MI) const;
  void convertToConcreteStore(MachineInstr &MI) const;
};

} // namespace

GBLDLowering::GBLDLowering(GBTargetMachine &TargetMachine,
                           CodeGenOptLevel OptLevel)
    : MachineFunctionPass(ID) {}

StringRef GBLDLowering::getPassName() const { return "GB LD lowering"; }

void GBLDLowering::convertToConcreteLoad(MachineInstr &MI) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();

  assert(MI.getOpcode() == GB::LD_A_iGPR16);
  assert(MI.getOperand(0).isReg());

  Register PtrReg = MI.getOperand(0).getReg();
  switch ((unsigned)PtrReg) {
  case GB::HL:
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_r_iHL)).addDef(GB::A);
    break;
  case GB::BC:
  case GB::DE:
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_A_iR16)).addReg(PtrReg);
    break;
  }
}

void GBLDLowering::convertToConcreteStore(MachineInstr &MI) const {
  auto &MF = *MI.getMF();
  auto &MBB = *MI.getParent();
  auto &TII = *MF.getSubtarget().getInstrInfo();

  MachineBasicBlock::iterator MBBI = MI.getIterator();

  assert(MI.getOpcode() == GB::LD_iGPR16_A);
  assert(MI.getOperand(0).isReg());

  Register PtrReg = MI.getOperand(0).getReg();
  switch ((unsigned)PtrReg) {
  case GB::HL:
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iHL_r)).addReg(GB::A);
    break;
  case GB::BC:
  case GB::DE:
    BuildMI(MBB, MBBI, MI.getDebugLoc(), TII.get(GB::LD_iR16_A)).addReg(PtrReg);
    break;
  }
}

bool GBLDLowering::runOnMachineFunction(MachineFunction &MF) {
  // During the pattern match, we produce LD_A_iGPR16 and LD_iGPR16_A
  // LD_A_iGPR16 is lowered to L_A_iHL or LD_A_iR16 depending on the 16-bit
  // source register. Its worth noting that we can further optimize this by
  // detecting cases where L_r8_iHL would be more optimal than L_A_iHL.
  bool MadeChanges = false;
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      switch (MI.getOpcode()) {
      default:
        continue;
      case GB::LD_A_iGPR16:
        convertToConcreteLoad(MI);
        break;
      case GB::LD_iGPR16_A:
        convertToConcreteStore(MI);
        break;
      }
      MI.removeFromParent();
      MadeChanges = true;
    }
  }
  return MadeChanges;
}

char GBLDLowering::ID = 0;
FunctionPass *llvm::createGBLDLowering(GBTargetMachine &TM,
                                       CodeGenOptLevel OptLevel) {
  return new GBLDLowering(TM, OptLevel);
}

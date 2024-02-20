#include "GB.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "gb-mcinstlower"

void llvm::LowerGBMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI) {
  OutMI.setOpcode(MI->getOpcode());

  LLVM_DEBUG(dbgs() << "MI=" << *MI);

  for (const MachineOperand &MO : MI->operands()) {
    MCOperand MCOp;
    switch (MO.getType()) {
    default:
      report_fatal_error("LowerGBMachineInstrToMCInst: unknown type");
    case MachineOperand::MO_Register:
      if (MO.isImplicit()) {
        continue;
      }
      MCOp = MCOperand::createReg(MO.getReg());
      break;
    case MachineOperand::MO_Immediate:
      MCOp = MCOperand::createImm(MO.getImm());
      break;
    }
    OutMI.addOperand(MCOp);
  }
}

#include "GB.h"
#include "MCTargetDesc/GBMCExpr.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/Debug.h"

#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "gb-mcinstlower"

static MCOperand createGBExpression(const MCExpr *Expr, MCContext &Ctx) {
  return MCOperand::createExpr(
      GBMCExpr::create(Expr, GBMCExpr::SPECIFIER_NONE, Ctx));
}

void llvm::LowerGBMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                       AsmPrinter &AP) {
  OutMI.setOpcode(MI->getOpcode());

  LLVM_DEBUG(dbgs() << "MBB=" << MI->getParent()->getName();
             dbgs() << " MI=" << *MI);

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
    case MachineOperand::MO_RegisterMask:
      continue;
    case MachineOperand::MO_MachineBasicBlock:
      MCOp = createGBExpression(
          MCSymbolRefExpr::create(MO.getMBB()->getSymbol(), AP.OutContext),
          AP.OutContext);
      break;
    case MachineOperand::MO_GlobalAddress: {
      MCExpr const *Expr =
          MCSymbolRefExpr::create(AP.getSymbol(MO.getGlobal()), AP.OutContext);
      if (MO.getOffset() != 0) {
        Expr = MCBinaryExpr::createAdd(
            Expr, MCConstantExpr::create(MO.getOffset(), AP.OutContext),
            AP.OutContext);
      }
      MCOp = createGBExpression(Expr, AP.OutContext);
      break;
    }
    case MachineOperand::MO_BlockAddress:
      assert(MO.getOffset() == 0);
      MCOp = createGBExpression(
          MCSymbolRefExpr::create(
              AP.GetBlockAddressSymbol(MO.getBlockAddress()), AP.OutContext),
          AP.OutContext);
      break;
    case MachineOperand::MO_ExternalSymbol:
      assert(MO.getOffset() == 0);
      MCOp = createGBExpression(
          MCSymbolRefExpr::create(
              AP.GetExternalSymbolSymbol(MO.getSymbolName()), AP.OutContext),
          AP.OutContext);
      break;
    case MachineOperand::MO_Immediate:
      MCOp = MCOperand::createImm(MO.getImm());
      break;
    }
    OutMI.addOperand(MCOp);
  }
}

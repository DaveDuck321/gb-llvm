#include "GB.h"
#include "GBMOFlags.h"
#include "MCTargetDesc/GBMCExpr.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "gb-mcinstlower"

static MCOperand createGBExpression(const MachineOperand &MO,
                                    const MCSymbol *Symbol, int64_t Offset,
                                    MCContext &Ctx) {
  const MCExpr *Expr = MCSymbolRefExpr::create(Symbol, Ctx);
  if (Offset != 0) {
    Expr =
        MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Offset, Ctx), Ctx);
  }

  GBMCExpr::SymbolSpecifier Specifier = GBMCExpr::SPECIFIER_NONE;
  switch (getGBFlag(MO)) {
  default:
    llvm_unreachable("Unhandled flag");
  case GBMOFlag::NONE:
    break;
  case GBMOFlag::LOWER_PART:
    Specifier = GBMCExpr::SPECIFIER_LO_16;
    break;
  case GBMOFlag::UPPER_PART:
    Specifier = GBMCExpr::SPECIFIER_HI_16;
    break;
  }
  return MCOperand::createExpr(GBMCExpr::create(Expr, Specifier, Ctx));
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
      MCOp = createGBExpression(MO, MO.getMBB()->getSymbol(), 0, AP.OutContext);
      break;
    case MachineOperand::MO_GlobalAddress: {
      MCOp = createGBExpression(MO, AP.getSymbol(MO.getGlobal()),
                                MO.getOffset(), AP.OutContext);
      break;
    }
    case MachineOperand::MO_BlockAddress:
      assert(MO.getOffset() == 0);
      MCOp =
          createGBExpression(MO, AP.GetBlockAddressSymbol(MO.getBlockAddress()),
                             MO.getOffset(), AP.OutContext);
      break;
    case MachineOperand::MO_ExternalSymbol:
      assert(MO.getOffset() == 0);
      MCOp =
          createGBExpression(MO, AP.GetExternalSymbolSymbol(MO.getSymbolName()),
                             MO.getOffset(), AP.OutContext);
      break;
    case MachineOperand::MO_Immediate:
      MCOp = MCOperand::createImm(MO.getImm());
      break;
    }
    OutMI.addOperand(MCOp);
  }
}

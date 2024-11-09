#ifndef LLVM_LIB_TARGET_GB_GB_H
#define LLVM_LIB_TARGET_GB_GB_H

#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class GBTargetMachine;
class MachineInstr;
class MCInst;
class AsmPrinter;

class FunctionPass;

FunctionPass *createGBISelDag(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBStackSlotLowering(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBLDLowering(GBTargetMachine &, CodeGenOptLevel);

void LowerGBMachineInstrToMCInst(const MachineInstr *, MCInst &OutMI,
                                 AsmPrinter &);

} // namespace llvm

#endif

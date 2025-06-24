#ifndef LLVM_LIB_TARGET_GB_GB_H
#define LLVM_LIB_TARGET_GB_GB_H

#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class GBTargetMachine;
class MachineInstr;
class MCInst;
class AsmPrinter;
class PassRegistry;

class FunctionPass;

FunctionPass *createGBISelDag(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBStackSlotLowering(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBPushPopCombine(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBInstructionRelaxation(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBBranchRelaxation(GBTargetMachine &, CodeGenOptLevel);

void LowerGBMachineInstrToMCInst(const MachineInstr *, MCInst &OutMI,
                                 AsmPrinter &);

void initializeGBDAGToDAGISelLegacyPass(PassRegistry &);

} // namespace llvm

#endif

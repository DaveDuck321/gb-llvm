#ifndef LLVM_LIB_TARGET_GB_GB_H
#define LLVM_LIB_TARGET_GB_GB_H

#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class AsmPrinter;
class FunctionPass;
class GBRegisterBankInfo;
class GBSubtarget;
class GBTargetMachine;
class InstructionSelector;
class MachineInstr;
class MCInst;
class PassRegistry;

FunctionPass *createGBISelDag(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBStackSlotLowering(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBPushPopCombine(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBInstructionRelaxation(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBBranchRelaxation(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBFoldImmediates(GBTargetMachine &, CodeGenOptLevel);
FunctionPass *createGBEarlyLowerIntoStack(GBTargetMachine &);

FunctionPass *createGBPreLegalizeCombiner(CodeGenOptLevel);
FunctionPass *createGBPostLegalizeCombiner(CodeGenOptLevel);
FunctionPass *createGBPostLegalizeExpand(CodeGenOptLevel);
FunctionPass *createGBCFIInserter();

InstructionSelector *createGBInstructionSelector(const GBSubtarget &,
                                                 const GBRegisterBankInfo &);

void LowerGBMachineInstrToMCInst(const MachineInstr *, MCInst &OutMI,
                                 AsmPrinter &);

void initializeGBDAGToDAGISelLegacyPass(PassRegistry &);

} // namespace llvm

#endif

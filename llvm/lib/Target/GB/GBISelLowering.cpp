#include "GBISelLowering.h"
#include "GBRegisterInfo.h"
#include "GBSubtarget.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/RuntimeLibcalls.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

#define DEBUG_TYPE "gb-lower"

GBTargetLowering::GBTargetLowering(const TargetMachine &TM,
                                   const GBSubtarget &STI)
    : TargetLowering(TM) {
  addRegisterClass(MVT::i8, &GB::GPR8RegClass);
  addRegisterClass(MVT::i16, &GB::GPR16RegClass);
  computeRegisterProperties(STI.getRegisterInfo());

  // setStackPointerRegisterToSaveRestore(GB::SP);
  setMinFunctionAlignment(Align{2});
  setPrefFunctionAlignment(Align{2});
  setMinStackArgumentAlignment(Align{1});
  setMinimumJumpTableEntries(INT_MAX); // Disable jump tables

  // Undefined bools allow a fast setcc implementation using the rla instruction
  // This makes select 4 cycles slower compared to
  // ZeroOrNegativeOneBooleanContent but makes setcc 12-20 cycles faster.
  setBooleanContents(UndefinedBooleanContent);

  // Might as well lie here since everything larger than an i8 will be a
  // __sync_lock call anyway.
  setSupportsUnalignedAtomics(true);

  // All the hand-written runtime libcalls are implemented in fastcc
  setLibcallCallingConv(RTLIB::Libcall::MEMCPY, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::MEMMOVE, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::MEMSET, CallingConv::Fast);

  setLibcallCallingConv(RTLIB::Libcall::SHL_I16, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::SHL_I32, CallingConv::Fast);

  setLibcallCallingConv(RTLIB::Libcall::SRA_I16, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::SRL_I16, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::SRA_I32, CallingConv::Fast);
  setLibcallCallingConv(RTLIB::Libcall::SRL_I32, CallingConv::Fast);

  setSchedulingPreference(Sched::RegPressure);
}

bool GBTargetLowering::useSoftFloat() const { return true; }

bool GBTargetLowering::isSelectSupported(SelectSupportKind) const {
  // We don't have a select instruction: inform the optimizer
  // This will expand selects into brcond... but only with optimizations on.
  return false;
}

bool GBTargetLowering::allowsMisalignedMemoryAccesses(EVT, unsigned AddrSpace,
                                                      Align,
                                                      MachineMemOperand::Flags,
                                                      unsigned *Fast) const {
  if (Fast != nullptr) {
    *Fast = 1;
  }
  return true;
}

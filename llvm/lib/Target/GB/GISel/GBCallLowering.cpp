#include "GBCallLowering.h"
#include "GBISelLowering.h"
#include "GBRegisterInfo.h"
#include "GBSubtarget.h"
#include "MCTargetDesc/GBMCTargetDesc.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/FunctionLoweringInfo.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#include "GBGenCallingConv.inc"

struct GBOutgoingArgHandler : public CallLowering::OutgoingValueHandler {
  MachineInstrBuilder MIB;

  GBOutgoingArgHandler(MachineIRBuilder &MIRBuilder, MachineRegisterInfo &MRI,
                       MachineInstrBuilder MIB)
      : OutgoingValueHandler(MIRBuilder, MRI), MIB{MIB} {}

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    assert(VA.getLocInfo() == CCValAssign::Full);

    MIB.addUse(PhysReg, RegState::Implicit);
    MIRBuilder.buildCopy(PhysReg, ValVReg);
  }

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    assert(VA.getLocInfo() == CCValAssign::Full);

    MachineFunction &MF = MIRBuilder.getMF();
    auto *MMO = MF.getMachineMemOperand(MPO, MachineMemOperand::MOStore, MemTy,
                                        inferAlignFromPtrInfo(MF, MPO));
    MIRBuilder.buildStore(ValVReg, Addr, *MMO);
  }

  Register getStackAddress(uint64_t Size, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    MachineFunction &MF = MIRBuilder.getMF();
    LLT P0 = LLT::pointer(0, 16);
    MIRBuilder.buildInstr(GB::LD_HL_SP).addImm(Offset);
    auto ResReg = MIRBuilder.buildCopy(P0, Register(GB::HL));
    MPO = MachinePointerInfo::getStack(MF, Offset);
    return ResReg.getReg(0);
  }
};

struct GBIncomingValueHandler : public CallLowering::IncomingValueHandler {
  GBIncomingValueHandler(MachineIRBuilder &MIRBuilder, MachineRegisterInfo &MRI)
      : IncomingValueHandler(MIRBuilder, MRI) {}

  void assignValueToAddress(Register ValVReg, Register Addr, LLT MemTy,
                            const MachinePointerInfo &MPO,
                            const CCValAssign &VA) override {
    MachineFunction &MF = MIRBuilder.getMF();
    auto *MMO = MF.getMachineMemOperand(MPO, MachineMemOperand::MOLoad, MemTy,
                                        inferAlignFromPtrInfo(MF, MPO));
    MIRBuilder.buildLoad(ValVReg, Addr, *MMO);
  }

  virtual void markPhysRegUsed(MCRegister PhysReg) {
    MIRBuilder.getMRI()->addLiveIn(PhysReg);
    MIRBuilder.getMBB().addLiveIn(PhysReg);
  }

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        const CCValAssign &VA) override {
    markPhysRegUsed(PhysReg);
    CallLowering::IncomingValueHandler::assignValueToReg(ValVReg, PhysReg, VA);
  }

  Register getStackAddress(uint64_t MemSize, int64_t Offset,
                           MachinePointerInfo &MPO,
                           ISD::ArgFlagsTy Flags) override {
    auto &MFI = MIRBuilder.getMF().getFrameInfo();
    const bool IsImmutable = !Flags.isByVal();

    int FI = MFI.CreateFixedObject(MemSize, Offset, IsImmutable);
    MPO = MachinePointerInfo::getFixedStack(MIRBuilder.getMF(), FI);
    return MIRBuilder.buildFrameIndex(LLT::pointer(0, 16), FI).getReg(0);
  }
};

struct GBCallReturnHandler : public GBIncomingValueHandler {
  GBCallReturnHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                      MachineInstrBuilder &MIB)
      : GBIncomingValueHandler(B, MRI), MIB(MIB) {}

  void markPhysRegUsed(MCRegister PhysReg) override {
    MIB.addDef(PhysReg, RegState::Implicit);
  }

  MachineInstrBuilder MIB;
};

GBCallLowering::GBCallLowering(const GBTargetLowering &TLI)
    : CallLowering(&TLI) {}

bool GBCallLowering::lowerReturn(MachineIRBuilder &MIRBuilder, const Value *Val,
                                 ArrayRef<Register> VRegs,
                                 FunctionLoweringInfo &FLI) const {
  MachineFunction &MF = MIRBuilder.getMF();
  Function &F = MF.getFunction();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const DataLayout &DL = MF.getDataLayout();

  auto CallConv = MF.getFunction().getCallingConv();
  if (not is_contained({CallingConv::C, CallingConv::Cold, CallingConv::Fast,
                        CallingConv::GB_Interrupt},
                       CallConv)) {
    report_fatal_error("Unsupported calling convention");
  }

  if (not FLI.CanLowerReturn) {
    insertSRetStores(MIRBuilder, Val->getType(), VRegs, FLI.DemoteRegister);
    MIRBuilder.buildInstr(GB::RET);
    return true;
  }

  // The interrupt calling convention sneakily also reenables interrupts on
  // return. This is a little counter-intuitive but let's just assume that the
  // only caller is the interrupt handler itself.
  auto Opcode = CallConv == CallingConv::GB_Interrupt ? GB::RETI : GB::RET;
  MachineInstrBuilder MIB = MIRBuilder.buildInstrNoInsert(Opcode);

  if (Val != nullptr) {
    SmallVector<ArgInfo, 8> SplitArgs;
    ArgInfo OrigArg{VRegs, Val->getType(), 0};
    setArgFlags(OrigArg, AttributeList::ReturnIndex, DL, F);
    splitToValueTypes(OrigArg, SplitArgs, DL, F.getCallingConv());

    OutgoingValueAssigner ArgAssigner(RetCC_GB);
    GBOutgoingArgHandler ArgHandler(MIRBuilder, MRI, MIB);
    bool Success = determineAndHandleAssignments(
        ArgHandler, ArgAssigner, SplitArgs, MIRBuilder, F.getCallingConv(),
        F.isVarArg());
    assert(Success);
  }
  MIRBuilder.insertInstr(MIB);
  return true;
}

bool GBCallLowering::canLowerReturn(MachineFunction &MF,
                                    CallingConv::ID CallConv,
                                    SmallVectorImpl<BaseArgInfo> &Outs,
                                    bool IsVarArg) const {
  assert(not IsVarArg);
  assert(is_contained({CallingConv::C, CallingConv::Cold, CallingConv::Fast,
                       CallingConv::GB_Interrupt},
                      CallConv));
  if (Outs.size() == 0) {
    return true;
  }

  if (CallConv == CallingConv::GB_Interrupt) {
    llvm_unreachable("GB_Interrupt must return void");
  }

  if (Outs.size() == 1) {
    return Outs[0].Ty->getScalarSizeInBits() <= 16;
  }

  // TODO GB: support multiple 8-bit returns
  return false;
}

bool GBCallLowering::lowerFormalArguments(MachineIRBuilder &MIRBuilder,
                                          const Function &F,
                                          ArrayRef<ArrayRef<Register>> VRegs,
                                          FunctionLoweringInfo &FLI) const {
  MachineFunction &MF = MIRBuilder.getMF();
  const DataLayout &DL = MF.getDataLayout();
  MachineRegisterInfo &MRI = MF.getRegInfo();

  SmallVector<ArgInfo, 8> ArgInfos;
  if (not FLI.CanLowerReturn) {
    insertSRetIncomingArgument(F, ArgInfos, FLI.DemoteRegister, MRI, DL);
  }

  unsigned I = 0;
  for (auto &Arg : F.args()) {
    ArgInfo AInfo(VRegs[I], Arg, I);
    setArgFlags(AInfo, I + AttributeList::FirstArgIndex, DL, F);
    splitToValueTypes(AInfo, ArgInfos, DL, F.getCallingConv());
    ++I;
  }

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(F.getCallingConv(), F.isVarArg(), MF, ArgLocs, F.getContext());

  auto *AssignFn = [&] {
    switch (F.getCallingConv()) {
    case CallingConv::GB_Interrupt:
      return CC_GB_Interrupt;
    case CallingConv::Fast:
      return CC_GB_Fast;
    case CallingConv::C:
    case CallingConv::Cold:
      return CC_GB;
    default:
      llvm_unreachable("Unrecoginzed calling convention");
    }
  }();
  IncomingValueAssigner ArgAssigner(AssignFn);
  GBIncomingValueHandler ArgHandler(MIRBuilder, MRI);

  return determineAndHandleAssignments(ArgHandler, ArgAssigner, ArgInfos,
                                       MIRBuilder, F.getCallingConv(),
                                       F.isVarArg());
}

bool GBCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                               CallLoweringInfo &Info) const {
  MachineFunction &MF = MIRBuilder.getMF();
  Function &F = MF.getFunction();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  auto &DL = F.getDataLayout();
  const GBSubtarget &STI = MF.getSubtarget<GBSubtarget>();
  const GBRegisterInfo *TRI = STI.getRegisterInfo();

  SmallVector<ArgInfo, 8> OutArgs;
  for (auto &OrigArg : Info.OrigArgs) {
    splitToValueTypes(OrigArg, OutArgs, DL, Info.CallConv);
  }

  SmallVector<ArgInfo, 8> InArgs;
  if (!Info.OrigRet.Ty->isVoidTy()) {
    splitToValueTypes(Info.OrigRet, InArgs, DL, Info.CallConv);
  }

  auto CallSeqStart = MIRBuilder.buildInstr(GB::ADJCALLSTACKDOWN);

  auto Call = [&] {
    if (Info.Callee.isReg()) {
      assert(Info.CallConv != CallingConv::Fast);
      auto Call = MIRBuilder.buildInstrNoInsert(GB::CALL_HL);
      Call.addRegMask(TRI->getCallPreservedMask(MF, Info.CallConv));
      return Call;
    }

    auto Call = MIRBuilder.buildInstrNoInsert(GB::CALL);
    Call.add(Info.Callee);
    Call.addRegMask(TRI->getCallPreservedMask(MF, Info.CallConv));
    return Call;
  }();

  CCAssignFn *AssignFn = [&] {
    switch (Info.CallConv) {
    case CallingConv::GB_Interrupt:
      return CC_GB_Interrupt;
    case CallingConv::Fast:
      return CC_GB_Fast;
    case CallingConv::C:
    case CallingConv::Cold:
      return CC_GB;
    default:
      llvm_unreachable("Unrecoginzed calling convention");
    }
  }();
  OutgoingValueAssigner Assigner(AssignFn);
  GBOutgoingArgHandler Handler(MIRBuilder, MRI, Call);

  if (!determineAndHandleAssignments(Handler, Assigner, OutArgs, MIRBuilder,
                                     Info.CallConv, Info.IsVarArg)) {
    return false;
  }

  if (Info.Callee.isReg()) {
    constrainOperandRegClass(MF, *TRI, MRI, *STI.getInstrInfo(),
                             *STI.getRegBankInfo(), *Call, Call->getDesc(),
                             Info.Callee, 0);
    MIRBuilder.buildCopy(Register(GB::HL), Info.Callee);
  }

  if (Info.Callee.isReg()) {
    MIRBuilder.buildCopy(GB::HL, Info.Callee);
  }
  MIRBuilder.insertInstr(Call);

  CallSeqStart.addImm(Assigner.StackSize).addImm(0);

  auto CallSeqEnd = MIRBuilder.buildInstr(GB::ADJCALLSTACKUP);
  CallSeqEnd.addImm(Assigner.StackSize).addImm(0);

  if (Info.CanLowerReturn && !Info.OrigRet.Ty->isVoidTy()) {
    IncomingValueAssigner Assigner(RetCC_GB);
    GBCallReturnHandler Handler(MIRBuilder, MRI, Call);
    if (!determineAndHandleAssignments(Handler, Assigner, InArgs, MIRBuilder,
                                       Info.CallConv, Info.IsVarArg)) {
      return false;
    }
  }

  if (not Info.CanLowerReturn) {
    insertSRetLoads(MIRBuilder, Info.OrigRet.Ty, Info.OrigRet.Regs,
                    Info.DemoteRegister, Info.DemoteStackIndex);
  }

  return true;
}

#include "GB.h"
#include "TargetInfo/GBTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetMachine.h"

#include <memory>

using namespace llvm;

namespace {

class GBAsmPrinter : public AsmPrinter {
public:
  explicit GBAsmPrinter(TargetMachine &TM, std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)) {}

  void emitInstruction(const MachineInstr *) override;
};

} // namespace

void GBAsmPrinter::emitInstruction(const MachineInstr *MI) {
  MCInst Inst;
  LowerGBMachineInstrToMCInst(MI, Inst, *this);
  EmitToStreamer(*OutStreamer, Inst);
}

extern "C" void LLVMInitializeGBAsmPrinter() {
  RegisterAsmPrinter<GBAsmPrinter> _(getTheGBTarget());
}

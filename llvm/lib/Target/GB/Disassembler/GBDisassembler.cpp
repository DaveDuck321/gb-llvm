#include "TargetInfo/GBTargetInfo.h"

#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MathExtras.h"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>

#define DEBUG_TYPE "gb-disassembler"

using namespace llvm;

using DecodeStatus = MCDisassembler::DecodeStatus;

#define GET_REGINFO_ENUM
#include "GBGenRegisterInfo.inc"

// Functions required by tablegen
namespace {

unsigned GPR8DecodeTable[] = {GB::B, GB::C, GB::D, GB::E,
                              GB::H, GB::L, ~0u,   GB::A};
unsigned GPR16DecodeTable[] = {GB::BC, GB::DE, GB::HL, GB::SP};
unsigned SR16DecodeTable[] = {GB::BC, GB::DE, GB::HL, GB::AF};
unsigned IR16DecodeTable[] = {GB::BC, GB::DE};

DecodeStatus DecodeGPR8RegisterClass(MCInst &MI, uint64_t Encoding,
                                     uint64_t Addr, const void *Decoder) {
  // Corresponds to indirect (HL) -- which is not a register
  assert(Encoding != 6);
  assert((Encoding & ~0b111) == 0);

  MI.addOperand(MCOperand::createReg(GPR8DecodeTable[Encoding]));

  return DecodeStatus::Success;
}

DecodeStatus DecodeGPR16RegisterClass(MCInst &MI, uint64_t Encoding,
                                      uint64_t Addr, const void *Decoder) {
  assert((Encoding & ~0b11) == 0);

  MI.addOperand(MCOperand::createReg(GPR16DecodeTable[Encoding]));

  return DecodeStatus::Success;
}

DecodeStatus DecodeIntReg16RegisterClass(MCInst &MI, uint64_t Encoding,
                                         uint64_t Addr, const void *Decoder) {
  return DecodeGPR16RegisterClass(MI, Encoding, Addr, Decoder);
}

DecodeStatus DecodeSR16RegisterClass(MCInst &MI, uint64_t Encoding,
                                     uint64_t Addr, const void *Decoder) {
  assert((Encoding & ~0b11) == 0);

  MI.addOperand(MCOperand::createReg(SR16DecodeTable[Encoding]));

  return DecodeStatus::Success;
}

DecodeStatus DecodeIR16RegisterClass(MCInst &MI, uint64_t Encoding,
                                     uint64_t Addr, const void *Decoder) {
  assert((Encoding & ~0b1) == 0);

  MI.addOperand(MCOperand::createReg(IR16DecodeTable[Encoding]));

  return DecodeStatus::Success;
}

DecodeStatus DecodeRstVecOperand(MCInst &MI, uint64_t Encoding, uint64_t Addr,
                                 const void *Decoder) {
  assert((Encoding & ~0b111) == 0);

  MI.addOperand(MCOperand::createImm(Encoding << 3));

  return DecodeStatus::Success;
}

template <size_t SizeInBits>
DecodeStatus DecodeSImmOperand(MCInst &MI, uint64_t Encoding, uint64_t Addr,
                               const void *Decoder) {
  assert(Encoding < (1 << SizeInBits));

  MI.addOperand(MCOperand::createImm(SignExtend64<SizeInBits>(Encoding)));

  return DecodeStatus::Success;
}

template <size_t SizeInBits>
DecodeStatus DecodeUImmOperand(MCInst &MI, uint64_t Encoding, uint64_t Addr,
                               const void *Decoder) {
  assert(Encoding < (1 << SizeInBits));

  MI.addOperand(MCOperand::createImm(Encoding));

  return DecodeStatus::Success;
}

} // namespace

#include "GBGenDisassemblerTables.inc"

namespace {
class GBMCDisassembler : public MCDisassembler {
public:
  using MCDisassembler::MCDisassembler;

  DecodeStatus getInstruction(MCInst &MI, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Addr,
                              raw_ostream &CStream) const override {

    const std::array Tables = {DecoderTable8, DecoderTable16, DecoderTable24};

    // Return the first successful decoding
    uint64_t Inst = 0;
    for (unsigned I = 0; I < Tables.size(); I++) {
      if (I >= Bytes.size()) {
        break;
      }

      // Tablegen already supports native immediate endianness
      Inst |= Bytes[I] << (8 * I);

      const auto Status =
          decodeInstruction(Tables[I], MI, Inst, Addr, this, STI);

      if (Status == DecodeStatus::Success) {
        Size = I + 1;
        return DecodeStatus::Success;
      }
    }

    Size = 0;
    return DecodeStatus::Fail;
  }
};
} // namespace

static MCDisassembler *createGBMCDisassembler(const Target &T,
                                              const MCSubtargetInfo &STI,
                                              MCContext &Ctx) {
  return new GBMCDisassembler(STI, Ctx);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeGBDisassembler() {
  TargetRegistry::RegisterMCDisassembler(getTheGBTarget(),
                                         createGBMCDisassembler);
}

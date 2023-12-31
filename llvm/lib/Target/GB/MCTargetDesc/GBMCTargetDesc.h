#ifndef LLVM_LIB_TARGET_RISCV_MCTARGETDESC_GBMCTARGETDESC_H
#define LLVM_LIB_TARGET_RISCV_MCTARGETDESC_GBMCTARGETDESC_H

#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class raw_pwrite_stream;
class Target;

MCCodeEmitter *createGBMCCodeEmitter(const MCInstrInfo &, MCContext &);

MCAsmBackend *createGBAsmBackend(const Target &, const MCSubtargetInfo &,
                                 const MCRegisterInfo &,
                                 const MCTargetOptions &);

std::unique_ptr<MCObjectTargetWriter> createGBELFObjectWriter();
} // namespace llvm

#define GET_REGINFO_ENUM
#include "GBGenRegisterInfo.inc"

#define GET_INSTRINFO_ENUM
#include "GBGenInstrInfo.inc"

#endif

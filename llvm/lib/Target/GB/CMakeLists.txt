add_llvm_component_group(GB)

set(LLVM_TARGET_DEFINITIONS GB.td)

tablegen(LLVM GBGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM GBGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM GBGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM GBGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(GBCommonTableGen)

add_llvm_target(GBCodeGen
    GBTargetMachine.cpp

    LINK_COMPONENTS
    CodeGen
    Core
    GBInfo
    GBDesc
    MC
    Support
    Target

    ADD_TO_COMPONENT
    GB
    )

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)

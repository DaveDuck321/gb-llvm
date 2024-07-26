add_llvm_component_group(GB)

set(LLVM_TARGET_DEFINITIONS GB.td)

tablegen(LLVM GBGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM GBGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM GBGenCallingConv.inc -gen-callingconv)
tablegen(LLVM GBGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM GBGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM GBGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM GBGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM GBGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM GBGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(GBCommonTableGen)

add_llvm_target(GBCodeGen
    GBAsmPrinter.cpp
    GBFrameLowering.cpp
    GBInstrInfo.cpp
    GBISelDAGToDAG.cpp
    GBISelLowering.cpp
    GBMCInstLower.cpp
    GBRegisterInfo.cpp
    GBSubtarget.cpp
    GBTargetMachine.cpp
    GBStackSlotLowering.cpp

    LINK_COMPONENTS
    AsmPrinter
    CodeGen
    CodeGenTypes
    Core
    GBAsmParser
    GBDesc
    GBInfo
    MC
    SelectionDAG
    Support
    Target
    TargetParser
    TransformUtils

    ADD_TO_COMPONENT
    GB
    )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)

add_llvm_component_group(GB)

set(LLVM_TARGET_DEFINITIONS GB.td)

tablegen(LLVM GBGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM GBGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM GBGenCallingConv.inc -gen-callingconv)
tablegen(LLVM GBGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM GBGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM GBGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM GBGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM GBGenRegisterBank.inc -gen-register-bank)
tablegen(LLVM GBGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM GBGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM GBGenGlobalISel.inc -gen-global-isel)

tablegen(LLVM GBGenPreLegalizeGICombiner.inc -gen-global-isel-combiner
              -combiners="GBPreLegalizeCombiner")
tablegen(LLVM GBGenPostLegalizeGICombiner.inc -gen-global-isel-combiner
              -combiners="GBPostLegalizeCombiner")


add_public_tablegen_target(GBCommonTableGen)

add_llvm_target(GBCodeGen
    GBAsmPrinter.cpp
    GBBranchRelaxation.cpp
    GBCFIInserter.cpp
    GBEarlyLowerIntoStack.cpp
    GBFoldImmediates.cpp
    GBFrameLowering.cpp
    GBInstrInfo.cpp
    GBInstructionRelaxation.cpp
    GBISelDAGToDAG.cpp
    GBISelLowering.cpp
    GBMCInstLower.cpp
    GBPushPopCombine.cpp
    GBRegisterInfo.cpp
    GBStackSlotLowering.cpp
    GBSubtarget.cpp
    GBTargetMachine.cpp
    GISel/GBCallLowering.cpp
    GISel/GBCombinerCommon.cpp
    GISel/GBIncrementSerialize.cpp
    GISel/GBInstructionSelector.cpp
    GISel/GBLegalizerInfo.cpp
    GISel/GBPostLegalizeCombiner.cpp
    GISel/GBPostLegalizeExpand.cpp
    GISel/GBPreLegalizeCombiner.cpp
    GISel/GBRegisterBankInfo.cpp

    LINK_COMPONENTS
    AsmPrinter
    CodeGen
    CodeGenTypes
    Core
    GBAsmParser
    GBDesc
    GBInfo
    GlobalISel
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

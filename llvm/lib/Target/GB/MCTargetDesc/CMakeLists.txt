add_llvm_component_library(LLVMGBDesc
    GBAsmBackend.cpp
    GBELFObjectWriter.cpp
    GBMCCodeEmitter.cpp
    GBMCTargetDesc.cpp

    LINK_COMPONENTS
    MC
    GBInfo
    Support
    TargetParser

    ADD_TO_COMPONENT
    GB
    )

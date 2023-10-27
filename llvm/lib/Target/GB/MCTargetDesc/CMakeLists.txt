add_llvm_component_library(LLVMGBDesc
    GBMCTargetDesc.cpp

    LINK_COMPONENTS
    MC
    GBInfo
    Support
    TargetParser

    ADD_TO_COMPONENT
    GB
    )

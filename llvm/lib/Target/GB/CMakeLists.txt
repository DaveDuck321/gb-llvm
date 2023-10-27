add_llvm_component_group(GB)

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

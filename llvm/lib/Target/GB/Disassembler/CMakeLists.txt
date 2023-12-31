add_llvm_component_library(LLVMGBDisassembler
  GBDisassembler.cpp

  LINK_COMPONENTS
  GBDesc
  GBInfo
  MC
  MCDisassembler
  Support

  ADD_TO_COMPONENT
  GB
  )

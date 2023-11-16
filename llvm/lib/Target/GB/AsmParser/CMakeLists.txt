add_llvm_component_library(LLVMGBAsmParser
  GBAsmParser.cpp

  LINK_COMPONENTS
  GBDesc
  GBInfo
  CodeGenTypes
  MC
  MCParser
  Support

  ADD_TO_COMPONENT
  GB
  )

add_llvm_component_library(LLVMGBInfo
  GBTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  GB
  )

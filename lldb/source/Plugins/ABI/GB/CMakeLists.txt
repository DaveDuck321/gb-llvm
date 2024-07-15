add_lldb_library(lldbPluginABIGB PLUGIN
  ABIcustom_gb.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
  LINK_COMPONENTS
    Support
    TargetParser
  )

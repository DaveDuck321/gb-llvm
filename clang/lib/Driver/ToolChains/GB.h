#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_GB_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_GB_H

#include "ToolChains/Gnu.h"
#include "clang/Driver/ToolChain.h"
#include "llvm/Support/Compiler.h"
#include "llvm/TargetParser/Triple.h"

namespace clang::driver::toolchains {

class LLVM_LIBRARY_VISIBILITY GBToolchain : public ToolChain {
  Tool *buildLinker() const override;

  void
  addClangTargetOptions(const llvm::opt::ArgList &DriverArgs,
                        llvm::opt::ArgStringList &CC1Args,
                        Action::OffloadKind DeviceOffloadKind) const override;

  const char *getDefaultLinker() const override { return "lld"; }

  RuntimeLibType GetDefaultRuntimeLibType() const override {
    return ToolChain::RLT_CompilerRT;
  }

  CXXStdlibType GetDefaultCXXStdlibType() const override {
    return ToolChain::CST_Libcxx;
  }

  bool HasNativeLLVMSupport() const override { return true; }
  bool isBareMetal() const override { return true; }
  bool isPICDefault() const override { return false; }
  bool isPIEDefault(const llvm::opt::ArgList &Args) const override {
    return false;
  }
  bool isPICDefaultForced() const override { return true; }

public:
  GBToolchain(const Driver &, const llvm::Triple &, const llvm::opt::ArgList &);
};

} // namespace clang::driver::toolchains

namespace clang::driver::tools::GB {

class LLVM_LIBRARY_VISIBILITY Linker : public Tool {
  bool hasIntegratedCPP() const override { return false; }

public:
  Linker(const ToolChain &TC) : Tool("GB::Linker", "lld", TC) {}

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

} // namespace clang::driver::tools::GB

#endif

#include "GB.h"
#include "CommonArgs.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Job.h"
#include "clang/Driver/ToolChain.h"
#include "llvm/Support/Path.h"

#include <memory>
#include <string_view>

using namespace clang::driver::toolchains;
using namespace clang::driver;
using namespace clang::driver::tools;
using namespace llvm::opt;
using namespace llvm;

GBToolchain::GBToolchain(const Driver &driver, const llvm::Triple &target,
                         const llvm::opt::ArgList &args)
    : ToolChain(driver, target, args) {
  // Add our build directory to the search path to avoid finding the host's lld
  getProgramPaths().push_back(getDriver().Dir);
}

Tool *GBToolchain::buildLinker() const { return new GB::Linker(*this); }

void GBToolchain::addClangTargetOptions(
    const llvm::opt::ArgList &DriverArgs, llvm::opt::ArgStringList &CC1Args,
    Action::OffloadKind DeviceOffloadKind) const {
  CC1Args.push_back("-nostdsysteminc");

  // Needed to fit compiler-rt into imem
  CC1Args.push_back("-fdata-sections");
  CC1Args.push_back("-ffunction-sections");
}

void GB::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                              const InputInfo &Output,
                              const InputInfoList &Inputs,
                              const llvm::opt::ArgList &TCArgs,
                              const char *LinkingOutput) const {
  const ToolChain &ToolChain = getToolChain();
  const Driver &D = ToolChain.getDriver();
  ArgStringList CmdArgs;

  if (!D.SysRoot.empty()) {
    CmdArgs.push_back(TCArgs.MakeArgString("--sysroot=" + D.SysRoot));
  }

  bool LinkerIsLLD;
  auto Linker = ToolChain.GetLinkerPath(&LinkerIsLLD);
  if (not LinkerIsLLD) {
    D.Diag(diag::err_drv_lld_only);
  }

  if (not TCArgs.hasArg(options::OPT_nostdlib, options::OPT_nostartfiles)) {
  }

  AddLinkerInputs(ToolChain, Inputs, TCArgs, CmdArgs, JA);

  // Needed to fit compiler-rt into imem
  CmdArgs.push_back("--gc-sections");

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  auto RtLib = ToolChain.GetRuntimeLibType(TCArgs);
  assert((RtLib == ToolChain::RLT_CompilerRT) && "unknown runtime library");

  if (not TCArgs.hasArg(options::OPT_T)) {
    for (const auto &LibPath : ToolChain.getLibraryPaths()) {
      SmallString<128> LDScript(LibPath);
      llvm::sys::path::append(LDScript, "ldscripts/gb.ld");

      if (llvm::sys::fs::exists(LibPath)) {
        CmdArgs.push_back(TCArgs.MakeArgString("-T" + LDScript));
        break;
      }
    }
  } else {
    TCArgs.AddAllArgs(CmdArgs, options::OPT_T);
  }

  if (RtLib == ToolChain::RLT_CompilerRT) {
    auto add_lib_rt_component = [&](std::string_view name,
                                    ToolChain::FileType type) {
      std::string RtLib = getToolChain().getCompilerRT(TCArgs, name, type);
      if (llvm::sys::fs::exists(RtLib)) {
        CmdArgs.push_back(TCArgs.MakeArgString(RtLib));
      }
    };
    add_lib_rt_component("builtins", ToolChain::FT_Static);
    add_lib_rt_component("crtbegin", ToolChain::FT_Object);
    add_lib_rt_component("crtend", ToolChain::FT_Object);
  }

  C.addCommand(std::make_unique<Command>(
      JA, *this, ResponseFileSupport::AtFileCurCP(),
      TCArgs.MakeArgString(Linker), CmdArgs, Inputs, Output));
}

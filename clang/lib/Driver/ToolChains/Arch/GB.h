#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_GB_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_GB_H

#include "clang/Driver/Driver.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Option/Option.h"
#include <string>
#include <vector>

namespace clang::driver::tools::gb {

std::string getGBTargetCPU(const llvm::opt::ArgList &Args);

void getGBTargetFeatures(const Driver &D, const llvm::Triple &Triple,
                         const llvm::opt::ArgList &Args,
                         std::vector<llvm::StringRef> &Features);

} // namespace clang::driver::tools::gb

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_GB_H

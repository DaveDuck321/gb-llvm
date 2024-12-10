#include "GB.h"
#include "clang/Driver/Driver.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang;
using namespace llvm::opt;

std::string gb::getGBTargetCPU(const ArgList &Args) { return ""; }

void gb::getGBTargetFeatures(const Driver &D, const llvm::Triple &Triple,
                             const ArgList &Args,
                             std::vector<StringRef> &Features) {}

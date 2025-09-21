#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_GB_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_GB_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/Support/Compiler.h"
#include "llvm/TargetParser/Triple.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY GBTargetInfo : public TargetInfo {
  static const char *const GCCRegNames[];

public:
  GBTargetInfo(const llvm::Triple &Triple, const TargetOptions &);

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  llvm::SmallVector<Builtin::InfosShard> getTargetBuiltins() const override;

  bool allowsLargerPreferedTypeAlignment() const override;

  bool hasFeature(StringRef Feature) const override;

  ArrayRef<const char *> getGCCRegNames() const override;

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override;

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &info) const override;

  std::string_view getClobbers() const override;

  BuiltinVaListKind getBuiltinVaListKind() const override;
  CallingConvCheckResult checkCallingConvention(CallingConv) const override;
};

} // namespace targets
} // namespace clang
#endif // LLVM_CLANG_LIB_BASIC_TARGETS_GB_H

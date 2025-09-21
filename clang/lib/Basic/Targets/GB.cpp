#include "GB.h"
#include "clang/Basic/MacroBuilder.h"
#include "clang/Basic/TargetInfo.h"

using namespace clang;
using namespace clang::targets;

const char *const GBTargetInfo::GCCRegNames[] = {"pc", "sp", "af",
                                                 "bc", "de", "dl"};

GBTargetInfo::GBTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
    : TargetInfo(Triple) {
  TLSSupported = false;
  IntWidth = 16;
  IntAlign = 16;
  PointerWidth = PointerAlign = 16;
  LongWidth = 32;
  LongAlign = 16;
  LongLongWidth = 64;
  LongLongAlign = 16;
  SuitableAlign = 16;

  DefaultAlignForAttributeAligned = 16;
  HalfWidth = HalfAlign = 16;

  LongDoubleWidth = DoubleWidth = FloatWidth = 32;
  LongDoubleAlign = DoubleAlign = FloatAlign = 16;
  DoubleFormat = &llvm::APFloat::IEEEsingle();
  LongDoubleFormat = &llvm::APFloat::IEEEsingle();

  SizeType = UnsignedInt;
  PtrDiffType = SignedInt;
  IntPtrType = SignedInt;
  Char16Type = UnsignedInt;
  WIntType = SignedInt;
  Int16Type = SignedInt;
  Char32Type = UnsignedLong;
  SigAtomicType = SignedChar;
  resetDataLayout(
      "e-S16-p:16:16-i8:8-i16:16-i32:16-i64:16-f32:16-f64:16-a:0-m:e-n8:16");
}

void GBTargetInfo::getTargetDefines(const LangOptions &Opts,
                                    MacroBuilder &Builder) const {
  Builder.defineMacro("__GB__");
}

llvm::SmallVector<Builtin::InfosShard> GBTargetInfo::getTargetBuiltins() const {
  return {};
}

bool GBTargetInfo::allowsLargerPreferedTypeAlignment() const { return false; }

bool GBTargetInfo::hasFeature(StringRef Feature) const {
  return Feature == "GB";
}

ArrayRef<const char *> GBTargetInfo::getGCCRegNames() const {
  return {GCCRegNames};
}

ArrayRef<TargetInfo::GCCRegAlias> GBTargetInfo::getGCCRegAliases() const {
  static const TargetInfo::GCCRegAlias GCCRegAliases[] = {
      {{}, "pc"}, {{}, "sp"}, {{}, "af"}, {{}, "bc"}, {{}, "de"}, {{}, "hl"}};
  return llvm::ArrayRef(GCCRegAliases);
}

bool GBTargetInfo::validateAsmConstraint(
    const char *&Name, TargetInfo::ConstraintInfo &info) const {
  return false;
}

std::string_view GBTargetInfo::getClobbers() const { return "~{flags}"; }

TargetInfo::BuiltinVaListKind GBTargetInfo::getBuiltinVaListKind() const {
  return TargetInfo::VoidPtrBuiltinVaList;
}

TargetInfo::CallingConvCheckResult GBTargetInfo::checkCallingConvention(CallingConv CC) const {
    switch (CC) {
      default:
        return CCCR_Warning;
      case CC_C:
      case CC_GB_Interrupt:
        return CCCR_OK;
    }
}

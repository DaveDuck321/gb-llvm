#include "ABIInfoImpl.h"
#include "TargetInfo.h"
#include "clang/AST/Type.h"
#include "clang/CodeGen/CGFunctionInfo.h"
#include "llvm/Support/Debug.h"

#include <memory>

using namespace clang;
using namespace clang::CodeGen;

namespace {
class GBABIInfo : public DefaultABIInfo {
public:
  explicit GBABIInfo(CodeGenTypes &CGT) : DefaultABIInfo(CGT) {}

  ABIArgInfo classifyGBArgumentType(QualType Ty) const {
    Ty = useFirstFieldIfTransparentUnion(Ty);

    uint64_t SizeBits = getContext().getTypeSize(Ty);

    // Ignore empty types (even though this will almost never happen in c++)
    if (isEmptyRecord(getContext(), Ty, true) && SizeBits == 0) {
      return ABIArgInfo::getIgnore();
    }

    if (isAggregateTypeForABI(Ty)) {
      // Structures with either a non-trivial destructor or a non-trivial
      // copy constructor are always passed indirectly.
      if (CGCXXABI::RecordArgABI RAA = getRecordArgABI(Ty, getCXXABI())) {
        return getNaturalAlignIndirect(
            Ty, /*AddrSpace=*/getDataLayout().getAllocaAddrSpace(),
            /*ByVal=*/RAA == CGCXXABI::RAA_DirectInMemory);
      }

      // Expand structs
      const auto *RT = Ty->getAs<RecordType>();
      RecordDecl *RD = RT->getDecl();
      bool ShouldExpand = true;
      if (SizeBits > 32 || !Ty->isConstantSizeType()) {
        ShouldExpand = false;
      }

      for (auto *Field : RD->fields()) {
        size_t FieldSizeBits = getContext().getTypeSize(Field->getType());
        if (FieldSizeBits > 16 || Field->isBitField()) {
          ShouldExpand = false;
          break;
        }
      }

      if (ShouldExpand) {
        return ABIArgInfo::getExpand();
      }
    }

    return DefaultABIInfo::classifyArgumentType(Ty);
  }

  void computeInfo(CGFunctionInfo &FI) const override {
    if (!getCXXABI().classifyReturnType(FI)) {
      FI.getReturnInfo() = classifyReturnType(FI.getReturnType());
    }

    assert(FI.getNumRequiredArgs() == FI.arg_size() &&
           "Variadic arguments currently not supported");

    for (auto &I : FI.arguments()) {
      I.info = classifyGBArgumentType(I.type);
    }
  }
};

class GBTargetCodeGenInfo : public TargetCodeGenInfo {
public:
  explicit GBTargetCodeGenInfo(CodeGenTypes &CGT)
      : TargetCodeGenInfo(std::make_unique<GBABIInfo>(CGT)) {}
};
} // namespace

std::unique_ptr<TargetCodeGenInfo>
CodeGen::createGBTargetCodeGenInfo(CodeGenModule &CGM) {
  return std::make_unique<GBTargetCodeGenInfo>(CGM.getTypes());
}

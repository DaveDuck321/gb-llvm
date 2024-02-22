#include "GBTargetMachine.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/CodeGen.h"

using namespace llvm;

namespace {

class GBDAGToDAGISel final : public SelectionDAGISel {
public:
  static char ID;

  GBDAGToDAGISel(GBTargetMachine &TargetMachine, CodeGenOptLevel OptLevel);

  StringRef getPassName() const override;
  void Select(SDNode *) override;

#define GET_DAGISEL_DECL
#include "GBGenDAGISel.inc"
};

} // namespace

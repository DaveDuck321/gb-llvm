#include "GBTargetMachine.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/CodeGen.h"
#include <vector>

using namespace llvm;

namespace {

class GBDAGToDAGISel final : public SelectionDAGISel {
public:
  GBDAGToDAGISel(GBTargetMachine &TargetMachine, CodeGenOptLevel OptLevel);

  void Select(SDNode *) override;

  void PreprocessISelDAG() override;

private:
  bool parallelizei16IncDec(std::vector<SDNode *> AllNodes);
  bool serializei16IncDec(std::vector<SDNode *> AllNodes, bool DoIncrements);

#define GET_DAGISEL_DECL
#include "GBGenDAGISel.inc"
};

class GBDAGToDAGISelLegacy : public SelectionDAGISelLegacy {
public:
  static char ID;
  explicit GBDAGToDAGISelLegacy(GBTargetMachine &TargetMachine,
                                CodeGenOptLevel OptLevel);
};
} // namespace

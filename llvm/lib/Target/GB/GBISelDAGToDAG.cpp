#include "GBISelDAGToDAG.h"

#include "GB.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#define DEBUG_TYPE "gb-isel"

#define GET_DAGISEL_BODY GBDAGToDAGISel
#include "GBGenDAGISel.inc"

GBDAGToDAGISel::GBDAGToDAGISel(GBTargetMachine &TargetMachine,
                               CodeGenOptLevel OptLevel)
    : SelectionDAGISel(ID, TargetMachine) {}

StringRef GBDAGToDAGISel::getPassName() const {
  return "GB DAG->DAG Pattern Instruction Selection";
}

void GBDAGToDAGISel::Select(SDNode *Node) {
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(dbgs() << "already selected, "; Node->dump(CurDAG);
               dbgs() << "\n");
    Node->setNodeId(-1);
  }

  switch (Node->getOpcode()) {
  default:
    // Tablegen is VERY smart here, it generates our accumulator copies
    // automatically (and kinda invisibly).

    // 1) A pattern match against a physical register generates a copy to the
    //  physical register + chain + glue to instruction.
    // 2) Tablegen calls DAG->getMachineNode(..., ..., VTs, Ops)
    //    - VTs lists the results eg. [result1, result2, chain, glue]
    //      - In a pattern match, VTs is determined by the matched DAG node
    //    - Ops is the inputs eg. [input1, chain, glue]
    // 3) The DAG is optimized... Note. MachineNode's result is still a virtual
    //  register as far as the DAG is concerned.
    // 4) In InstructionEmitter, llvm checks if the MachineNode result count
    //  from VTs is more than the count of explicitly specified outputs. (eg.
    //  (outs ...) in tablegen).
    //   - If it is, this instruction must use the results from one of the
    //  implicit defs. In this case, a COPY machine instruction is generated,
    //  moving this physical register into the virtual register result of the
    //  instruction.

    SelectCode(Node);
  }
}

char GBDAGToDAGISel::ID = 0;

FunctionPass *llvm::createGBISelDag(GBTargetMachine &TM,
                                    CodeGenOptLevel OptLevel) {
  return new GBDAGToDAGISel(TM, OptLevel);
}

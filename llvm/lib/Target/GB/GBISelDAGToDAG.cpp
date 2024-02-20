#include "GBISelDAGToDAG.h"
#include "GB.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineValueType.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/ValueTypes.h"
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

void GBDAGToDAGISel::selectAccumulatorNode(SDNode *Node, unsigned TargetOp,
                                           MVT Width) {
  SDLoc Dl = Node;
  SDValue In0 = Node->getOperand(0);
  SDValue In1 = Node->getOperand(1);

  assert(Width == MVT::i8 or Width == MVT::i16);
  assert(In0.getSimpleValueType() == Width);
  assert(In1.getSimpleValueType() == Width);

  unsigned PhyAccReg = (Width == MVT::i8) ? GB::A : GB::HL;

  SDValue Chain = CurDAG->getEntryNode();
  SDValue Glue;

  // Copy input to A
  Chain = CurDAG->getCopyToReg(Chain, Dl, PhyAccReg, In0, Glue);
  Glue = Chain.getValue(1);
  Chain->setNodeId(-1); // Select

  MachineSDNode *CNode;
  {
    // Output = chain and glue (no value)
    SmallVector<EVT, 4> VTs;
    VTs.push_back(MVT::Other);
    VTs.push_back(MVT::Glue);
    SDVTList VTList = CurDAG->getVTList(VTs);

    // Input = chain, glue, and immediate
    SmallVector<SDValue, 4> Ops;
    Ops.push_back(In1);
    Ops.push_back(Chain);
    Ops.push_back(Glue);

    CNode = CurDAG->getMachineNode(TargetOp, Dl, VTList, Ops);
    Chain = SDValue(CNode, 0);
    Glue = SDValue(CNode, 1);
  }

  // Copy output from A
  SDValue Result = CurDAG->getCopyFromReg(Chain, Dl, PhyAccReg, Width, Glue);
  Result->setNodeId(-1); // Select

  // Done!
  LLVM_DEBUG(dbgs() << "matched accumulator node, "; Node->dump(CurDAG);
             dbgs() << "\n");
  ReplaceUses(SDValue(Node, 0), Result);
  CurDAG->RemoveDeadNode(Node);
}

void GBDAGToDAGISel::selectBinaryNode(SDNode *Node, unsigned Op8i,
                                      unsigned Op8r, unsigned Op16) {
  SDValue In0 = Node->getOperand(0);
  SDValue In1 = Node->getOperand(1);

  // TODO GB: support arbitrary sizes?
  // TODO GB: support indirect access

  // 8-bit: eg. add_r and addi
  if (In0.getSimpleValueType() == MVT::i8 &&
      In1.getSimpleValueType() == MVT::i8) {
    if (In1.getOpcode() == ISD::Constant) {
      return selectAccumulatorNode(Node, Op8i, MVT::i8);
    }
    return selectAccumulatorNode(Node, Op8r, MVT::i8);
  }

  // 16-bit: eg. add hl
  if (Op16 != 0 && In0.getSimpleValueType() == MVT::i16 &&
      In1.getSimpleValueType() == MVT::i16) {
    if (In1->getOpcode() == ISD::Constant) {
      return; // TODO GB: support this
    }
    return selectAccumulatorNode(Node, Op16, MVT::i16);
  }

  llvm_unreachable("Inputs to binary node are not of the correct type");
}

void GBDAGToDAGISel::Select(SDNode *Node) {
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(dbgs() << "already selected, "; Node->dump(CurDAG);
               dbgs() << "\n");
    Node->setNodeId(-1);
  }

  // Trying to achieve the same thing with tablegen gives the following error:
  // "Cannot handle instructions producing instructions with temporaries yet!"
  switch (Node->getOpcode()) {
  case ISD::Constant: {
    // TODO GB: typecheck?
    const ConstantInt &Val = *cast<ConstantSDNode>(Node)->getConstantIntValue();
    SDValue Result =
        CurDAG->getTargetConstant(Val, SDLoc(Node), Node->getValueType(0));

    // Substitute in the new node
    ReplaceUses(SDValue(Node, 0), Result);
    CurDAG->RemoveDeadNode(Node);
    break;
  }
  case ISD::ADD:
    selectBinaryNode(Node, GB::ADDI, GB::ADD_r, GB::ADD_HL);
    break;
  case ISD::SUB:
    selectBinaryNode(Node, GB::SUBI, GB::SUB_r);
    break;
  case ISD::AND:
    selectBinaryNode(Node, GB::ANDI, GB::AND_r);
    break;
  case ISD::XOR:
    selectBinaryNode(Node, GB::XORI, GB::XOR_r);
    break;
  case ISD::OR:
    selectBinaryNode(Node, GB::ORI, GB::OR_r);
    break;
  default:
    // Fallback to TableGen
    SelectCode(Node);
  }
}

char GBDAGToDAGISel::ID = 0;

FunctionPass *llvm::createGBISelDag(GBTargetMachine &TM,
                                    CodeGenOptLevel OptLevel) {
  return new GBDAGToDAGISel(TM, OptLevel);
}

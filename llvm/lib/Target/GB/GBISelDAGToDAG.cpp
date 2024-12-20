#include "GBISelDAGToDAG.h"

#include "GB.h"
#include "GBISelLowering.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/CodeGen/FunctionLoweringInfo.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#include <cstddef>
#include <optional>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#define DEBUG_TYPE "gb-isel"

#define GET_DAGISEL_BODY GBDAGToDAGISel
#include "GBGenDAGISel.inc"

static cl::opt<bool> GBDisablePreprocessi16Serialize(
    "gb-disable-i16-serialize", cl::Hidden,
    cl::desc("Disables PreprocessISelDAG, serializei16Increments"));

namespace {

std::optional<std::tuple<SDValue, SDValue, long>>
identify16BitConstantAddSub(SDValue LSB, SDValue MSB) {
  if (LSB.getOpcode() == ISD::ADDC && MSB->getOpcode() == ISD::ADDE &&
      MSB->getGluedNode() == LSB.getNode()) {
    auto LSBConstant = LSB->getOperand(1);
    auto MSBConstant = MSB->getOperand(1);

    auto LSBValue = LSB->getOperand(0);
    auto MSBValue = MSB->getOperand(0);

    if (LSBConstant->getOpcode() == ISD::Constant &&
        MSBConstant->getOpcode() == ISD::Constant) {
      // Addition
      if (MSBConstant->getAsZExtVal() == 0) {
        size_t Offset = LSBConstant->getAsZExtVal();
        assert(Offset > 0);
        return std::make_tuple(LSBValue, MSBValue, Offset);
      }

      // Subtraction
      if (MSBConstant->getAsZExtVal() == 0xff) {
        long Offset = (signed char)MSBConstant->getAsZExtVal();
        assert(Offset < 0);
        return std::make_tuple(LSBValue, MSBValue, Offset);
      }
    }
  }
  return std::nullopt;
}

bool is16BitConstantAddition(SDValue ResultLSB, SDValue ResultMSB) {
  return identify16BitConstantAddSub(ResultLSB, ResultMSB).has_value();
}

std::tuple<SDValue, SDValue, long>
decompose16BitConstantAddition(SDValue ResultLSB, SDValue ResultMSB) {
  assert(is16BitConstantAddition(ResultLSB, ResultMSB));
  return *identify16BitConstantAddSub(ResultLSB, ResultMSB);
}

} // namespace

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

void GBDAGToDAGISel::PreprocessISelDAG() {
  auto GetCurrentNodes = [this]() {
    std::vector<SDNode *> T;
    T.reserve(CurDAG->allnodes_size());
    for (SDNode &N : CurDAG->allnodes())
      T.push_back(&N);
    return T;
  };

  if (OptLevel == CodeGenOptLevel::None) {
    return;
  }

  // Identify the parallel pattern:
  //    <op> <value>, <(addr)>
  //    <op> <value>, <(addr+1)>
  //    <op> <value>, <(addr+2)>
  // Convert to:
  //    <op> <value>, <(addr)>
  //    addr' = addr + 1
  //    <op> <value>, <(addr')>
  //    addr'' = addr' + 1
  //    <op> <value>, <(addr'')>
  //    addr''' = addr'' + 1
  //    <op> <value>, <(addr''')>
  // Reasoning:
  //    - Register pressure can be reduced with the LDI pattern
  //    - i16 increment is MUCH faster than addition for all GPRs
  if (!GBDisablePreprocessi16Serialize) {
    // LLVM gives us an inconsistent mix of parallel and serial addition chains.
    // Try to recognize and parallelize additions first to make the
    // serialization pattern match simpler.
    while (parallelizei16IncDec(GetCurrentNodes())) {
    }
    serializei16IncDec(GetCurrentNodes(), true);
    serializei16IncDec(GetCurrentNodes(), false);
  }
}

bool GBDAGToDAGISel::parallelizei16IncDec(std::vector<SDNode *> AllNodes) {
  bool MadeChanges = false;

  for (auto *Node : AllNodes) {
    if (Node->getValueType(0) != MVT::i16) {
      continue;
    }
    SDLoc NodeLoc = Node;

    if (Node->getOpcode() == GBISD::COMBINE) {
      SDValue CombineResult = SDValue{Node, 0};
      SDValue ResultLSB = Node->getOperand(0);
      SDValue ResultMSB = Node->getOperand(1);
      if (is16BitConstantAddition(ResultLSB, ResultMSB)) {
        auto [OurInputLSB, OurInputMSB, OurOffset] =
            decompose16BitConstantAddition(ResultLSB, ResultMSB);

        // We identify chained additions of constant
        // This is primarily generated when legalizing large stores
        if (is16BitConstantAddition(OurInputLSB, OurInputMSB)) {
          auto [ParentInputLSB, ParentInputMSB, ParentOffset] =
              decompose16BitConstantAddition(OurInputLSB, OurInputMSB);
          auto CombinedOffset = OurOffset + ParentOffset;
          if (CombinedOffset < -128 or CombinedOffset > 127) {
            continue;
          }

          SDValue ResLSB = CurDAG->getNode(
              ISD::ADDC, NodeLoc, CurDAG->getVTList(MVT::i8, MVT::Glue),
              ParentInputLSB,
              CurDAG->getConstant(CombinedOffset, NodeLoc, MVT::i8));

          SDValue ResLSBGlue = SDValue(ResLSB.getNode(), 1);

          SDValue ResMSB = CurDAG->getNode(
              ISD::ADDE, NodeLoc, CurDAG->getVTList(MVT::i8, MVT::Glue),
              ParentInputMSB,
              CurDAG->getConstant(CombinedOffset < 0 ? 0xFF : 0, NodeLoc,
                                  MVT::i8),
              ResLSBGlue);

          SDValue NewCombined = CurDAG->getNode(GBISD::COMBINE, NodeLoc,
                                                MVT::i16, ResLSB, ResMSB);

          CurDAG->ReplaceAllUsesOfValueWith(CombineResult, NewCombined);
          MadeChanges = true;
        }
      }
    }
  }

  if (MadeChanges) {
    CurDAG->RemoveDeadNodes();
  }
  return MadeChanges;
}

bool GBDAGToDAGISel::serializei16IncDec(std::vector<SDNode *> AllNodes,
                                        bool DoIncrements) {
  using ValueOrPair = std::variant<SDValue, std::pair<SDValue, SDValue>>;
  std::map<ValueOrPair, std::map<size_t, std::vector<SDValue>>>
      PositiveOffsetsFromValue;
  std::map<ValueOrPair, std::map<size_t, std::vector<SDValue>>>
      NegativeOffsetsFromValue;
  for (auto *Node : AllNodes) {
    if (Node->getValueType(0) != MVT::i16) {
      continue;
    }

    const SDValue Input = SDValue{Node, 0};

    long Offset = 0;
    ValueOrPair Base = Input;
    if (Node->getOpcode() == GBISD::COMBINE) {
      SDValue ResultLSB = Node->getOperand(0);
      SDValue ResultMSB = Node->getOperand(1);
      if (is16BitConstantAddition(ResultLSB, ResultMSB)) {
        auto [LSB, MSB, Constant] =
            decompose16BitConstantAddition(ResultLSB, ResultMSB);

        // Identify 16-bit additions that come directly from a 16-bit register
        if (LSB->getOpcode() == GBISD::LOWER &&
            MSB->getOpcode() == GBISD::UPPER &&
            MSB->getOperand(0) == LSB->getOperand(0)) {
          Offset = Constant;
          Base = LSB->getOperand(0);
        } else {
          Offset = Constant;
          Base = std::make_pair(LSB, MSB);
        }
      } else {
        auto Pair = std::make_pair(ResultLSB, ResultMSB);
        PositiveOffsetsFromValue[Pair][0].push_back(Input);
        NegativeOffsetsFromValue[Pair][0].push_back(Input);
      }
    }
    if (Offset >= 0) {
      PositiveOffsetsFromValue[Base][Offset].push_back(Input);
    }

    if (Offset <= 0) {
      NegativeOffsetsFromValue[Base][-Offset].push_back(Input);
    }
  }

  // Make any valid substitutions
  bool MadeChanges = false;

  auto ProduceIncDecChain =
      [&](unsigned Op,
          const std::map<ValueOrPair, std::map<size_t, std::vector<SDValue>>>
              &OffsetsFromValue) {
        for (const auto &[VBase, Offsets] : OffsetsFromValue) {
          if (Offsets.size() == 1) {
            // This node is not used in any additional i16 additions
            continue;
          }

          SDValue Base;
          if (std::holds_alternative<SDValue>(VBase)) {
            Base = std::get<SDValue>(VBase);
          } else {
            auto [LHS, RHS] = std::get<std::pair<SDValue, SDValue>>(VBase);
            Base =
                CurDAG->getNode(GBISD::COMBINE, SDLoc{LHS}, MVT::i16, LHS, RHS);
          }

          size_t LastOffset = 0;
          SDValue ValueToIncrement = Base;
          for (const auto &[Offset, ValuesToReplace] : Offsets) {
            if (Offset == 0) {
              continue;
            }

            if (Offset - LastOffset > 3) {
              // Large gap...
              // Its likely faster to just codegen the addition
              break;
            }

            do {
              assert(ValueToIncrement.getSimpleValueType() == MVT::i16);
              ValueToIncrement = CurDAG->getNode(Op, ValuesToReplace.at(0),
                                                 MVT::i16, ValueToIncrement);
            } while (Offset != ++LastOffset);

            // Replace any additions with the chained increment
            for (const auto &Value : ValuesToReplace) {
              if (Value->getOpcode() == GBISD::COMBINE) {
                // Also replace values using the intermediate results
                auto NewLower = CurDAG->getNode(GBISD::LOWER, Value, MVT::i8,
                                                ValueToIncrement);
                auto NewUpper = CurDAG->getNode(GBISD::UPPER, Value, MVT::i8,
                                                ValueToIncrement);
                CurDAG->ReplaceAllUsesOfValueWith(Value->getOperand(0),
                                                  NewLower);
                CurDAG->ReplaceAllUsesOfValueWith(Value->getOperand(1),
                                                  NewUpper);
              }
              CurDAG->ReplaceAllUsesOfValueWith(Value, ValueToIncrement);
              MadeChanges = true;
            }
          }
        }
      };

  if (DoIncrements) {
    ProduceIncDecChain(GBISD::INC16, PositiveOffsetsFromValue);
  } else {
    ProduceIncDecChain(GBISD::DEC16, NegativeOffsetsFromValue);
  }

  if (MadeChanges) {
    CurDAG->RemoveDeadNodes();
  }
  return MadeChanges;
}

char GBDAGToDAGISel::ID = 0;

FunctionPass *llvm::createGBISelDag(GBTargetMachine &TM,
                                    CodeGenOptLevel OptLevel) {
  return new GBDAGToDAGISel(TM, OptLevel);
}

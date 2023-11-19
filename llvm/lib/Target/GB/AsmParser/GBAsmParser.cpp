#include "MCTargetDesc/GBMCTargetDesc.h"
#include "TargetInfo/GBTargetInfo.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCAsmMacro.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegister.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <variant>

using namespace llvm;

namespace {

class GBOperand : public MCParsedAsmOperand {
  struct RegOp {
    unsigned RegNum;
  };
  struct ImmOp {
    const MCExpr *Val;
  };
  struct TokOp {
    StringRef Val;
  };

  class Printer {
    raw_ostream &OS;

  public:
    Printer(raw_ostream &OS) : OS(OS) {}
    void operator()(const RegOp &Op) { OS << "<register " << Op.RegNum << ">"; }
    void operator()(const ImmOp &Op) { OS << Op.Val; }
    void operator()(const TokOp &Tok) { OS << "'" << Tok.Val << "'"; }
  };

  SMLoc StartLoc, EndLoc;
  std::variant<RegOp, ImmOp, TokOp> Data;

public:
  bool isToken() const override { return std::holds_alternative<TokOp>(Data); };
  bool isImm() const override { return std::holds_alternative<ImmOp>(Data); };
  bool isReg() const override { return std::holds_alternative<RegOp>(Data); };
  unsigned getReg() const override { return std::get<RegOp>(Data).RegNum; };
  bool isMem() const override { return false; };

  SMLoc getStartLoc() const override { return StartLoc; };
  SMLoc getEndLoc() const override { return EndLoc; };

  void print(raw_ostream &OS) const override { std::visit(Printer{OS}, Data); };

  // Used by tablegen
  StringRef getToken() const { return std::get<TokOp>(Data).Val; };

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Unsupported operand count");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Unsupported operand count");

    const auto *Expr = std::get<ImmOp>(Data).Val;
    if (const auto *CE = dyn_cast<MCConstantExpr>(Expr); CE != nullptr) {
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    } else {
      Inst.addOperand(MCOperand::createExpr(Expr));
    }
  }

  static auto createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = TokOp{Str};
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static auto createReg(unsigned RegNo, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = RegOp{RegNo};
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static auto createImm(const MCExpr *Val, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = ImmOp{Val};
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }
};

class GBAsmParser : public MCTargetAsmParser {
public:
  GBAsmParser(const MCSubtargetInfo &STI, MCAsmParser &P,
              const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII) {}

  enum GBMatchResultTy {
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "GBGenAsmMatcher.inc"
  };

  bool parseRegister(MCRegister &Reg, SMLoc &StartLoc, SMLoc &EndLoc) override;

  ParseStatus tryParseRegister(MCRegister &Reg, SMLoc &StartLoc,
                               SMLoc &EndLoc) override;

  ParseStatus tryParseImmediate(const MCExpr *&Expr, SMLoc &StartLoc,
                                SMLoc &EndLoc);

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

private:
  ParseStatus tryParseRegister(OperandVector &Operands);
  ParseStatus tryParseImmediate(OperandVector &Operands);
  ParseStatus tryParseOperand(OperandVector &Operands);

#define GET_ASSEMBLER_HEADER
#include "GBGenAsmMatcher.inc"
};

} // namespace

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "GBGenAsmMatcher.inc"

bool GBAsmParser::parseRegister(MCRegister &Reg, SMLoc &StartLoc,
                                SMLoc &EndLoc) {

  if (auto Status = tryParseRegister(Reg, StartLoc, EndLoc);
      Status.isSuccess()) {
    return false; // Success
  }
  return Error(StartLoc, "invalid register name");
}

ParseStatus GBAsmParser::tryParseRegister(MCRegister &Reg, SMLoc &StartLoc,
                                          SMLoc &EndLoc) {
  const auto &Token = getParser().getTok();
  StartLoc = Token.getLoc();
  EndLoc = Token.getEndLoc();
  Reg = MatchRegisterName(Token.getIdentifier().lower());

  if (Reg.isValid()) {
    getParser().Lex();
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseRegister(OperandVector &Operands) {
  switch (getLexer().getKind()) {
  default:
    return ParseStatus::NoMatch;
  case AsmToken::Identifier:
    break;
  }

  MCRegister Reg;
  SMLoc StartLoc, EndLoc;
  if (tryParseRegister(Reg, StartLoc, EndLoc).isSuccess()) {
    Operands.push_back(GBOperand::createReg(Reg, StartLoc, EndLoc));
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseImmediate(const MCExpr *&Expr, SMLoc &StartLoc,
                                           SMLoc &EndLoc) {
  StartLoc = getLexer().getLoc();
  if (getParser().parseExpression(Expr, EndLoc)) {
    return ParseStatus::NoMatch;
  }
  return ParseStatus::Success;
}

ParseStatus GBAsmParser::tryParseImmediate(OperandVector &Operands) {
  switch (getLexer().getKind()) {
  default:
    return ParseStatus::NoMatch;
  case AsmToken::LParen:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::String:
    break;
  }

  const MCExpr *Expr;
  SMLoc StartLoc, EndLoc;
  if (tryParseImmediate(Expr, StartLoc, EndLoc).isSuccess()) {
    Operands.push_back(GBOperand::createImm(Expr, StartLoc, EndLoc));
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseOperand(OperandVector &Operands) {
  if (tryParseRegister(Operands).isSuccess()) {
    return ParseStatus::Success;
  }
  if (tryParseImmediate(Operands).isSuccess()) {
    return ParseStatus::Success;
  }

  return ParseStatus::Failure;
}

bool GBAsmParser::ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                                   SMLoc NameLoc, OperandVector &Operands) {

  auto &Lexer = getLexer();
  Operands.push_back(GBOperand::createToken(Name, NameLoc));

  int OperandNum = 0;
  while (Lexer.isNot(AsmToken::EndOfStatement)) {
    if (OperandNum++ > 0) {
      if (!Lexer.is(AsmToken::Comma)) {
        return Error(Lexer.getLoc(), "expected a comma");
      }
      Lexer.Lex();
    }

    if (!tryParseOperand(Operands).isSuccess()) {
      SMLoc ErrLoc = Lexer.getLoc();
      getParser().eatToEndOfStatement();
      return Error(ErrLoc, "unknown operand");
    }
  }

  Lexer.Lex();  // Consume the EndOfStatement
  return false; // Success
}

bool GBAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                          OperandVector &Operands,
                                          MCStreamer &Out, uint64_t &ErrorInfo,
                                          bool MatchingInlineAsm) {
  MCInst Inst;
  switch (MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm)) {
  default:
    return Error(IDLoc, "Unknown matching error");
  case Match_MnemonicFail:
    return Error(IDLoc, "unrecognized mnemonic");
  case Match_InvalidOperand:
    return Error(IDLoc, "invalid operand");
  case Match_Success:
    Inst.setLoc(IDLoc);
    Out.emitInstruction(Inst, getSTI());
    return false; // Success
  }
}

extern "C" void LLVMInitializeGBAsmParser() {
  RegisterMCAsmParser<GBAsmParser> _{getTheGBTarget()};
}

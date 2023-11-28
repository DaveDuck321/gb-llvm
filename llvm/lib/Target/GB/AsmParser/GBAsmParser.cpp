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

#include <cmath>
#include <cstdint>
#include <memory>
#include <variant>

using namespace llvm;

namespace {

bool fitsInIntOfWidth(int64_t Value, uint8_t Width, bool IsSigned) {
  if (IsSigned) {
    return -(1 << (Width - 1)) <= Value && (1 << (Width - 1)) > Value;
  }
  return 0 <= Value && (1 << Width) > Value;
}

class GBOperand : public MCParsedAsmOperand {
  struct Reg {
    unsigned RegNum;
  };

  struct Imm {
    int64_t Val;
  };

  struct Token {
    StringRef Val;
  };

  class Printer {
    raw_ostream &OS;

  public:
    Printer(raw_ostream &OS) : OS(OS) {}
    void operator()(const Reg &Op) { OS << "<register " << Op.RegNum << ">"; }
    void operator()(const Imm &Op) { OS << Op.Val; }
    void operator()(const Token &Token) { OS << "'" << Token.Val << "'"; }
  };

  SMLoc StartLoc, EndLoc;
  std::variant<Reg, Imm, Token> Data;

public:
  bool isToken() const override { return std::holds_alternative<Token>(Data); };
  bool isImm() const override { return std::holds_alternative<Imm>(Data); };
  bool isImmN(uint8_t Width, bool IsSigned) const {
    return isImm() &&
           fitsInIntOfWidth(std::get<Imm>(Data).Val, Width, IsSigned);
  }
  bool isImm3() const { return isImmN(3, false); }
  bool isImm8() const { return isImmN(8, false); }
  bool isImm16() const { return isImmN(16, false); }
  bool isAddr16() const { return isImmN(16, false); }

  bool isReg() const override { return std::holds_alternative<Reg>(Data); };
  unsigned getReg() const override { return std::get<Reg>(Data).RegNum; };
  bool isMem() const override { return false; };

  SMLoc getStartLoc() const override { return StartLoc; };
  SMLoc getEndLoc() const override { return EndLoc; };

  void print(raw_ostream &OS) const override { std::visit(Printer{OS}, Data); };

  // Used by tablegen
  StringRef getToken() const { return std::get<Token>(Data).Val; };

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Unsupported operand count");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Unsupported operand count");
    Inst.addOperand(MCOperand::createImm(std::get<Imm>(Data).Val));
  }

  static auto createToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = Token{Str};
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static auto createReg(unsigned RegNo, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = Reg{RegNo};
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static auto createImm(int64_t Val, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = Imm{Val};
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

  ParseStatus tryParseImmediate(int64_t &Imm, SMLoc &StartLoc, SMLoc &EndLoc);

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

ParseStatus GBAsmParser::tryParseImmediate(int64_t &Val, SMLoc &StartLoc,
                                           SMLoc &EndLoc) {
  StartLoc = getLexer().getLoc();

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr, EndLoc)) {
    return ParseStatus::NoMatch;
  }

  // TODO GB: hmm, what are non-constant expressions here?
  if (Expr->getKind() != MCExpr::Constant) {
    return ParseStatus::NoMatch;
  }
  Val = dyn_cast<MCConstantExpr>(Expr)->getValue();
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

  SMLoc StartLoc, EndLoc;
  if (int64_t Imm; tryParseImmediate(Imm, StartLoc, EndLoc).isSuccess()) {
    Operands.push_back(GBOperand::createImm(Imm, StartLoc, EndLoc));
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

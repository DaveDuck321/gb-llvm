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
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdint>
#include <memory>
#include <optional>
#include <stdint.h>
#include <variant>

using namespace llvm;

namespace {

static std::optional<unsigned> getFlagEncoding(StringRef Flag) {
  if (Flag == "nz") {
    return 0u;
  }
  if (Flag == "z") {
    return 1u;
  }
  if (Flag == "nc") {
    return 2u;
  }
  if (Flag == "c") {
    return 3u;
  }
  return std::nullopt;
}

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
    const MCExpr *Expr;
  };

  struct Flag {
    StringRef Flag;
  };

  struct Token {
    StringRef Val;
  };

  class Printer {
    raw_ostream &OS;

  public:
    Printer(raw_ostream &OS) : OS(OS) {}
    void operator()(const Reg &Op) { OS << "<register " << Op.RegNum << ">"; }
    void operator()(const Imm &Op) { OS << *Op.Expr; }
    void operator()(const Flag &Flag) { OS << Flag.Flag; }
    void operator()(const Token &Token) { OS << "'" << Token.Val << "'"; }
  };

  SMLoc StartLoc, EndLoc;
  std::variant<Reg, Imm, Token, Flag> Data;

public:
  bool isToken() const override { return std::holds_alternative<Token>(Data); };
  bool isImm() const override {
    return std::holds_alternative<Imm>(Data) || isFlag();
  }

  bool isImmN(uint8_t Width, bool IsSigned, bool AllowExpr = true) const {
    if (not std::holds_alternative<Imm>(Data)) {
      return false;
    }

    int64_t Value;
    const MCExpr *Expr = std::get<Imm>(Data).Expr;
    const bool IsConstantImm = Expr->evaluateAsAbsolute(Value);
    if (not IsConstantImm) {
      // TODO GB: is this actually verified later?
      return AllowExpr;
    }
    return fitsInIntOfWidth(Value, Width, IsSigned);
  }

  bool isUImm3() const { return isImmN(3, false, false); }
  bool isUImm8() const { return isImmN(8, false); }
  bool isSImm8() const { return isImmN(8, true); }
  bool isUImm16() const { return isImmN(16, false); }
  bool isFlag() const { return std::holds_alternative<Flag>(Data); }

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
    assert(std::holds_alternative<Imm>(Data));

    int64_t Value;
    const MCExpr *Expr = std::get<Imm>(Data).Expr;
    const bool IsConstantImm = Expr->evaluateAsAbsolute(Value);
    if (IsConstantImm) {
      Inst.addOperand(MCOperand::createImm(Value));
    } else {
      Inst.addOperand(MCOperand::createExpr(Expr));
    }
  }

  void addFlagOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Unsupported operand count");
    const auto Encoding = getFlagEncoding(std::get<Flag>(Data).Flag.lower());
    if (not Encoding.has_value()) {
      llvm_unreachable("Unsupported flag operand");
    }
    Inst.addOperand(MCOperand::createImm(Encoding.value()));
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

  static auto createImm(const MCExpr *Expr, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = Imm{Expr};
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static auto createFlag(StringRef Name, SMLoc S, SMLoc E) {
    auto Op = std::make_unique<GBOperand>();
    Op->Data = Flag{Name};
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
  ParseStatus tryParseFlag(OperandVector &Operands);
  ParseStatus tryParseImmediate(OperandVector &Operands);
  ParseStatus tryParseOperand(OperandVector &Operands);
  ParseStatus tryParseRegister(OperandVector &Operands);
  ParseStatus tryParseToken(OperandVector &Operands);

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
  auto &Lexer = getLexer();
  const auto &Token = Lexer.getTok();
  StartLoc = Token.getLoc();
  EndLoc = Token.getEndLoc();
  Reg = MatchRegisterName(Token.getIdentifier().lower());

  if (Reg.isValid()) {
    Lexer.Lex();
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseOperand(OperandVector &Operands) {
  // Immediates + flags should have already been parsed by tablegen
  if (tryParseRegister(Operands).isSuccess()) {
    return ParseStatus::Success;
  }
  if (tryParseToken(Operands).isSuccess()) {
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseToken(OperandVector &Operands) {
  // Currently (hl) is the only token operand
  // NOTE: this eats tokens during the parse, it much be called after exhausting
  // all other operand types
  auto &Lexer = getLexer();
  const auto StartLoc = Lexer.getLoc();
  if (not Lexer.is(AsmToken::LParen)) {
    return ParseStatus::NoMatch;
  }
  Lexer.Lex();

  if (Lexer.getTok().getString().lower() != "hl") {
    return ParseStatus::NoMatch;
  }
  Lexer.Lex();

  if (not Lexer.is(AsmToken::RParen)) {
    return ParseStatus::NoMatch;
  }
  Lexer.Lex();

  Operands.push_back(GBOperand::createToken("(hl)", StartLoc));
  return ParseStatus::Success;
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
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Integer:
  case AsmToken::Dot:
    break;
  case AsmToken::Identifier: {
    // Only proceed to parse the immediate expression if it is definitively not
    // a register/ flag name.
    // TODO: can I make the assembler grammar non-ambiguous?
    const auto &Token = getLexer().getTok();
    const auto TokenStr = Token.getIdentifier().lower();
    MCRegister Reg = MatchRegisterName(TokenStr);
    if (Reg.isValid()) {
      return ParseStatus::NoMatch;
    }
    if (getFlagEncoding(TokenStr).has_value()) {
      return ParseStatus::NoMatch;
    }
  }
  }

  SMLoc StartLoc, EndLoc;
  if (const MCExpr * Expr;
      tryParseImmediate(Expr, StartLoc, EndLoc).isSuccess()) {
    Operands.push_back(GBOperand::createImm(Expr, StartLoc, EndLoc));
    return ParseStatus::Success;
  }
  return ParseStatus::NoMatch;
}

ParseStatus GBAsmParser::tryParseFlag(OperandVector &Operands) {
  auto &Lexer = getLexer();
  const auto StartLoc = Lexer.getLoc();

  if (not Lexer.is(AsmToken::Identifier)) {
    return ParseStatus::NoMatch;
  }

  const auto Flag = Lexer.getTok().getString();
  Lexer.Lex();

  Operands.push_back(GBOperand::createFlag(Flag, StartLoc, Lexer.getLoc()));
  return ParseStatus::Success;
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

    if (MatchOperandParserImpl(Operands, Name).isSuccess()) {
      continue;
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
  // TODO GB: actually position the cursor in the correct place after an error
  // TODO GB: actually give a proper error message
  MCInst Inst;
  const auto MatchResult =
      MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm);
  switch (MatchResult) {
  default:
    if (const char *Diag = getMatchKindDiag((GBMatchResultTy)MatchResult);
        Diag != nullptr) {
      return Error(IDLoc, Diag);
    }
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

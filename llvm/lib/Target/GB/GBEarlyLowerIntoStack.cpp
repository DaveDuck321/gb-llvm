#include "GB.h"
#include "GBRegisterInfo.h"
#include "GBTargetMachine.h"
#include "MCTargetDesc/GBMCTargetDesc.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LiveInterval.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/LaneBitmask.h"
#include "llvm/Pass.h"
#include "llvm/Support/Alignment.h"

using namespace llvm;

#define DEBUG_TYPE "gb-early-stack-lowering"

static cl::opt<bool>
    GBEnableEarlyLowingIntoStack("gb-enable-early-lowing-into-stack",
                                 cl::Hidden,
                                 cl::desc("Enables GBEarlyLowerIntoStack"));

static cl::opt<bool> GBDebugEarlyLowingEverything(
    "gb-debug-early-lower-everything", cl::Hidden,
    cl::desc(
        "GBEarlyLowerIntoStack will greedily lower every possible interval"));

static cl::opt<bool> GBDebugDontLowerRegions(
    "gb-debug-dont-lower-regions", cl::Hidden,
    cl::desc("GBEarlyLowerIntoStack will not lower BB-local regions."));

static cl::opt<bool> GBDebugDontLowerLiveThroughs(
    "gb-debug-dont-lower-live-throughs", cl::Hidden,
    cl::desc("GBEarlyLowerIntoStack will not lower live-throughs."));

namespace {
std::pair<bool, bool> readsWritesVirtualRegister(const TargetRegisterInfo &TRI,
                                                 MachineInstr &MI, Register Reg,
                                                 LaneBitmask Lane) {
  bool Def = false;
  bool Use = false;

  for (unsigned I = 0; I != MI.getNumOperands(); I += 1) {
    const MachineOperand &MO = MI.getOperand(I);
    if (!MO.isReg() || MO.getReg() != Reg) {
      continue;
    }

    if (MO.isUse()) {
      if (MO.getSubReg() == 0 ||
          (TRI.getSubRegIndexLaneMask(MO.getSubReg()) & Lane).any()) {
        Use |= !MO.isUndef();
      }
      continue;
    }

    if (MO.getSubReg()) {
      if ((TRI.getSubRegIndexLaneMask(MO.getSubReg()) & Lane).any()) {
        Def = true;
      }
      continue;
    }

    Def = true;
  }
  // A partial redefine uses Reg unless there is also a full define.
  return std::make_pair(Use, Def);
}

struct ReloadableRegionInfo {
  // Within a MBB, if on the stack, a store must be inserted after Start, a load
  // must be inserted before End.
  SlotIndex Start;
  SlotIndex End;
  LiveInterval *Interval;
  LaneBitmask SubRegLaneMask;

  void print(raw_ostream &OS) const {
    OS << "Reloadable(%" << Interval->reg().virtRegIndex() << ", ";
    PrintLaneMask(SubRegLaneMask).Print(OS);
    OS << ") : [" << Start << "," << End << "]\n";
  }
};

struct LiveThroughInfo {
  struct BBInfo {
    // If Start is valid, a load must be inserted immediately before it
    // If End is valid, a store must be inserted immediately after it
    SlotIndex Start;
    SlotIndex End;

    MachineBasicBlock *MBB;

    void print(raw_ostream &OS) const {
      OS << "[";
      if (Start.isValid()) {
        OS << Start;
      }
      OS << ",";
      if (End.isValid()) {
        OS << End;
      }
      OS << ":@" << MBB->getNumber() << "]";
    }
  };
  llvm::SmallVector<BBInfo, 4> BlocksInfo;
  LiveInterval *Interval;
  LaneBitmask SubRegLaneMask;

  void print(raw_ostream &OS) const {
    OS << "LiveThrough(%" << Interval->reg().virtRegIndex() << ", ";
    PrintLaneMask(SubRegLaneMask).Print(OS);
    OS << ") : ";
    for (auto const &Block : BlocksInfo) {
      Block.print(OS);
      if (&Block != &BlocksInfo.back()) {
        OS << ", ";
      }
    }
    OS << "\n";
  }
};

class GBEarlyLowerIntoStack : public MachineFunctionPass {
  static char ID;
  MachineFunction *MF;
  GBTargetMachine *TM;
  LiveIntervals *LIS;
  MachineDominatorTree *DT;
  MachinePostDominatorTree *PDT;
  MachineRegisterInfo *MRI;
  TargetRegisterInfo const *TRI;
  MachineFrameInfo *MFI;

  std::vector<ReloadableRegionInfo> ReloadableRegions;
  std::vector<LiveThroughInfo> LiveThroughs;
  std::vector<LiveInterval *> VirtualIntervals;
  std::vector<LiveRange *> PhysicalRanges;

  std::vector<LiveRange *> HLRegRanges;
  std::vector<LiveRange *> ARegRanges;
  std::vector<LiveRange *> FRegRanges;

  std::vector<ReloadableRegionInfo *> RemainingReloadableRegions;
  std::vector<LiveThroughInfo *> RemainingLiveThroughs;

  std::vector<ReloadableRegionInfo *> SelectedReloadableRegions;
  std::vector<LiveThroughInfo *> SelectedLiveThroughs;

public:
  GBEarlyLowerIntoStack(GBTargetMachine &TM)
      : MachineFunctionPass(ID), TM{&TM} {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<LiveIntervalsWrapperPass>();
    AU.addRequired<MachineBlockFrequencyInfoWrapperPass>();
    AU.addRequired<MachineDominatorTreeWrapperPass>();
    AU.addRequired<MachinePostDominatorTreeWrapperPass>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override { return "GBEarlyLowerIntoStack"; }

  void redistributeLiveIntervals();
  bool isInLiveRanges(std::vector<LiveRange *> const &,
                      ArrayRef<SlotIndex>) const;

  std::pair<unsigned, Align> getSpillSizeAlign(Register, LaneBitmask) const;

  // Returns (SubRegIndex, is8Bit)
  std::pair<unsigned, bool> subregInfo(Register Reg, LaneBitmask Mask) const;
  void saveToSlot(MachineBasicBlock *, MachineBasicBlock::iterator MBBI,
                  DebugLoc, Register, LaneBitmask, unsigned FrameIndex);
  void loadFromSlot(MachineBasicBlock *, MachineBasicBlock::iterator MBBI,
                    DebugLoc, Register, LaneBitmask, unsigned FrameIndex);

  void clearState();
  void populateIntervals();
  void sortWorklistByPriority();
  bool maybeSelectIntervalToLower();

  bool applyChangesToRegions();
  bool applyChangesToLiveThroughs();
  bool applyChanges();
  bool runOnMachineFunction(MachineFunction &MF) override;
};

char GBEarlyLowerIntoStack::ID;
} // namespace

void GBEarlyLowerIntoStack::redistributeLiveIntervals() {
  SlotIndexes *SI = LIS->getSlotIndexes();
  SI->reanalyze(*MF);
  LIS->reanalyze(*MF);

  ConnectedVNInfoEqClasses EQC(*LIS);
  for (unsigned I = 0, E = MRI->getNumVirtRegs(); I != E; ++I) {
    Register Reg = Register::index2VirtReg(I);

    LiveInterval &LI = LIS->getInterval(Reg);
    unsigned NumComp = EQC.Classify(LI);
    if (NumComp == 1)
      continue;

    SmallVector<LiveInterval *> NewLIs;
    const TargetRegisterClass *RC = MRI->getRegClass(LI.reg());
    for (unsigned I = 1; I < NumComp; ++I) {
      Register NewR = MRI->createVirtualRegister(RC);
      NewLIs.push_back(&LIS->createEmptyInterval(NewR));
    }
    EQC.Distribute(LI, NewLIs.begin(), *MRI);
  }
}

bool GBEarlyLowerIntoStack::isInLiveRanges(
    std::vector<LiveRange *> const &Ranges,
    ArrayRef<SlotIndex> Indicies) const {
  for (auto *Range : Ranges) {
    if (Range->isLiveAtIndexes(Indicies)) {
      return true;
    }
  }
  return false;
}

std::pair<unsigned, Align>
GBEarlyLowerIntoStack::getSpillSizeAlign(Register Reg, LaneBitmask Lane) const {
  auto GetPair = [&](const TargetRegisterClass &RegClass) {
    return std::make_pair(TRI->getSpillSize(RegClass),
                          TRI->getSpillAlign(RegClass));
  };

  auto *SuperClass = MRI->getRegClass(Reg);
  if (TRI->getRegSizeInBits(*SuperClass) == 16 && not Lane.all()) {
    return GetPair(GB::GPR8RegClass);
  }

  assert(Lane.all());
  return GetPair(*SuperClass);
}

std::pair<unsigned, bool>
GBEarlyLowerIntoStack::subregInfo(Register Reg, LaneBitmask Mask) const {
  auto *RegClass = MRI->getRegClass(Reg);

  if (Mask.all()) {
    // Entire 8 or 16-bit register
    if (TRI->getRegSizeInBits(*RegClass) == 8) {
      return std::make_pair(0U, true);
    }
    return std::make_pair(0U, false);
  }

  // 16-bit reg, but def/use for 8-bit subreg
  SmallVector<unsigned> SubRegs;
  bool DidMatch = TRI->getCoveringSubRegIndexes(RegClass, Mask, SubRegs);
  assert(DidMatch);
  assert(SubRegs.size() == 1);
  assert(TRI->getRegSizeInBits(*RegClass) == 16);
  return std::make_pair(SubRegs[0], true);
}

void GBEarlyLowerIntoStack::saveToSlot(MachineBasicBlock *MBB,
                                       MachineBasicBlock::iterator MBBI,
                                       DebugLoc DL, Register Reg,
                                       LaneBitmask Mask, unsigned FrameIndex) {
  auto const *MCInfo = MF->getTarget().getMCInstrInfo();

  auto [SubReg, Is8BitStore] = subregInfo(Reg, Mask);
  if (Is8BitStore) {
    MachineMemOperand *StoreMMO = MF->getMachineMemOperand(
        MachinePointerInfo::getFixedStack(*MF, FrameIndex),
        MachineMemOperand::MOStore, MFI->getObjectSize(FrameIndex),
        MFI->getObjectAlign(FrameIndex));

    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_HL_SP))
        .addFrameIndex(FrameIndex);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_iHL_r))
        .addReg(Reg, getKillRegState(true), SubReg)
        .addReg(GB::HL, getImplRegState(true) | getKillRegState(true))
        .addMemOperand(StoreMMO);
  } else {
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_HL_SP))
        .addFrameIndex(FrameIndex);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_iHL_r))
        .addReg(Reg, getKillRegState(true), 1)
        .addReg(GB::HL, getImplRegState(true));
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::INC16), GB::HL).addReg(GB::HL);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_iHL_r))
        .addReg(Reg, getKillRegState(true), 2)
        .addReg(GB::HL, getImplRegState(true) | getKillRegState(true));
  }
}

void GBEarlyLowerIntoStack::loadFromSlot(MachineBasicBlock *MBB,
                                         MachineBasicBlock::iterator MBBI,
                                         DebugLoc DL, Register Reg,
                                         LaneBitmask Mask,
                                         unsigned FrameIndex) {
  auto const *MCInfo = MF->getTarget().getMCInstrInfo();

  auto [SubReg, Is8BitStore] = subregInfo(Reg, Mask);
  if (Is8BitStore) {
    MachineMemOperand *LoadMMO = MF->getMachineMemOperand(
        MachinePointerInfo::getFixedStack(*MF, FrameIndex),
        MachineMemOperand::MOLoad, MFI->getObjectSize(FrameIndex),
        MFI->getObjectAlign(FrameIndex));

    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_HL_SP))
        .addFrameIndex(FrameIndex);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_r_iHL))
        .addDef(Reg, /*Flags=*/0, SubReg)
        .addReg(GB::HL, getImplRegState(true) | getKillRegState(true))
        .addMemOperand(LoadMMO);
  } else {
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_HL_SP))
        .addFrameIndex(FrameIndex);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_r_iHL))
        .addDef(Reg, /*Flags=*/0, 1)
        .addReg(GB::HL, getImplRegState(true));
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::INC16), GB::HL).addReg(GB::HL);
    BuildMI(*MBB, MBBI, DL, MCInfo->get(GB::LD_r_iHL))
        .addDef(Reg, /*Flags=*/0, 2)
        .addReg(GB::HL, getImplRegState(true) | getKillRegState(true));
  }
}

void GBEarlyLowerIntoStack::clearState() {
  ReloadableRegions.clear();
  LiveThroughs.clear();
  VirtualIntervals.clear();
  PhysicalRanges.clear();

  HLRegRanges.clear();
  ARegRanges.clear();
  FRegRanges.clear();

  RemainingReloadableRegions.clear();
  RemainingLiveThroughs.clear();

  SelectedReloadableRegions.clear();
  SelectedLiveThroughs.clear();
}

void GBEarlyLowerIntoStack::populateIntervals() {
  clearState();

  // Populate the virtual intervals
  for (unsigned I = 0, E = MRI->getNumVirtRegs(); I != E; ++I) {
    Register Reg = Register::index2VirtReg(I);
    if (MRI->reg_nodbg_empty(Reg)) {
      continue;
    }

    auto &Interval = LIS->getInterval(Reg);
    VirtualIntervals.push_back(&Interval);
  }

  // Populate the physical ranges
  for (unsigned Unit = 0; Unit < TRI->getNumRegUnits(); Unit += 1) {
    auto &Range = LIS->getRegUnit(Unit);
    PhysicalRanges.push_back(&Range);
  }

  for (auto Unit : TRI->regunits(GB::HL)) {
    HLRegRanges.push_back(&LIS->getRegUnit(Unit));
  }
  for (auto Unit : TRI->regunits(GB::A)) {
    ARegRanges.push_back(&LIS->getRegUnit(Unit));
  }
  for (auto Unit : TRI->regunits(GB::F)) {
    FRegRanges.push_back(&LIS->getRegUnit(Unit));
  }

  // Populate working region info
  for (auto *Interval : VirtualIntervals) {
    auto Reg = Interval->reg();
    if (Reg.isPhysical()) {
      continue;
    }

    llvm::SmallVector<std::pair<LaneBitmask, LiveRange const *>> LaneMasks;
    llvm::SmallVector<LiveThroughInfo::BBInfo, 4> BBInfos;
    for (auto &Sub : Interval->subranges()) {
      LaneMasks.emplace_back(Sub.LaneMask, &Sub);
    }
    if (LaneMasks.empty()) {
      // All 8-bit regs and 16-bit regs that are exclusively used 16-bit
      LaneMasks.emplace_back(LaneBitmask::getAll(), Interval);
    }

    for (auto [Lane, SubInterval] : LaneMasks) {
      for (auto &MBB : *MF) {
        MachineInstr *FirstUse = nullptr;
        bool HasAnyDef = false;
        bool FirstDefBeforeUse = false;

        MachineInstr *PreviousUseDef = nullptr;

        for (auto &MI : MBB) {
          if (MI.isDebugInstr()) {
            continue;
          }

          auto [Reads, Writes] =
              readsWritesVirtualRegister(*TRI, MI, Reg, Lane);
          if (Reads && FirstUse == nullptr) {
            FirstUse = &MI;
          }
          if (Writes && FirstUse == nullptr) {
            FirstDefBeforeUse = true;
          }
          if (Writes) {
            HasAnyDef = true;
          }

          if (Reads || Writes) {
            if (PreviousUseDef != nullptr && Reads) {
              ReloadableRegionInfo Info = {
                  LIS->getInstructionIndex(*PreviousUseDef),
                  LIS->getInstructionIndex(MI), Interval, Lane};
              ReloadableRegions.push_back(Info);
            }
            PreviousUseDef = &MI;
          }
        }

        LiveThroughInfo::BBInfo ToAdd{};
        ToAdd.MBB = &MBB;

        if (PreviousUseDef == nullptr) {
          // This MBB does not touch the register, neither Start nor End are set
          // Insert the entry if we simply live through the MBB, otherwise
          // ignore
          if (LIS->isLiveInToMBB(Interval, &MBB)) {
            assert(LIS->isLiveOutOfMBB(Interval, &MBB));
            BBInfos.push_back(ToAdd);
          }
          continue;
        }

        if (not FirstDefBeforeUse && LIS->isLiveInToMBB(*SubInterval, &MBB)) {
          ToAdd.Start = LIS->getInstructionIndex(*FirstUse);
        }

        if (HasAnyDef && LIS->isLiveOutOfMBB(*SubInterval, &MBB)) {
          ToAdd.End = LIS->getInstructionIndex(*PreviousUseDef);
        }

        if (ToAdd.Start.isValid() || ToAdd.End.isValid()) {
          BBInfos.push_back(ToAdd);
        }
      }
      if (not BBInfos.empty()) {
        LiveThroughs.push_back(LiveThroughInfo{
            std::move(BBInfos),
            Interval,
            Lane,
        });
      }
    }
  }

  // Populate the working queues
  RemainingReloadableRegions.clear();
  for (auto &Region : ReloadableRegions) {
    RemainingReloadableRegions.push_back(&Region);
  }

  RemainingLiveThroughs.clear();
  for (auto &LiveThrough : LiveThroughs) {
    RemainingLiveThroughs.push_back(&LiveThrough);
  }
}

void GBEarlyLowerIntoStack::sortWorklistByPriority() {
  // TODO
}

bool GBEarlyLowerIntoStack::maybeSelectIntervalToLower() {
  if (not GBDebugDontLowerRegions) {
    if (not RemainingReloadableRegions.empty()) {
      auto *Region = RemainingReloadableRegions.back();
      RemainingReloadableRegions.pop_back();
      if (!isInLiveRanges(HLRegRanges,
                          {Region->Start.getNextIndex(), Region->End}) &&
          !isInLiveRanges(FRegRanges,
                          {Region->Start.getNextIndex(), Region->End})) {
        SelectedReloadableRegions.push_back(Region);
        return true;
      }
    }
  }

  if (not GBDebugDontLowerLiveThroughs) {
    if (not RemainingLiveThroughs.empty()) {
      auto *LiveThrough = RemainingLiveThroughs.back();
      RemainingLiveThroughs.pop_back();

      bool IsPossible = true;
      for (auto &Info : LiveThrough->BlocksInfo) {
        if ((Info.Start.isValid() &&
             (isInLiveRanges(HLRegRanges, {Info.Start}) ||
              isInLiveRanges(FRegRanges, {Info.Start}))) ||
            (Info.End.isValid() && Info.End.getNextIndex().isValid() &&
             (isInLiveRanges(HLRegRanges, {Info.End.getNextIndex()}) ||
              isInLiveRanges(FRegRanges, {Info.End.getNextIndex()})))) {
          IsPossible = false;
          break;
        }
      }

      if (IsPossible) {
        SelectedLiveThroughs.push_back(LiveThrough);
      }
      return true;
    }
  }
  return false;
}

bool GBEarlyLowerIntoStack::applyChangesToRegions() {
  bool DidMakeChanges = false;

  // It is very easy to reuse slots that expire after leaving a BB
  std::map<unsigned, std::vector<int>> ClassToSlots;
  std::map<MachineBasicBlock const *, std::map<unsigned, size_t>>
      SlotsUsedInBasicBlock;

  for (auto *Region : SelectedReloadableRegions) {
    auto Reg = Region->Interval->reg();
    auto [SpillSize, SpillAlign] =
        getSpillSizeAlign(Reg, Region->SubRegLaneMask);

    auto *Start = LIS->getInstructionFromIndex(Region->Start);
    auto *End = LIS->getInstructionFromIndex(Region->End);
    auto *MBB = Start->getParent();

    // Allocate or reuse a slot index
    auto &SlotsInClass = ClassToSlots[SpillSize];
    auto SlotIndex = SlotsUsedInBasicBlock[MBB][SpillSize]++;
    if (SlotIndex >= SlotsInClass.size()) {
      assert(SpillSize == SpillAlign.value());
      auto NewSlotIndex = MFI->CreateSpillStackObject(SpillSize, SpillAlign);
      SlotsInClass.push_back(NewSlotIndex);
    }
    assert(SlotIndex < SlotsInClass.size());

    assert(not isInLiveRanges(HLRegRanges,
                              {Region->Start.getNextIndex(), Region->End}));
    assert(not isInLiveRanges(FRegRanges,
                              {Region->Start.getNextIndex(), Region->End}));

    // Save to slot
    MachineBasicBlock::iterator MBBI = *Start;
    ++MBBI;

    saveToSlot(MBB, MBBI, Start->getDebugLoc(), Reg, Region->SubRegLaneMask,
               SlotsInClass[SlotIndex]);

    // Load from slot
    MBBI = *End;
    loadFromSlot(MBB, MBBI, End->getDebugLoc(), Reg, Region->SubRegLaneMask,
                 SlotsInClass[SlotIndex]);

    DidMakeChanges = true;
  }
  return DidMakeChanges;
}

bool GBEarlyLowerIntoStack::applyChangesToLiveThroughs() {
  bool DidMakeChanges = false;
  for (auto *LiveThrough : SelectedLiveThroughs) {
    Register Reg = LiveThrough->Interval->reg();
    auto [SpillSize, SpillAlign] =
        getSpillSizeAlign(Reg, LiveThrough->SubRegLaneMask);

    auto NewSlotIndex = MFI->CreateSpillStackObject(SpillSize, SpillAlign);

    for (auto const &Info : LiveThrough->BlocksInfo) {
      auto *MBB = Info.MBB;

      if (Info.Start.isValid()) {
        assert(not isInLiveRanges(HLRegRanges, {Info.Start}));
        assert(not isInLiveRanges(FRegRanges, {Info.Start}));

        auto *Start = LIS->getInstructionFromIndex(Info.Start);
        MachineBasicBlock::iterator MBBI = *Start;
        loadFromSlot(MBB, MBBI, Start->getDebugLoc(), Reg,
                     LiveThrough->SubRegLaneMask, NewSlotIndex);
      }

      if (Info.End.isValid()) {
        if (Info.End.getNextIndex().isValid()) {
          assert(not isInLiveRanges(HLRegRanges, {Info.End.getNextIndex()}));
          assert(not isInLiveRanges(FRegRanges, {Info.End.getNextIndex()}));
        }

        auto *End = LIS->getInstructionFromIndex(Info.End);
        MachineBasicBlock::iterator MBBI = *End;
        ++MBBI;
        saveToSlot(MBB, MBBI, End->getDebugLoc(), Reg,
                   LiveThrough->SubRegLaneMask, NewSlotIndex);
      }
      DidMakeChanges = true;
    }
  }
  return DidMakeChanges;
}

bool GBEarlyLowerIntoStack::applyChanges() {
  bool MadeChanges = false;
  MadeChanges |= applyChangesToRegions();
  MadeChanges |= applyChangesToLiveThroughs();
  return MadeChanges;
}

bool GBEarlyLowerIntoStack::runOnMachineFunction(MachineFunction &MF) {
  if (!GBEnableEarlyLowingIntoStack) {
    return false;
  }

  this->MF = &MF;
  LIS = &getAnalysis<LiveIntervalsWrapperPass>().getLIS();
  DT = &getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree();
  PDT = &getAnalysis<MachinePostDominatorTreeWrapperPass>().getPostDomTree();
  MRI = &MF.getRegInfo();
  TRI = MRI->getTargetRegisterInfo();
  MFI = &MF.getFrameInfo();

  populateIntervals();

  LLVM_DEBUG(dbgs() << "*************** GBLowerIntoStack *****************\n";
             LIS->dump(); MF.print(dbgs()));

  // Dump the intervals
  LLVM_DEBUG(dbgs() << "ReloadableRegions:\n";
             for (auto &Info : ReloadableRegions) { Info.print(dbgs()); });

  LLVM_DEBUG(dbgs() << "LiveThroughs:\n";
             for (auto &Info : LiveThroughs) { Info.print(dbgs()); });

  do {
    sortWorklistByPriority();
  } while (maybeSelectIntervalToLower());

  bool DidApplyChanges = applyChanges();
  if (DidApplyChanges) {
    LLVM_DEBUG(
        dbgs() << "*************** After GBLowerIntoStack *****************\n";
        LIS->dump(); MF.print(dbgs()););

    // Intervals may now contain multiple connected components.
    // Split them up into their own registers
    redistributeLiveIntervals();

    LLVM_DEBUG(dbgs() << "******* After redistributing *************** \n";
               LIS->dump(); MF.print(dbgs()));
  }
  return DidApplyChanges;
}

FunctionPass *llvm::createGBEarlyLowerIntoStack(GBTargetMachine &TM) {
  return new GBEarlyLowerIntoStack(TM);
}

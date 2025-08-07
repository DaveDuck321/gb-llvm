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
#include "llvm/IR/Operator.h"
#include "llvm/MC/LaneBitmask.h"
#include "llvm/Pass.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include <limits>
#include <variant>

using namespace llvm;

#define DEBUG_TYPE "gb-early-stack-lowering"

static cl::opt<bool>
    GBDisableEarlyLowingIntoStack("gb-disable-early-lowing-into-stack",
                                  cl::Hidden,
                                  cl::desc("Disables GBEarlyLowerIntoStack"));

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
  size_t Index;
  // Within a MBB, if on the stack, a store must be inserted after Start, a load
  // must be inserted before End.
  SlotIndex Start;
  SlotIndex End;
  LiveInterval *Interval;
  LaneBitmask SubRegLaneMask;
  std::vector<ReloadableRegionInfo *> CollidingSaveRestores;
  std::vector<ReloadableRegionInfo *> OverlappingWithStart;
  std::vector<ReloadableRegionInfo *> OverlappingWithEnd;
  std::vector<ReloadableRegionInfo *> TiedOverlaps;
  bool CanFoldWithPrevious;
  bool IsSelected;

  void print(raw_ostream &OS) const {
    OS << "Reloadable(%" << Interval->reg().virtRegIndex() << ", ";
    PrintLaneMask(SubRegLaneMask).Print(OS);
    OS << ") : [" << Start << "," << End << "]";
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
  std::set<SlotIndex> SlotsWithNoRegion;
  LiveInterval *Interval;
  LaneBitmask SubRegLaneMask;
  bool IsSelected;

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
  }
};

struct OverlapsAtIndex {
  int OverlapsFull16Bit = 0;
  int OverlapsHigh16 = 0;
  int OverlapsLow16 = 0;
  int Overlaps8 = 0;
  int InstructionCost = 0;

  void checkPositive() {
    assert(OverlapsFull16Bit >= 0);
    assert(OverlapsHigh16 >= 0);
    assert(OverlapsLow16 >= 0);
    assert(Overlaps8 >= 0);
    assert(InstructionCost >= 0);
  }

  OverlapsAtIndex &operator+=(OverlapsAtIndex const &Other) {
    OverlapsFull16Bit += Other.OverlapsFull16Bit;
    OverlapsHigh16 += Other.OverlapsHigh16;
    OverlapsLow16 += Other.OverlapsLow16;
    Overlaps8 += Other.Overlaps8;
    InstructionCost += Other.InstructionCost;
    checkPositive();
    return *this;
  }

  OverlapsAtIndex &operator-=(OverlapsAtIndex const &Other) {
    OverlapsFull16Bit -= Other.OverlapsFull16Bit;
    OverlapsHigh16 -= Other.OverlapsHigh16;
    OverlapsLow16 -= Other.OverlapsLow16;
    Overlaps8 -= Other.Overlaps8;
    InstructionCost -= Other.InstructionCost;
    checkPositive();
    return *this;
  }

  constexpr int getOverusage() const {
    int Physical16BitUsage = OverlapsFull16Bit;

    // High and Low subregs will form 16-bit regs. But only between themselves.
    int MinSubOverlaps = std::min(OverlapsHigh16, OverlapsLow16);
    int MaxSubOverlaps = std::max(OverlapsHigh16, OverlapsLow16);
    Physical16BitUsage += MinSubOverlaps;

    constexpr int Num16BitRegs = 3;
    int Physical16BitOverusage = std::max(0, Physical16BitUsage - Num16BitRegs);

    int RemainingSubOverlaps = MaxSubOverlaps - MinSubOverlaps;
    int Physical8BitUsage =
        Overlaps8 + RemainingSubOverlaps + 2 * Physical16BitUsage;

    constexpr int Num8BitRegs = 7;
    int Physical8BitOverusage = std::max(0, Physical8BitUsage - Num8BitRegs);

    // TODO: calculate an appropiate weight for this
    return Physical8BitOverusage + Physical16BitOverusage;
  }

  auto print(raw_ostream &OS) const {
    OS << "OverlapsFull16Bit: " << OverlapsFull16Bit << ", ";
    OS << "OverlapsHigh16: " << OverlapsHigh16 << ", ";
    OS << "OverlapsLow16: " << OverlapsLow16 << ", ";
    OS << "Overlaps8: " << Overlaps8 << ", ";
    OS << "Overusage: " << getOverusage();
  }
};

class GBEarlyLowerIntoStack : public MachineFunctionPass {
  static char ID;
  MachineFunction *MF;
  LiveIntervals *LIS;
  MachineDominatorTree *DT;
  MachinePostDominatorTree *PDT;
  MachineBlockFrequencyInfo *MBFI;
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

  std::map<SlotIndex, OverlapsAtIndex> CachedOverlaps;

  double CachedCost = 0.0; // TODO: cache as int type instead

public:
  GBEarlyLowerIntoStack() : MachineFunctionPass(ID) {}

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
  void incrementOverlaps(OverlapsAtIndex &Result, Register, LaneBitmask) const;

  void saveToSlot(MachineBasicBlock *, MachineBasicBlock::iterator MBBI,
                  DebugLoc, Register, LaneBitmask, unsigned FrameIndex);
  void loadFromSlot(MachineBasicBlock *, MachineBasicBlock::iterator MBBI,
                    DebugLoc, Register, LaneBitmask, unsigned FrameIndex);

  void clearState();
  void populateIntervals();
  void precomputeOverlaps();
  void calculateCostFromScratch();
  void populateValidIntervalsIntoWorklist();

  template <bool isIncremental = true>
  void setSelect(ReloadableRegionInfo *, bool IsSelect,
                 bool PrintDebug = false);

  bool shouldStoreAtStart(ReloadableRegionInfo *) const;

  OverlapsAtIndex countOverlaps(ReloadableRegionInfo *, SlotIndex,
                                bool PrintDebug = false) const;
  OverlapsAtIndex countOverlaps(LiveThroughInfo *, SlotIndex,
                                bool PrintDebug = false) const;
  OverlapsAtIndex countStaticOverlaps(SlotIndex, bool PrintDebug = false) const;

  std::pair<OverlapsAtIndex, OverlapsAtIndex>
  countOverlaps(ReloadableRegionInfo *) const;

  bool debugForceLowerNextRegionOrInterval();
  bool lowerNextRegionOrInterval();

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

  SI->reanalyze(*MF);
  LIS->reanalyze(*MF);
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

void GBEarlyLowerIntoStack::incrementOverlaps(OverlapsAtIndex &Result,
                                              Register Reg,
                                              LaneBitmask Mask) const {
  auto [SubRegIndex, Is8Bit] = subregInfo(Reg, Mask);
  if (SubRegIndex == 0 && Is8Bit) {
    Result.Overlaps8 += 1;
    return;
  }
  if (SubRegIndex == 0 && !Is8Bit) {
    Result.OverlapsFull16Bit += 1;
    return;
  }
  if (SubRegIndex == 1) {
    assert(Is8Bit);
    Result.OverlapsHigh16 += 1;
    return;
  }
  if (SubRegIndex == 2) {
    assert(Is8Bit);
    Result.OverlapsLow16 += 1;
    return;
  }
  llvm_unreachable("Non-classifiable register");
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
        .addDef(Reg, /*Flags=*/getUndefRegState(true), 1)
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

  CachedOverlaps.clear();
  CachedCost = 0.0;
}

void GBEarlyLowerIntoStack::populateIntervals() {
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
        bool HasPreviousRegion = false;

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
              auto [PreviousReads, PreviousWrites] =
                  readsWritesVirtualRegister(*TRI, *PreviousUseDef, Reg, Lane);
              ReloadableRegionInfo Info = {
                  ReloadableRegions.size(),
                  LIS->getInstructionIndex(*PreviousUseDef),
                  LIS->getInstructionIndex(MI),
                  Interval,
                  Lane,
                  {},
                  {},
                  {},
                  {},
                  /*CanFoldWithPrevious=*/HasPreviousRegion && !PreviousWrites,
                  /*IsSelected=*/false,
              };
              ReloadableRegions.push_back(Info);
              HasPreviousRegion = true;
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
            {},
            Interval,
            Lane,
            false,
        });
      }
    }
  }
}

void GBEarlyLowerIntoStack::precomputeOverlaps() {
  // Live throughs cache the slots that contribute to overlaps (overlaps inside
  // the first region are not counted here).
  for (auto &LiveThrough : LiveThroughs) {
    auto SubRegLaneMask = LiveThrough.SubRegLaneMask;

    auto Index = LiveThrough.Interval->beginIndex();
    auto End = LiveThrough.Interval->endIndex();
    for (; Index != End; Index = Index.getNextIndex()) {
      // Record the index iff the region analysis doesn't considered this index.
      bool IsAlreadyCounted = false;
      for (const auto &Region : ReloadableRegions) {
        if ((Region.SubRegLaneMask & SubRegLaneMask).none()) {
          continue;
        }

        if (Region.Start < Index && Region.End >= Index) {
          IsAlreadyCounted = true;
          break;
        }
      }

      if (!IsAlreadyCounted) {
        LiveThrough.SlotsWithNoRegion.emplace(Index);
      }
    }
  }

  // Regions cache the regions whose stack save/ restore may overlap with their
  // save/ restore
  for (auto &Target : ReloadableRegions) {
    for (auto &Other : ReloadableRegions) {
      if (&Other == &Target) {
        // We only consider live-through registers from anything placed before
        // us
        break;
      }

      if (Other.Start == Target.Start || Other.End == Target.End) {
        // We overlap with the register used in by Other
        Target.CollidingSaveRestores.push_back(&Other);
      }
    }
  }

  // Regions also cache the regions that may overlap with their save/ restore
  for (auto &Target : ReloadableRegions) {
    auto TargetStart = Target.Start.getBoundaryIndex();
    auto TargetEnd = Target.End.getBoundaryIndex();
    for (auto &Other : ReloadableRegions) {
      bool EitherSelected = false;
      if (Other.Start < TargetStart && Other.End >= TargetStart) {
        Target.OverlappingWithStart.push_back(&Other);
        EitherSelected = true;
      }

      if (Other.Start < TargetEnd && Other.End >= TargetEnd) {
        Target.OverlappingWithEnd.push_back(&Other);
        EitherSelected = true;
      }

      if (EitherSelected) {
        Target.TiedOverlaps.push_back(&Other);
      }
    }
  }
}

void GBEarlyLowerIntoStack::calculateCostFromScratch() {
  CachedOverlaps.clear();
  CachedCost = 0.0;

  const double RefFreq = MBFI->getEntryFreq().getFrequency();

  for (auto &Region : ReloadableRegions) {
    setSelect</*isIncremental=*/false>(&Region, Region.IsSelected);
  }

  // TODO: everything else

  // TODO: is this really all the slots?
  for (auto &[Slot, Overlaps] : CachedOverlaps) {
    Overlaps += countStaticOverlaps(Slot);
  }

  for (auto const &[Slot, Overlaps] : CachedOverlaps) {
    const auto *MBB = LIS->getMBBFromIndex(Slot);
    double ThisFreq = MBFI->getBlockFreq(MBB).getFrequency();
    double RelativeFreq = ThisFreq / RefFreq;

    CachedCost += RelativeFreq * Overlaps.getOverusage();
    LLVM_DEBUG(dbgs() << Slot << ": "; Overlaps.print(dbgs()); dbgs() << "\n";);
  }
  LLVM_DEBUG(dbgs() << "CachedCost: " << CachedCost << "\n");
}

void GBEarlyLowerIntoStack::populateValidIntervalsIntoWorklist() {
  for (auto &Region : ReloadableRegions) {
    // TODO: ideally this would have a few slots of leeway to reduce false
    // positives
    if (!isInLiveRanges(HLRegRanges,
                        {Region.Start.getNextIndex(), Region.End}) &&
        !isInLiveRanges(FRegRanges,
                        {Region.Start.getNextIndex(), Region.End})) {

      if (not GBDebugDontLowerRegions) {
        RemainingReloadableRegions.push_back(&Region);
      }
    }
  }

  for (auto &LiveThrough : LiveThroughs) {
    // TODO: ideally this would have a few slots of leeway to reduce false
    // positives
    bool IsValid = true;
    for (auto &Info : LiveThrough.BlocksInfo) {
      if ((Info.Start.isValid() &&
           (isInLiveRanges(HLRegRanges, {Info.Start}) ||
            isInLiveRanges(FRegRanges, {Info.Start}))) ||
          (Info.End.isValid() && Info.End.getNextIndex().isValid() &&
           (isInLiveRanges(HLRegRanges, {Info.End.getNextIndex()}) ||
            isInLiveRanges(FRegRanges, {Info.End.getNextIndex()})))) {
        IsValid = false;
        break;
      }
    }

    if (IsValid && not GBDebugDontLowerLiveThroughs) {
      RemainingLiveThroughs.push_back(&LiveThrough);
    }
  }
}

template <bool isIncremental>
void GBEarlyLowerIntoStack::setSelect(ReloadableRegionInfo *Region,
                                      bool IsSelect, bool PrintDebug) {
  assert(Region->IsSelected != IsSelect || isIncremental);

  auto Slot = Region->Start;
  auto End = Region->End;

  const double RefFreq = MBFI->getEntryFreq().getFrequency();
  const auto *MBB = LIS->getMBBFromIndex(Slot);
  double ThisFreq = MBFI->getBlockFreq(MBB).getFrequency();
  double RelativeFreq = ThisFreq / RefFreq;

  // Update index overlaps
  for (; Slot != End; Slot = Slot.getNextIndex()) {
    auto &SlotOverlaps = CachedOverlaps[Slot];
    if (PrintDebug) {
      LLVM_DEBUG(dbgs() << Slot << "\n"
                        << " Before: ";
                 SlotOverlaps.print(dbgs()); dbgs() << "\n";);
    }

    if constexpr (isIncremental) {
      Region->IsSelected = !IsSelect;
      auto OverlapsBefore = countOverlaps(Region, Slot);
      CachedCost -= RelativeFreq *
                    SlotOverlaps.getOverusage(); // TODO: rounding error here
      SlotOverlaps -= OverlapsBefore;
    }

    Region->IsSelected = IsSelect;
    auto OverlapsAfter = countOverlaps(Region, Slot);
    SlotOverlaps += OverlapsAfter;
    if constexpr (isIncremental) {
      CachedCost += RelativeFreq *
                    SlotOverlaps.getOverusage(); // TODO: rounding error here
    }

    if (PrintDebug) {
      LLVM_DEBUG(dbgs() << " After: "; SlotOverlaps.print(dbgs());
                 dbgs() << "\n\n";);
    }
  }

  // Update overlaps that don't correspond to indicies (such as newly added save/ restores)
  Region->IsSelected = !IsSelect;
  auto EndOverlapsBefore = countOverlaps(Region);

  // TODO: everything else
  Region->IsSelected = IsSelect;
  if (PrintDebug) {
    LLVM_DEBUG(dbgs() << "CachedCost: " << CachedCost << "\n");
  }
}

bool GBEarlyLowerIntoStack::shouldStoreAtStart(
    ReloadableRegionInfo *Region) const {
  assert(&ReloadableRegions[Region->Index] == Region);
  assert(Region->IsSelected);
  return !(Region->CanFoldWithPrevious &&
           ReloadableRegions[Region->Index - 1].IsSelected);
}

OverlapsAtIndex
GBEarlyLowerIntoStack::countOverlaps(ReloadableRegionInfo *Region,
                                     SlotIndex Index, bool PrintDebug) const {
  OverlapsAtIndex Result = {};
  if (Region->IsSelected) {
    return Result;
  }

  // Unselected regions simply count towards their register class
  if (Region->Start < Index && Region->End >= Index) {
    incrementOverlaps(Result, Region->Interval->reg(), Region->SubRegLaneMask);
  }
  return Result;
}

OverlapsAtIndex
GBEarlyLowerIntoStack::countOverlaps(LiveThroughInfo *LiveThrough,
                                     SlotIndex Index, bool PrintDebug) const {
  OverlapsAtIndex Result = {};
  if (LiveThrough->IsSelected) {
    return Result;
  }

  // Increment the counters iff the region analysis hasn't considered this
  // index.
  if (LiveThrough->SlotsWithNoRegion.count(Index) != 0) {
    incrementOverlaps(Result, LiveThrough->Interval->reg(),
                      LiveThrough->SubRegLaneMask);
  }
  return Result;
}

OverlapsAtIndex
GBEarlyLowerIntoStack::countStaticOverlaps(SlotIndex Index,
                                           bool PrintDebug) const {
  OverlapsAtIndex Result = {};
  auto *MI = LIS->getInstructionFromIndex(Index);

  // Count overlaps due to dead registers (which don't have an interval)
  if (MI && MI->isCall()) {
    // A call clobbers everything
    Result.Overlaps8 += 7;
  } else {
    // Count overlaps with physical register intervals
    for (const auto *Range : PhysicalRanges) {
      auto Liveness = Range->Query(Index);
      if (Liveness.valueOutOrDead()) {
        // TODO: how do I know this size of the physical register?
        Result.Overlaps8 += 1;
      }
    }
  }
  return Result;
}

std::pair<OverlapsAtIndex, OverlapsAtIndex>
GBEarlyLowerIntoStack::countOverlaps(ReloadableRegionInfo *Target) const {
  std::pair<OverlapsAtIndex, OverlapsAtIndex> Result = {};
  if (not Target->IsSelected) {
    return Result;
  }

  // At the very least we always contribute the register we load and HL
  bool StoreIsLowered = shouldStoreAtStart(Target);
  if (StoreIsLowered) {
    incrementOverlaps(Result.first, Target->Interval->reg(),
                      Target->SubRegLaneMask);
    Result.first.OverlapsFull16Bit += 1;
  }

  incrementOverlaps(Result.second, Target->Interval->reg(),
                    Target->SubRegLaneMask);
  Result.second.OverlapsFull16Bit += 1;

  for (const auto *Other : Target->CollidingSaveRestores) {
    if (!Other->IsSelected) {
      continue;
    }

    if (Other->Start == Target->Start && StoreIsLowered) {
      // We overlap with the register used in by Other
      incrementOverlaps(Result.first, Other->Interval->reg(),
                        Other->SubRegLaneMask);
    }

    if (Other->End == Target->End) {
      // We overlap with the register defined in by Other
      incrementOverlaps(Result.second, Other->Interval->reg(),
                        Other->SubRegLaneMask);
    }
  }

  if (StoreIsLowered) {
    for (const auto *MaybeOverlap : Target->OverlappingWithStart) {
      if (not MaybeOverlap->IsSelected) {
        continue;
      }
      incrementOverlaps(Result.first, MaybeOverlap->Interval->reg(),
                        MaybeOverlap->SubRegLaneMask);
    }
  }

  for (const auto *MaybeOverlap : Target->OverlappingWithEnd) {
    if (not MaybeOverlap->IsSelected) {
      continue;
    }
    incrementOverlaps(Result.second, MaybeOverlap->Interval->reg(),
                      MaybeOverlap->SubRegLaneMask);
  }
  return Result;
}

bool GBEarlyLowerIntoStack::debugForceLowerNextRegionOrInterval() {
  if (not RemainingReloadableRegions.empty()) {
    auto *Region = RemainingReloadableRegions.back();
    RemainingReloadableRegions.pop_back();
    Region->IsSelected = true;
    return true;
  }
  if (not RemainingLiveThroughs.empty()) {
    auto *Region = RemainingLiveThroughs.back();
    RemainingLiveThroughs.pop_back();
    Region->IsSelected = true;
    return true;
  }
  return false;
}

bool GBEarlyLowerIntoStack::lowerNextRegionOrInterval() {
  if (GBDebugEarlyLowingEverything) {
    return debugForceLowerNextRegionOrInterval();
  }

  double CurrentCost = CachedCost;
  if (CurrentCost == 0.0) {
    // No overuse, exit early
    return false;
  }

  double BestImprovement = std::numeric_limits<double>::min();
  std::variant<std::monostate, ReloadableRegionInfo *, LiveThroughInfo *>
      Choice = std::monostate{};

  // TODO: also sort by the cost of the store/ reload sequence
  for (auto *Region : RemainingReloadableRegions) {
    assert(not Region->IsSelected);
    setSelect(Region, true);
    double PotentialCost = CachedCost;
    setSelect(Region, false);
    double Improvement = CurrentCost - PotentialCost;
    if (Improvement > BestImprovement) {
      BestImprovement = Improvement;
      Choice = Region;
    }
    LLVM_DEBUG(Region->print(dbgs()); dbgs() << " = " << Improvement << "\n";);
    Region->IsSelected = false;
  }

  if (BestImprovement < 0.0 || std::holds_alternative<std::monostate>(Choice)) {
    // No benefit to further lowering
    return false;
  }

  if (auto *Region = std::get_if<ReloadableRegionInfo *>(&Choice)) {
    LLVM_DEBUG(dbgs() << "Selected: "; (*Region)->print(dbgs());
               dbgs() << "\n");
    RemainingReloadableRegions.erase(
        std::find(RemainingReloadableRegions.begin(),
                  RemainingReloadableRegions.end(), *Region));
    setSelect(*Region, true, /*PrintDebug=*/true);
  }

  if (auto *LiveThrough = std::get_if<LiveThroughInfo *>(&Choice)) {
    LLVM_DEBUG(dbgs() << "Selected: "; (*LiveThrough)->print(dbgs());
               dbgs() << "\n");
    RemainingLiveThroughs.erase(std::find(RemainingLiveThroughs.begin(),
                                          RemainingLiveThroughs.end(),
                                          *LiveThrough));
    (*LiveThrough)->IsSelected = true;
    llvm_unreachable("here");
  }

  return true;
}

bool GBEarlyLowerIntoStack::applyChangesToRegions() {
  bool DidMakeChanges = false;

  // It is very easy to reuse slots that expire after leaving a BB
  std::map<unsigned, std::vector<int>> ClassToSlots;
  std::map<MachineBasicBlock const *, std::map<unsigned, size_t>>
      SlotsUsedInBasicBlock;

  // TODO: also map across live-throughs
  std::map<MachineBasicBlock const *,
           std::map<std::pair<Register, LaneBitmask>, int>>
      SlotMapping;

  for (auto &Region : ReloadableRegions) {
    if (not Region.IsSelected) {
      continue;
    }

    auto Reg = Region.Interval->reg();
    auto [SpillSize, SpillAlign] =
        getSpillSizeAlign(Reg, Region.SubRegLaneMask);

    auto *Start = LIS->getInstructionFromIndex(Region.Start);
    auto *End = LIS->getInstructionFromIndex(Region.End);
    auto *MBB = Start->getParent();

    // Allocate or reuse a slot index
    unsigned Slot = 0;
    if (SlotMapping[MBB].count({Reg, Region.SubRegLaneMask}) == 0) {
      auto &SlotsInClass = ClassToSlots[SpillSize];
      auto SlotIndex = SlotsUsedInBasicBlock[MBB][SpillSize]++;
      if (SlotIndex >= SlotsInClass.size()) {
        assert(SpillSize == SpillAlign.value());
        auto NewSlotIndex = MFI->CreateSpillStackObject(SpillSize, SpillAlign);
        SlotsInClass.push_back(NewSlotIndex);
      }
      assert(SlotIndex < SlotsInClass.size());
      Slot = SlotsInClass[SlotIndex];
    } else {
      Slot = SlotMapping[MBB][{Reg, Region.SubRegLaneMask}];
    }

    assert(not isInLiveRanges(HLRegRanges,
                              {Region.Start.getNextIndex(), Region.End}));
    assert(not isInLiveRanges(FRegRanges,
                              {Region.Start.getNextIndex(), Region.End}));

    // Save to slot
    if (shouldStoreAtStart(&Region)) {
      MachineBasicBlock::iterator MBBI = *Start;
      ++MBBI;
      saveToSlot(MBB, MBBI, Start->getDebugLoc(), Reg, Region.SubRegLaneMask,
                 Slot);
    }

    // Load from slot
    MachineBasicBlock::iterator MBBI = *End;
    loadFromSlot(MBB, MBBI, End->getDebugLoc(), Reg, Region.SubRegLaneMask,
                 Slot);

    DidMakeChanges = true;
  }
  return DidMakeChanges;
}

bool GBEarlyLowerIntoStack::applyChangesToLiveThroughs() {
  bool DidMakeChanges = false;
  for (auto &LiveThrough : LiveThroughs) {
    if (not LiveThrough.IsSelected) {
      continue;
    }

    Register Reg = LiveThrough.Interval->reg();
    auto [SpillSize, SpillAlign] =
        getSpillSizeAlign(Reg, LiveThrough.SubRegLaneMask);

    auto NewSlotIndex = MFI->CreateSpillStackObject(SpillSize, SpillAlign);

    for (auto const &Info : LiveThrough.BlocksInfo) {
      auto *MBB = Info.MBB;

      if (Info.Start.isValid()) {
        assert(not isInLiveRanges(HLRegRanges, {Info.Start}));
        assert(not isInLiveRanges(FRegRanges, {Info.Start}));

        auto *Start = LIS->getInstructionFromIndex(Info.Start);
        MachineBasicBlock::iterator MBBI = *Start;
        loadFromSlot(MBB, MBBI, Start->getDebugLoc(), Reg,
                     LiveThrough.SubRegLaneMask, NewSlotIndex);
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
                   LiveThrough.SubRegLaneMask, NewSlotIndex);
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
  if (GBDisableEarlyLowingIntoStack) {
    return false;
  }

  this->MF = &MF;
  LIS = &getAnalysis<LiveIntervalsWrapperPass>().getLIS();
  DT = &getAnalysis<MachineDominatorTreeWrapperPass>().getDomTree();
  PDT = &getAnalysis<MachinePostDominatorTreeWrapperPass>().getPostDomTree();
  MBFI = &getAnalysis<MachineBlockFrequencyInfoWrapperPass>().getMBFI();
  MRI = &MF.getRegInfo();
  TRI = MRI->getTargetRegisterInfo();
  MFI = &MF.getFrameInfo();

  LLVM_DEBUG(
      dbgs() << "*************** GBEarlyLowerIntoStack *****************\n";
      LIS->dump(););

  clearState();
  populateIntervals();
  precomputeOverlaps();
  populateValidIntervalsIntoWorklist();
  calculateCostFromScratch();

  // Dump the intervals
  LLVM_DEBUG(dbgs() << "ReloadableRegions:\n";
             for (auto &Info : ReloadableRegions) {
               Info.print(dbgs());
               dbgs() << "\n";
             });

  LLVM_DEBUG(dbgs() << "LiveThroughs:\n"; for (auto &Info : LiveThroughs) {
    Info.print(dbgs());
    dbgs() << "\n";
  });

  while (lowerNextRegionOrInterval()) {
  }

  // Check the incremental updates have been correctly applied
  double CostBefore = CachedCost;
  calculateCostFromScratch();
  double CostAfter = CachedCost;
  LLVM_DEBUG(dbgs() << "IncrementCost: " << CostBefore
                    << ", FreshCost: " << CostAfter << "\n");
  assert(std::abs(CostBefore - CostAfter) < 0.1);

  bool DidApplyChanges = applyChanges();
  if (DidApplyChanges) {
    LLVM_DEBUG(dbgs() << "*************** After GBEarlyLowerIntoStack "
                         "*****************\n";
               LIS->dump(););

    // Intervals may now contain multiple connected components.
    // Split them up into their own registers
    redistributeLiveIntervals();

    LLVM_DEBUG(dbgs() << "******* After redistributing *************** \n";
               LIS->dump(););
  } else {
    LLVM_DEBUG(dbgs() << "*************** After GBEarlyLowerIntoStack "
                         "*****************\n Applied no changes!\n";);
  }

  return DidApplyChanges;
}

FunctionPass *llvm::createGBEarlyLowerIntoStack(GBTargetMachine &) {
  return new GBEarlyLowerIntoStack();
}

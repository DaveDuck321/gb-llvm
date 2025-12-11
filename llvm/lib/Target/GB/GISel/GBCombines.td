include "llvm/Target/GlobalISel/Combine.td"

class GBGenericInstruction : GenericInstruction { let Namespace = "GB"; }

class GBGenericBranchInstruction<dag inputs> : GBGenericInstruction {
  let OutOperandList = (outs);
  let InOperandList = !con(inputs, (ins unknown:$truebb));

  let hasSideEffects = false;
  let isBranch = true;
  let isTerminator = true;
}

def G_JP_CP
    : GBGenericBranchInstruction<(ins unknown:$tst, type0:$src1, type0:$src2)>;

def GBCombineBranchCC
    : GICombineRule<
          (defs root:$root),
          (match(G_ICMP $cmp_res, $p, $lhs, $rhs),
              (G_BRCOND $cmp_res, $dst_bb):$root,
              [{ return MRI.getType(${lhs}.getReg()) == LLT::scalar(8); }]),
          (apply(G_JP_CP $p, $lhs, $rhs, $dst_bb):$root)>;

// This really should be c++ but I'm leaving this as tablegen out of protest.
// (apply
//      (G_CONSTANT $one, 1),
//      (G_JP_CP CmpInst::ICMP_EQ, $cond, $one, $dst_bb):$root)

def GBNormalizeBranch
    : GICombineRule<
          (defs root:$root),
          (match(G_BRCOND $cond, $dst_bb):$root,
              [{ return MRI.getType(${cond}.getReg()) == LLT::scalar(8); }]),
          (apply [{
            auto& MIR = Helper.getBuilder();
            auto One = MIR.buildConstant(MRI.createGenericVirtualRegister(LLT::scalar(8)), 1);
            MIR.buildInstr(GB::G_JP_CP)
              .addPredicate(CmpInst::ICMP_EQ)
              .addUse(${cond}.getReg())
              .addUse(One.getReg(0))
              .add(${dst_bb});
            ${root}->eraseFromParent();
        }]:$root)>;

def GBPreLegalizeCombiner
    : GICombiner<"GBPreLegalizeCombinerImpl", [all_combines, GBCombineBranchCC,
                                               GBNormalizeBranch]>;

def GBPostLegalizeCombiner
    : GICombiner<"GBPostLegalizeCombinerImpl", [all_combines, GBCombineBranchCC,
                                                GBNormalizeBranch]>;

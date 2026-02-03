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

def G_JP_BINARY_OP : GBGenericBranchInstruction<(ins untyped_imm_0:$opcode,
                         untyped_imm_0:$flag, type0:$src1, type0:$src2)>;

def gb_branch_cmp_to_jp_cp
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

def gb_branch_to_jp_cp
    : GICombineRule<
          (defs root:$root),
          (match(G_BRCOND $cond, $dst_bb):$root,
              [{ return MRI.getType(${cond}.getReg()) == LLT::scalar(8); }]),
          (apply [{
            auto& MIR = Helper.getBuilder();
            auto Zero = MIR.buildConstant(MRI.createGenericVirtualRegister(LLT::scalar(8)), 0);
            auto One = MIR.buildConstant(MRI.createGenericVirtualRegister(LLT::scalar(8)), 1);
            auto Masked = MIR.buildAnd(MRI.createGenericVirtualRegister(LLT::scalar(8)), ${cond}.getReg(), One.getReg(0));
            MIR.buildInstr(GB::G_JP_CP)
              .addPredicate(CmpInst::ICMP_NE)
              .addUse(Masked.getReg(0))
              .addUse(Zero.getReg(0))
              .add(${dst_bb});
            ${root}->eraseFromParent();
        }]:$root)>;

def gb_icmp_to_unsigned_icmp
    : GICombineRule<
          (defs root:$root, build_fn_matchinfo:$matchinfo),
          (match(G_ICMP $dst, $p, $lhs, $rhs):$root,
              [{ return matchCombineICMPToUnsigned(*${root}, ${matchinfo}); }]),
          (apply [{ Helper.applyBuildFn(*${root}, ${matchinfo}); }])>;

def gb_normalize_jp_cp
    : GICombineRule<
          (defs root:$root, build_fn_matchinfo:$matchinfo),
          (match(G_JP_CP $p, $lhs, $rhs, $dst_bb):$root,
              [{ return matchNormalizeJpCp(*${root}, ${matchinfo}); }]),
          (apply [{ Helper.applyBuildFn(*${root}, ${matchinfo}); }])>;

def gb_jp_cp_to_jp_binary
    : GICombineRule<
          (defs root:$root, build_fn_matchinfo:$matchinfo),
          (match(G_JP_CP $p, $lhs, $rhs, $dst_bb):$root,
              [{return matchJpCmpToJpBinaryOp(*${root}, ${matchinfo});}]),
          (apply [{ Helper.applyBuildFn(*${root}, ${matchinfo}); }])>;

def GBPreLegalizeCombiner
    : GICombiner<"GBPreLegalizeCombinerImpl", [all_combines,
                                               gb_branch_cmp_to_jp_cp,
                                               gb_branch_to_jp_cp,
                                               gb_icmp_to_unsigned_icmp]>;

def GBPostLegalizeCombiner
    : GICombiner<"GBPostLegalizeCombinerImpl",
                 [all_combines, gb_branch_cmp_to_jp_cp, gb_branch_to_jp_cp,
                  gb_icmp_to_unsigned_icmp, gb_normalize_jp_cp,
                  gb_jp_cp_to_jp_binary]>;

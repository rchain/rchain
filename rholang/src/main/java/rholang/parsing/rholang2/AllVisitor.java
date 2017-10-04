package rholang.parsing.rholang2;

import rholang.parsing.rholang2.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  rholang.parsing.rholang2.Absyn.Contr.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Proc.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Chan.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Bind.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.PMBranch.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.CBranch.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Value.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Quantity.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Entity.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Struct.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.Collect.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.VarPattern.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.PPattern.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.CPattern.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.PatternBind.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.PatternPatternMatch.Visitor<R,A>,
  rholang.parsing.rholang2.Absyn.ValPattern.Visitor<R,A>
{}

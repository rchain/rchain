package coop.rchain.syntax.rholang;

import coop.rchain.syntax.rholang.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  coop.rchain.syntax.rholang.Absyn.Contr.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.Proc.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.Chan.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.Bind.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.PMBranch.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.CBranch.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.RhoBool.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.Quantity.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.Value.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.VarPattern.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.PPattern.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.CPattern.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.PatternBind.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.PatternPatternMatch.Visitor<R,A>,
  coop.rchain.syntax.rholang.Absyn.ValPattern.Visitor<R,A>
{}

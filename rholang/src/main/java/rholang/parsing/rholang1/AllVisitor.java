package rholang.parsing.rholang1;

import rholang.parsing.rholang1.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  rholang.parsing.rholang1.Absyn.Contr.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Expr.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Bind.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.PMBranch.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.CBranch.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Value.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Quantity.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Entity.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Struct.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Collect.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.VarPattern.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.Pattern.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.PatternBind.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.PatternPatternMatch.Visitor<R,A>,
  rholang.parsing.rholang1.Absyn.ValPattern.Visitor<R,A>
{}

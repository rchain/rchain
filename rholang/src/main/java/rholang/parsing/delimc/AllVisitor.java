package rholang.parsing.delimc;

import rholang.parsing.delimc.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  rholang.parsing.delimc.Absyn.TypedExpr.Visitor<R,A>,
  rholang.parsing.delimc.Absyn.Expr.Visitor<R,A>,
  rholang.parsing.delimc.Absyn.Tuple.Visitor<R,A>,
  rholang.parsing.delimc.Absyn.Value.Visitor<R,A>,
  rholang.parsing.delimc.Absyn.Type.Visitor<R,A>,
  rholang.parsing.delimc.Absyn.TType.Visitor<R,A>
{}

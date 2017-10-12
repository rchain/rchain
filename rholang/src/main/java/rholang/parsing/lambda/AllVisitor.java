package rholang.parsing.lambda;

import rholang.parsing.lambda.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  rholang.parsing.lambda.Absyn.TypedExpr.Visitor<R,A>,
  rholang.parsing.lambda.Absyn.Expr.Visitor<R,A>,
  rholang.parsing.lambda.Absyn.Tuple.Visitor<R,A>,
  rholang.parsing.lambda.Absyn.Value.Visitor<R,A>,
  rholang.parsing.lambda.Absyn.Type.Visitor<R,A>,
  rholang.parsing.lambda.Absyn.TType.Visitor<R,A>
{}

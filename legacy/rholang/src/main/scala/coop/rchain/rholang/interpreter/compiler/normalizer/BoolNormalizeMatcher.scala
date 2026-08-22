package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{BoolFalse, BoolLiteral, BoolTrue}

object BoolNormalizeMatcher {
  def normalizeMatch(b: BoolLiteral): GBool =
    b match {
      case _: BoolTrue  => GBool(true)
      case _: BoolFalse => GBool(false)
    }
}

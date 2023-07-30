package coop.rchain.rholang.interpreter.compiler.normalizer

import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholangN.GBoolN
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{BoolFalse, BoolLiteral, BoolTrue}

object BoolNormalizeMatcher {
  def normalizeMatch(b: BoolLiteral): GBoolN =
    b match {
      case _: BoolTrue  => GBoolN(true)
      case _: BoolFalse => GBoolN(false)
    }
}

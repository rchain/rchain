package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Expr
import coop.rchain.models.Expr.ExprInstance.GBool
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.errors.NormalizerError

object GroundNormalizeMatcher {
  def normalizeMatch[F[_]: Sync](g: Ground): F[Expr] = {
    val ground: F[ExprN] = g match {
      case gb: GroundBool =>
        Sync[F].pure(BoolNormalizeMatcher.normalizeMatch(gb.boolliteral_))
      case gi: GroundInt =>
        Sync[F]
          .delay(gi.longliteral_.toLong)
          .adaptError { case e: NumberFormatException => NormalizerError(e.getMessage) }
          .map(long => GIntN(long))
      case gbi: GroundBigInt =>
        Sync[F]
          .delay(BigInt(gbi.longliteral_))
          .adaptError { case e: NumberFormatException => NormalizerError(e.getMessage) }
          .map(bigInt => GBigIntN(bigInt))
      case gs: GroundString => Sync[F].pure(GStringN(stripString(gs.stringliteral_)))
      case gu: GroundUri    => Sync[F].pure(GUriN(stripUri(gu.uriliteral_)))
    }
    ground.map(toProtoExpr)
  }
  // This is necessary to remove the backticks. We don't use a regular
  // expression because they're always there.
  def stripUri(raw: String): String = {
    require(raw.length >= 2)
    raw.substring(1, raw.length - 1)
  }
  // Similarly, we need to remove quotes from strings, since we are using
  // a custom string token
  def stripString(raw: String): String = {
    require(raw.length >= 2)
    raw.substring(1, raw.length - 1)
  }
}

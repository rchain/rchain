package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.MonadError
import cats.syntax.all._
import coop.rchain.models.Expr
import coop.rchain.models.Expr.ExprInstance.{GInt, GString, GUri}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  GroundBool,
  GroundInt,
  GroundString,
  GroundUri,
  Ground => AbsynGround
}
import coop.rchain.rholang.interpreter.errors.NormalizerError

import scala.util.Try

object GroundNormalizeMatcher {
  def normalizeMatch[F[_]](g: AbsynGround)(implicit M: MonadError[F, Throwable]): F[Expr] =
    g match {
      case gb: GroundBool => Expr(BoolNormalizeMatcher.normalizeMatch(gb.boolliteral_)).pure[F]
      case gi: GroundInt =>
        M.fromTry(
            Try(gi.longliteral_.toLong).adaptError {
              case e: NumberFormatException => NormalizerError(e.getMessage)
            }
          )
          .map { long =>
            Expr(GInt(long))
          }
      case gs: GroundString => Expr(GString(stripString(gs.stringliteral_))).pure[F]
      case gu: GroundUri    => Expr(GUri(stripUri(gu.uriliteral_))).pure[F]
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

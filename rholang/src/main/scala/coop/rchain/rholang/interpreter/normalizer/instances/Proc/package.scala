package coop.rchain.rholang.interpreter.normalizer.instances

import cats.MonadError
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.{ConnNotBody, ConnOrBody}
import coop.rchain.models.Expr.ExprInstance.{GBool, GInt, GString, GUri}
import coop.rchain.models.{EVar, Expr, Par, Var}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  BoolFalse,
  BoolLiteral,
  BoolTrue,
  GroundBool,
  GroundInt,
  GroundString,
  GroundUri,
  NameRemainder,
  NameRemainderEmpty,
  NameRemainderVar,
  ProcRemainder,
  ProcRemainderEmpty,
  ProcRemainderVar,
  ProcVar,
  ProcVarVar,
  ProcVarWildcard,
  Ground => AbsynGround
}
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  LevelContext,
  NameVisitOutputs,
  ProcSort,
  ProcVisitInputs,
  SourcePosition,
  VarSort
}
import coop.rchain.rholang.interpreter.errors.{
  InterpreterError,
  NormalizerError,
  PatternReceiveError,
  UnexpectedReuseOfProcContextFree
}
import coop.rchain.models.rholang.implicits._

import scala.util.Try

package object Proc {
  def handleProcVar[F[_]: Sync](
      pv: ProcVar,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    pv match {
      case pvw: ProcVarWildcard =>
        (
          Option(Var(Wildcard(Var.WildcardMsg()))),
          knownFree.addWildcard(SourcePosition(pvw.line_num, pvw.col_num))
        ).pure[F]
      case pvv: ProcVarVar =>
        val sourcePosition = SourcePosition(pvv.line_num, pvv.col_num)
        knownFree.get(pvv.var_) match {
          case None =>
            val newBindingsPair = knownFree.put((pvv.var_, ProcSort, sourcePosition))
            (Option(Var(FreeVar(knownFree.nextLevel))), newBindingsPair).pure[F]
          case Some(LevelContext(_, _, firstSourcePosition)) =>
            Sync[F].raiseError(
              UnexpectedReuseOfProcContextFree(pvv.var_, firstSourcePosition, sourcePosition)
            )
        }
    }

  def normalizeMatchProc[F[_]: Sync](
      r: ProcRemainder,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    r match {
      case _: ProcRemainderEmpty => (None: Option[Var], knownFree).pure[F]
      case pr: ProcRemainderVar =>
        handleProcVar[F](pr.procvar_, knownFree)
    }

  def normalizeMatchName[F[_]: Sync](
      nr: NameRemainder,
      knownFree: DeBruijnLevelMap[VarSort]
  ): F[(Option[Var], DeBruijnLevelMap[VarSort])] =
    nr match {
      case _: NameRemainderEmpty => (None: Option[Var], knownFree).pure[F]
      case nr: NameRemainderVar =>
        handleProcVar[F](nr.procvar_, knownFree)
    }

  def failOnInvalidConnective(
      input: ProcVisitInputs,
      depth: Int,
      nameRes: NameVisitOutputs
  ): Either[InterpreterError, NameVisitOutputs] =
    if (input.env.depth == 0) {
      Either
        .fromOption(
          nameRes.knownFree.connectives
            .collectFirst {
              case (_: ConnOrBody, sourcePosition) =>
                PatternReceiveError(s"\\/ (disjunction) at $sourcePosition")
              case (_: ConnNotBody, sourcePosition) =>
                PatternReceiveError(s"~ (negation) at $sourcePosition")
            },
          nameRes
        )
        .swap
    } else Right(nameRes)

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

  def normalizeBool(b: BoolLiteral): GBool =
    b match {
      case _: BoolTrue  => GBool(true)
      case _: BoolFalse => GBool(false)
    }

  def normalizeGround[F[_]: Sync](g: AbsynGround)(implicit M: MonadError[F, Throwable]): F[Expr] =
    g match {
      case gb: GroundBool => Expr(normalizeBool(gb.boolliteral_)).pure[F]
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
}

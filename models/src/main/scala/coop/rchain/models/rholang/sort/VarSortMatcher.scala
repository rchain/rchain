package coop.rchain.models.rholang.sort

import coop.rchain.models.Var
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import cats.implicits._
import cats.syntax._

object VarSortMatcher {
  def sortMatch(varOption: Option[Var]): Either[Throwable, ScoredTerm[Var]] =
    varOption match {
      case Some(v) =>
        v.varInstance match {
          case BoundVar(level) => ScoredTerm(v, Leaves(Score.BOUND_VAR, level)).asRight[Throwable]
          case FreeVar(level)  => ScoredTerm(v, Leaves(Score.FREE_VAR, level)).asRight[Throwable]
          case Wildcard(_)     => ScoredTerm(v, Leaves(Score.WILDCARD)).asRight[Throwable]
        }
      case None => Left(new IllegalArgumentException("VarSortMatcher was passed None"))
    }
}

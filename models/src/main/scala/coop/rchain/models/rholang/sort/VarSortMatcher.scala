package coop.rchain.models.rholang.sort

import coop.rchain.models.Var
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import cats.implicits._
import cats.syntax._

object VarSortMatcher {
  def sortMatch(v: Var): ScoredTerm[Var] =
    v.varInstance match {
      case BoundVar(level) => ScoredTerm(v, Leaves(Score.BOUND_VAR, level))
      case FreeVar(level)  => ScoredTerm(v, Leaves(Score.FREE_VAR, level))
      case Wildcard(_)     => ScoredTerm(v, Leaves(Score.WILDCARD))
    }
}

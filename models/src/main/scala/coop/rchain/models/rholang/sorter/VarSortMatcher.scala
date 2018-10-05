package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.Var
import coop.rchain.models.Var.VarInstance.{BoundVar, Empty, FreeVar, Wildcard}
import cats.implicits._
import cats.syntax._

private[sorter] object VarSortMatcher extends Sortable[Var] {
  def sortMatch[F[_]: Sync](v: Var): F[ScoredTerm[Var]] =
    v.varInstance match {
      case BoundVar(level) => ScoredTerm(v, Leaves(Score.BOUND_VAR, level)).pure[F]
      case FreeVar(level)  => ScoredTerm(v, Leaves(Score.FREE_VAR, level)).pure[F]
      case Wildcard(_)     => ScoredTerm(v, Leaves(Score.WILDCARD)).pure[F]
      case Empty           => ScoredTerm(Var(Empty), Leaf(Score.ABSENT)).pure[F]
    }
}

package coop.rchain.models.rholang.sort

import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance.GBool
import cats.implicits._

private[sort] object BoolSortMatcher extends Sortable[GBool] {
  def sortMatch[F[_]: Sync](g: GBool): F[ScoredTerm[GBool]] =
    if (g.value) {
      ScoredTerm(g, Leaves(Score.BOOL, 0)).pure[F]
    } else {
      ScoredTerm(g, Leaves(Score.BOOL, 1)).pure[F]
    }
}

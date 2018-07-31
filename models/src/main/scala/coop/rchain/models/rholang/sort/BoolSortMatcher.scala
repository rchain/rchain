package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance.GBool

object BoolSortMatcher extends Sortable[GBool] {
  def sortMatch(g: GBool): ScoredTerm[GBool] =
    if (g.value) {
      ScoredTerm(g, Leaves(Score.BOOL, 0))
    } else {
      ScoredTerm(g, Leaves(Score.BOOL, 1))
    }
}

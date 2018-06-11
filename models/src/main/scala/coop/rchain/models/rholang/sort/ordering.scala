package coop.rchain.models.rholang.sort

import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sort.ScoredTerm._

object ordering {

  implicit class SortOps(ps: List[Par]) {
    def sort: List[Par] =
      ps.map(par => ParSortMatcher.sortMatch(par).fold(th => throw th, score => score))
        .sorted
        .map(_.term)
  }
}

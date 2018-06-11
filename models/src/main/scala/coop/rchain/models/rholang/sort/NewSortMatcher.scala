package coop.rchain.models.rholang.sort

import coop.rchain.models.New
import coop.rchain.models.rholang.implicits._

object NewSortMatcher {
  def sortMatch(n: New): Either[Throwable, ScoredTerm[New]] =
    ParSortMatcher
      .sortMatch(n.p)
      .map(
        sortedPar =>
          ScoredTerm(New(bindCount = n.bindCount, p = sortedPar.term, locallyFree = n.locallyFree),
                     Node(Score.NEW, Leaf(n.bindCount), sortedPar.score)))
}

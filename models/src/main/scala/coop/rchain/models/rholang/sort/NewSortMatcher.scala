package coop.rchain.models.rholang.sort

import coop.rchain.models.New
import coop.rchain.models.rholang.implicits._

object NewSortMatcher extends Sortable[New] {
  def sortMatch(n: New): ScoredTerm[New] = {
    val sortedPar = ParSortMatcher.sortMatch(n.p)
    ScoredTerm(New(bindCount = n.bindCount, p = sortedPar.term, locallyFree = n.locallyFree),
               Node(Score.NEW, Leaf(n.bindCount), sortedPar.score))
  }
}

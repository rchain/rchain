package coop.rchain.models.rholang.sort

import coop.rchain.models.New

private[sort] object NewSortMatcher extends Sortable[New] {
  def sortMatch(n: New): ScoredTerm[New] = {
    val sortedPar = Sortable.sortMatch(n.p)
    ScoredTerm(
      New(bindCount = n.bindCount, p = sortedPar.term, uri = n.uri, locallyFree = n.locallyFree),
      new Node(Leaf(Score.NEW) +: (Leaf(n.bindCount) +: n.uri.map(Leaf.apply) :+ sortedPar.score))
    )
  }
}

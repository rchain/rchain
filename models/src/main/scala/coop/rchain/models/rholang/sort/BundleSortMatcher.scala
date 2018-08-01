package coop.rchain.models.rholang.sort

import coop.rchain.models.Bundle

private[sort] object BundleSortMatcher extends Sortable[Bundle] {
  def sortMatch(b: Bundle): ScoredTerm[Bundle] = {
    val score: Int = if (b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ_WRITE
    } else if (b.writeFlag && !b.readFlag) {
      Score.BUNDLE_WRITE
    } else if (!b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ
    } else {
      Score.BUNDLE_EQUIV
    }
    val sortedPar = Sortable
      .sortMatch(b.body)
    ScoredTerm(b.copy(body = sortedPar.term), Node(score, sortedPar.score))
  }
}

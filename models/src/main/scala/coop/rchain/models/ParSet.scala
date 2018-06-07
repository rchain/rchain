package coop.rchain.models

import scala.collection.immutable.BitSet

case class ParSet(ps: SortedHashSet[Par], connectiveUsed: Boolean = false) {
  lazy val locallyFree: BitSet = ParSet.updateLocallyFree(ps)
}

object ParSet {
  def updateLocallyFree(ps: SortedHashSet[Par]): BitSet =
    ps.sortedPars.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
}

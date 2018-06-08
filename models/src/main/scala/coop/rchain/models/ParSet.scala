package coop.rchain.models

import scala.collection.immutable.BitSet
import coop.rchain.models.rholang.sort.ordering._

case class ParSet private (ps: SortedHashSet[Par], connectiveUsed: Boolean) {
  lazy val locallyFree: BitSet = ParSet.updateLocallyFree(ps)

  override def equals(o: scala.Any): Boolean = o match {
    case parSet: ParSet =>
      this.ps == parSet.ps && this.connectiveUsed == parSet.connectiveUsed && this.locallyFree == parSet.locallyFree
    case _ => false
  }

  override def hashCode(): Int = (
    37 * this.ps.hashCode() +
      37 * this.connectiveUsed.hashCode() +
      37 * this.locallyFree.hashCode()
  )
}

object ParSet {
  def apply(ps: Seq[Par], connectiveUsed: Boolean = false): ParSet =
    ParSet(SortedHashSet(ps), connectiveUsed)

  def updateLocallyFree(ps: SortedHashSet[Par]): BitSet =
    ps.sortedPars.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
}

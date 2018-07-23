package coop.rchain.models

import java.util.Objects

import scala.collection.immutable.BitSet
import monix.eval.Coeval
import coop.rchain.models.rholang.sort.ordering._

//locallyFree is of type Coeval to make use of memoization
case class ParSet(ps: SortedParHashSet, connectiveUsed: Boolean, locallyFree: Coeval[BitSet]) {

  override def equals(o: scala.Any): Boolean = o match {
    case parSet: ParSet =>
      this.ps == parSet.ps && this.connectiveUsed == parSet.connectiveUsed && this.locallyFree.value == parSet.locallyFree.value
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(ps, Boolean.box(connectiveUsed), locallyFree.value)
}

object ParSet {
  def apply(ps: Seq[Par], connectiveUsed: Boolean, locallyFree: Coeval[BitSet]): ParSet =
    ParSet(SortedParHashSet(ps), connectiveUsed, locallyFree.memoize)

  def apply(ps: Seq[Par], connectiveUsed: Boolean = false): ParSet = {
    val shs = SortedParHashSet(ps)
    ParSet(shs, connectiveUsed, Coeval.delay(updateLocallyFree(shs)).memoize)
  }

  def updateLocallyFree(ps: SortedParHashSet): BitSet =
    ps.sortedPars.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
}

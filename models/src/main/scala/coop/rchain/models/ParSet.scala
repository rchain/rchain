package coop.rchain.models

import java.util.Objects

import monix.eval.Coeval

import scala.collection.immutable.BitSet

//locallyFree is of type Coeval to make use of memoization
case class ParSet(
    ps: SortedParHashSet,
    connectiveUsed: Boolean,
    locallyFree: Coeval[BitSet],
    remainder: Option[Var]
) {

  override def equals(o: scala.Any): Boolean = o match {
    case parSet: ParSet =>
      this.ps == parSet.ps &&
        this.remainder == parSet.remainder &&
        this.connectiveUsed == parSet.connectiveUsed
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(ps, remainder, Boolean.box(connectiveUsed))
}

object ParSet {
  def apply(
      ps: Seq[Par],
      connectiveUsed: Boolean,
      locallyFree: Coeval[BitSet],
      remainder: Option[Var]
  ): ParSet =
    ParSet(SortedParHashSet(ps), connectiveUsed, locallyFree.memoize, remainder)

  def apply(
      ps: Seq[Par],
      remainder: Option[Var]
  ): ParSet = {
    val shs = SortedParHashSet(ps)
    ParSet(
      shs,
      connectiveUsed(ps) || remainder.isDefined,
      Coeval.delay(updateLocallyFree(shs)).memoize,
      remainder
    )
  }

  def apply(ps: Seq[Par]): ParSet =
    apply(ps, None)

  private def connectiveUsed(seq: Seq[Par]): Boolean =
    seq.exists(_.connectiveUsed)

  private def updateLocallyFree(ps: SortedParHashSet): BitSet =
    ps.sortedPars.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
}

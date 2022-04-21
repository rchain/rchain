package coop.rchain.models

import monix.eval.Coeval

import java.util.Objects
import scala.collection.immutable.BitSet

final case class ParMap(
    ps: SortedParMap,
    connectiveUsed: Boolean,
    locallyFree: Coeval[BitSet],
    remainder: Option[Var]
) {

  override def equals(o: scala.Any): Boolean = o match {
    case parMap: ParMap =>
      this.ps == parMap.ps && this.remainder == parMap.remainder && this.connectiveUsed == parMap.connectiveUsed
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(ps, remainder, Boolean.box(connectiveUsed))
}

object ParMap {
  def apply(
      seq: Seq[(Par, Par)],
      connectiveUsed: Boolean,
      locallyFree: Coeval[BitSet],
      remainder: Option[Var]
  ) =
    new ParMap(SortedParMap(seq), connectiveUsed, locallyFree.memoize, remainder)

  def apply(
      seq: Seq[(Par, Par)],
      connectiveUsed: Boolean,
      locallyFree: BitSet,
      remainder: Option[Var]
  ): ParMap =
    apply(seq, connectiveUsed, Coeval.pure(locallyFree), remainder)

  def apply(seq: Seq[(Par, Par)]): ParMap =
    apply(seq, connectiveUsed(seq), updateLocallyFree(seq), None)

  def apply(map: Map[Par, Par]): ParMap = apply(map.toSeq)

  def apply(
      sMap: SortedParMap,
      connectiveUsed: Boolean,
      locallyFree: Coeval[BitSet],
      remainder: Option[Var]
  ) =
    new ParMap(sMap, connectiveUsed, locallyFree.memoize, remainder)

  def apply(
      sMap: SortedParMap,
      connectiveUsed: Boolean,
      locallyFree: BitSet,
      remainder: Option[Var]
  ): ParMap =
    apply(sMap, connectiveUsed, Coeval.pure(locallyFree), remainder)

  def apply(sMap: SortedParMap): ParMap =
    apply(sMap, connectiveUsed(sMap.unsortedList), updateLocallyFree(sMap.unsortedList), None)

  private def connectiveUsed(seq: Seq[(Par, Par)]): Boolean =
    seq.exists { case (k, v) => k.connectiveUsed || v.connectiveUsed }

  private def updateLocallyFree(seq: Seq[(Par, Par)]): BitSet =
    seq.foldLeft(BitSet()) {
      case (acc, (key, value)) =>
        acc | key.locallyFree | value.locallyFree
    }

}

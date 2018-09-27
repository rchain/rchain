package coop.rchain.models

import java.util.Objects

import monix.eval.Coeval

import scala.collection.immutable.BitSet
import coop.rchain.models.rholang.implicits._

case class ParMap(
    ps: SortedParMap,
    connectiveUsed: Boolean,
    locallyFree: Coeval[BitSet],
    remainder: Option[Var]
) {

  override def equals(o: scala.Any): Boolean = o match {
    case parMap: ParMap => this.ps == parMap.ps && this.remainder == parMap.remainder
    case _              => false
  }

  override def hashCode(): Int = Objects.hash(ps, remainder)
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

  def apply(map: SortedParMap): ParMap =
    apply(map.toSeq)

  private def connectiveUsed(map: Seq[(Par, Par)]): Boolean =
    map.exists { case (k, v) => k.connectiveUsed || v.connectiveUsed }

  private def updateLocallyFree(ps: Seq[(Par, Par)]): BitSet =
    ps.foldLeft(BitSet()) {
      case (acc, (key, value)) =>
        acc | key.locallyFree | value.locallyFree
    }

}

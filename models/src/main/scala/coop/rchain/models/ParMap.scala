package coop.rchain.models

import java.util.Objects

import monix.eval.Coeval

import scala.collection.immutable.BitSet
import coop.rchain.models.rholang.implicits._

case class ParMap(ps: SortedParMap, connectiveUsed: Boolean, locallyFree: Coeval[BitSet]) {

  override def equals(o: scala.Any): Boolean = o match {
    case parMap: ParMap => this.ps == parMap.ps
    case _              => false
  }

  override def hashCode(): Int = Objects.hash(ps)
}

object ParMap {
  def apply(seq: Seq[(Par, Par)], connectiveUsed: Boolean, locallyFree: Coeval[BitSet]): ParMap =
    new ParMap(SortedParMap(seq), connectiveUsed, locallyFree.memoize)

  def apply(seq: Seq[(Par, Par)], connectiveUsed: Boolean, locallyFree: BitSet): ParMap =
    new ParMap(SortedParMap(seq), connectiveUsed, Coeval.delay(locallyFree).memoize)

  def apply(seq: Seq[(Par, Par)], connectiveUsed: Boolean): ParMap =
    ParMap(seq, connectiveUsed, Coeval.delay(updateLocallyFree(seq)))

  def apply(seq: Seq[(Par, Par)]): ParMap =
    ParMap(SortedParMap(seq))

  def apply(map: SortedParMap): ParMap =
    ParMap(map, connectiveUsed(map), Coeval.delay(updateLocallyFree(map.toSeq)))

  def connectiveUsed(map: SortedParMap): Boolean =
    map.sortedMap.exists { case (k, v) => k.connectiveUsed || v.connectiveUsed }

  def updateLocallyFree(ps: Seq[(Par, Par)]): BitSet =
    ps.foldLeft(BitSet()) {
      case (acc, (key, value)) =>
        acc | key.locallyFree | value.locallyFree
    }

}

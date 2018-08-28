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
    new ParMap(SortedParMap(seq), connectiveUsed, Coeval.pure(locallyFree).memoize)

  def apply(seq: Seq[(Par, Par)]): ParMap =
    ParMap(seq, connectiveUsed(seq), Coeval.pure(updateLocallyFree(seq)))

  def apply(map: SortedParMap): ParMap =
    ParMap(map, connectiveUsed(map.toSeq), Coeval.pure(updateLocallyFree(map.toSeq)))

  private def connectiveUsed(map: Seq[(Par, Par)]): Boolean =
    map.exists { case (k, v) => k.connectiveUsed || v.connectiveUsed }

  private def updateLocallyFree(ps: Seq[(Par, Par)]): BitSet =
    ps.foldLeft(BitSet()) {
      case (acc, (key, value)) =>
        acc | key.locallyFree | value.locallyFree
    }

}

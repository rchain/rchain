package coop.rchain.rholang.interpreter

import scala.collection.immutable.BitSet

trait HasLocallyFree[T] {
  def wildcard(source: T): Boolean
  def locallyFree(source: T): BitSet
  def freeCount(source: T): Int
}

object HasLocallyFree {
  def apply[T](implicit ev: HasLocallyFree[T]): HasLocallyFree[T] = ev

  def wildcard[T: HasLocallyFree](source: T): Boolean =
    HasLocallyFree[T].wildcard(source)

  def locallyFree[T: HasLocallyFree](source: T): BitSet =
    HasLocallyFree[T].locallyFree(source)

  def freeCount[T: HasLocallyFree](source: T): Int =
    HasLocallyFree[T].freeCount(source)

}

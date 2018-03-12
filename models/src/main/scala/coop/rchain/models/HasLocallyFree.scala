package coop.rchain.models

import scala.collection.immutable.BitSet

trait HasLocallyFree[T] {
  def locallyFree(source: T): BitSet
  def freeCount(source: T): Int
}

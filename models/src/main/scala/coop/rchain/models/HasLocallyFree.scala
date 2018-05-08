package coop.rchain.models

import scala.collection.immutable.BitSet

trait HasLocallyFree[T] {
  def connectiveUsed(source: T): Boolean
  def locallyFree(source: T): BitSet
}

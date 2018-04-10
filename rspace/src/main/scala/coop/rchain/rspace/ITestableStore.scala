package coop.rchain.rspace

import scala.collection.immutable.Seq

/**
  * Used for unit-tests and other rspace-local calls
  */
private[rspace] trait ITestableStore[C, P] {

  private[rspace] type T

  def getPs(txn: T, channels: Seq[C]): Seq[Seq[P]]

  def isEmpty: Boolean
}

package coop.rchain.rspace

/**
  * Used for unit-tests and other rspace-local calls
  */
private[rspace] trait ITestableStore[C, P] {

  private[rspace] type T

  def getPs(txn: T, channels: List[C]): List[List[P]]

  def isEmpty: Boolean
}

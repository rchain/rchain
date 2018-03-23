package coop.rchain.storage

/**
  * Used for unit-tests and other package-local calls
  */
private[storage] trait ITestableStore[C, P] {

  private[storage] type T

  def getPs(txn: T, channels: List[C]): List[List[P]]

  def isEmpty: Boolean
}

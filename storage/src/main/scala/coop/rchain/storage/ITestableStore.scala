package coop.rchain.storage

/**
  * Used for unit-tests and other package-local calls
  */
private[storage] trait ITestableStore {
  def isEmpty: Boolean
}

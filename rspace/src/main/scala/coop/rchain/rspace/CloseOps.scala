package coop.rchain.rspace

trait CloseOps[T] {

  @volatile private[this] var isClosed = false

  private[rspace] def closeImp(): Unit

  final def close(): Unit = {
    isClosed = true
    closeImp()
  }

  private[rspace] def createTxnReadImp(): T

  private[rspace] def createTxnRead(): T =
    if (isClosed)
      throw new RSpaceClosedException()
    else
      createTxnReadImp()

  private[rspace] def createTxnWriteImp(): T

  private[rspace] def createTxnWrite(): T =
    if (isClosed)
      throw new RSpaceClosedException()
    else
      createTxnWriteImp()
}

package coop.rchain.rspace

trait CloseOps {
  @volatile private[this] var isClosed = false

  def isDbOpen : Boolean = !isClosed

  def close(): Unit =
    isClosed = true

  def failIfClosed(): Unit =
    if (isClosed)
      throw new RSpaceClosedException()
}

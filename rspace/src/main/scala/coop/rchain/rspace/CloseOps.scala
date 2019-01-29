package coop.rchain.rspace

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
// TODO Why!? Why would we ever allow code like that?
trait CloseOps {
  @volatile private[this] var isClosed = false

  def close(): Unit =
    isClosed = true

  def failIfClosed(): Unit =
    if (isClosed)
      throw new RSpaceClosedException()
}

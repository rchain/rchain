package coop.rchain.rspace
import cats.effect.Sync

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
// TODO Why!? Why would we ever allow code like that?
trait CloseOps[F[_]] {

  implicit val syncF: Sync[F]

  @volatile private[this] var isClosed = false

  def close: F[Unit] =
    syncF.delay {
      isClosed = true
    }

  def failIfClosed(): Unit =
    if (isClosed)
      throw new RSpaceClosedException()
}

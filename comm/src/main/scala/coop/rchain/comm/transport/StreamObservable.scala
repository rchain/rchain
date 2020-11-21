package coop.rchain.comm.transport

import java.nio.file._

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.transport.PacketOps._
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

final case class Stream(path: Path, sender: PeerNode)

class StreamObservable[F[_]: Sync: Log](peer: PeerNode, bufferSize: Int, folder: Path)(
    implicit scheduler: Scheduler
) extends Observable[Stream] {

  private val subject = buffer.LimitedBufferObservable.dropNew[Stream](bufferSize)

  def enque(blob: Blob): F[Unit] = {

    val logStreamInformation =
      Log[F].debug(
        s"Pushing message to $peer stream message queue."
      )

    val storeBlob: F[Option[Path]] =
      blob.packet.store[F](folder) >>= {
        case Right(file) => file.some.pure[F]
        case Left(e)     => Log[F].error(e.message) >> none.pure[F]
      }

    def push(file: Path): F[Boolean] =
      Sync[F].delay(subject.pushNext(Stream(file, blob.sender)))

    def propose(file: Path): F[Unit] = {
      val processError = Log[F].warn(
        s"Client stream message queue for $peer is full (${bufferSize} items). Dropping message.)"
      ) >> file.deleteSingleFile[F]
      push(file) >>= (pushSucceed => processError.unlessA(pushSucceed))
    }

    logStreamInformation >> storeBlob >>= (_.fold(().pure[F])(propose))
  }

  def unsafeSubscribeFn(subscriber: Subscriber[Stream]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => subscription.cancel()
  }
}

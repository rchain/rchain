package coop.rchain.comm.transport

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.transport.PacketOps._
import coop.rchain.shared.Log
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

import scala.collection.concurrent.TrieMap

final case class StreamMsgId(key: String, sender: PeerNode)

class StreamObservable[F[_]: Sync: Log](
    peer: PeerNode,
    bufferSize: Int,
    cache: TrieMap[String, Array[Byte]]
)(
    implicit scheduler: Scheduler
) extends Observable[StreamMsgId] {

  private val subject = buffer.LimitedBufferObservable.dropNew[StreamMsgId](bufferSize)

  def enque(blob: Blob): F[Unit] = {

    val logStreamInformation =
      Log[F].debug(
        s"Pushing message to $peer stream message queue."
      )

    val storeBlob: F[Option[String]] =
      blob.packet.store[F](cache) >>= {
        case Right(file) => file.some.pure[F]
        case Left(e)     => Log[F].error(e.message) >> none.pure[F]
      }

    def push(key: String): F[Boolean] =
      Sync[F].delay(subject.pushNext(StreamMsgId(key, blob.sender)))

    def propose(key: String): F[Unit] = {
      val processError = Log[F].warn(
        s"Client stream message queue for $peer is full (${bufferSize} items). Dropping message.)"
      ) >> Sync[F].delay(cache.remove(key))
      push(key) >>= (pushSucceed => processError.unlessA(pushSucceed))
    }

    logStreamInformation >> storeBlob >>= (_.fold(().pure[F])(propose))
  }

  def unsafeSubscribeFn(subscriber: Subscriber[StreamMsgId]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => subscription.cancel()
  }
}

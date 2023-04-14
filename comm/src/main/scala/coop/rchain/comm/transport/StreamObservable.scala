package coop.rchain.comm.transport

import cats.effect.{Async, Sync}
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.comm.transport.PacketOps._
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.Channel

import scala.collection.concurrent.TrieMap

final case class StreamMsgId(key: String, sender: PeerNode)

class StreamObservableClass[F[_]: Async: Log](
    peer: PeerNode,
    bufferSize: Int,
    cache: TrieMap[String, Array[Byte]],
    private val subject: Channel[F, StreamMsgId]
) {

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
      subject
        .trySend(StreamMsgId(key, blob.sender))
        .flatMap(
          _.leftTraverse(
            _ => new Exception(s"Channel is closed when trying to send.").raiseError[F, Boolean]
          ).map(_.merge)
        )

    def propose(key: String): F[Unit] = {
      val processError = Log[F].warn(
        s"Client stream message queue for $peer is full (${bufferSize} items). Dropping message.)"
      ) >> Sync[F].delay(cache.remove(key))
      push(key) >>= (pushSucceed => processError.unlessA(pushSucceed))
    }

    logStreamInformation >> storeBlob >>= (_.fold(().pure[F])(propose))
  }
}

object StreamObservable {
  type StreamObservable[F[_]] = (Blob => F[Unit], Stream[F, StreamMsgId])
  def apply[F[_]: Async: Log](
      peer: PeerNode,
      bufferSize: Int,
      cache: TrieMap[String, Array[Byte]]
  ): F[StreamObservable[F]] =
    Channel.bounded[F, StreamMsgId](bufferSize).map { q =>
      val x = new StreamObservableClass(peer, bufferSize, cache, q)
      (x.enque, q.stream)
    }
}

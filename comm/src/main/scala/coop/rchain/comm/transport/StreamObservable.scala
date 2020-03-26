package coop.rchain.comm.transport

import java.nio.file._

import cats.implicits._

import PacketOps._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._

import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

final case class StreamToPeers(peers: Seq[PeerNode], path: Path, sender: PeerNode)

class StreamObservable(bufferSize: Int, folder: Path)(implicit log: Log[Task], scheduler: Scheduler)
    extends Observable[StreamToPeers] {

  private val subject = buffer.LimitedBufferObservable.dropNew[StreamToPeers](bufferSize)

  def stream(peers: Seq[PeerNode], blob: Blob): Task[Unit] = {

    val logStreamInformation =
      log.debug(s"Streaming packet (type = ${blob.packet.typeId}) to peers ${peers.mkString(", ")}")

    val storeBlob: Task[Option[Path]] =
      blob.packet.store[Task](folder) >>= {
        case Right(file) => Task.pure(Some(file))
        case Left(e)     => log.error(e.message) >> None.pure[Task]
      }

    def push(file: Path): Task[Boolean] =
      Task.delay(subject.pushNext(StreamToPeers(peers, file, blob.sender)))

    def propose(file: Path): Task[Unit] =
      push(file) >>= (
        _.fold(
          log.debug(s"Enqueued for streaming packet $file"),
          log.warn(
            s"Client stream message queue is full. Dropping packet (type = ${blob.packet.typeId})"
          ) >> file.deleteSingleFile[Task]
        )
      )

    logStreamInformation >> storeBlob >>= (_.fold(Task.unit)(propose))
  }

  def unsafeSubscribeFn(subscriber: Subscriber[StreamToPeers]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => subscription.cancel()
  }
}

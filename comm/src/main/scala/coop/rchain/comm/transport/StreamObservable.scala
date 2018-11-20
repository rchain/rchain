package coop.rchain.comm.transport

import PacketOps._
import coop.rchain.comm._
import cats._, cats.data._, cats.implicits._
import coop.rchain.shared.Log
import coop.rchain.comm.PeerNode
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._
import monix.reactive.subjects.ConcurrentSubject
import java.nio.file._

case class ToStream(peerNode: PeerNode, path: Path, sender: PeerNode)

class StreamObservable(bufferSize: Int, folder: Path)(implicit log: Log[Task], scheduler: Scheduler)
    extends Observable[ToStream] {

  val subject = buffer.LimitedBufferObservable.dropNew[ToStream](bufferSize)

  def stream(peers: List[PeerNode], blob: Blob): Task[Unit] = {
    def push(peer: PeerNode): Task[Boolean] =
      blob.packet.store[Task](folder) >>= {
        case Right(file) => Task.delay(subject.pushNext(ToStream(peer, file, blob.sender)))
        case Left(UnableToStorePacket(p, er)) =>
          log.error(s"Could not serialize packet $p. Error message: $er") *> true.pure[Task]
        case Left(er) =>
          log.error(s"Could not serialize packet ${blob.packet}. Error: $er") *> true.pure[Task]
      }

    def retry(failed: List[PeerNode]): Task[Unit] =
      log.debug(s"Retrying for $failed") *> stream(failed, blob)

    for {
      results     <- peers.traverse(push _)
      paired      = peers.zip(results)
      (_, failed) = paired.partition(_._2)
      _           <- if (!failed.isEmpty) retry(failed.map(_._1)) else Task.unit
    } yield ()
  }

  def unsafeSubscribeFn(subscriber: Subscriber[ToStream]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => {
      subscription.cancel()
    }
  }
}

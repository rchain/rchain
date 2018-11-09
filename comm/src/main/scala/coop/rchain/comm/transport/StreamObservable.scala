package coop.rchain.comm.transport

import cats._, cats.data._, cats.implicits._
import coop.rchain.shared.Log
import coop.rchain.comm.PeerNode
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber
import monix.reactive.OverflowStrategy._
import monix.reactive.subjects.ConcurrentSubject

case class ToStream(peerNode: PeerNode, blob: Blob)

class StreamObservable(bufferSize: Int)(implicit log: Log[Task], scheduler: Scheduler)
    extends Observable[ToStream] {

  val subject = buffer.LimitedBufferObservable.dropNew[ToStream](bufferSize)

  def stream(peers: List[PeerNode], blob: Blob): Task[Unit] = {
    def push(peer: PeerNode): Task[Boolean] =
      Task.delay(subject.pushNext(ToStream(peer, blob)))

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

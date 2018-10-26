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

class StreamObservable(bufferSize: Int)(implicit scheduler: Scheduler)
    extends Observable[ToStream] {

  val subject = ConcurrentSubject.publishToOne[ToStream](DropNew(bufferSize))

  def stream(peers: List[PeerNode], blob: Blob): Task[Unit] =
    peers
      .traverse(
        peer =>
          Task.fromFuture {
            subject.onNext(ToStream(peer, blob))
          }
      )
      .as(())

  def unsafeSubscribeFn(subscriber: Subscriber[ToStream]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => {
      subscription.cancel()
    }
  }
}

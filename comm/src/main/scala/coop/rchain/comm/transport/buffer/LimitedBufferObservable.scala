package coop.rchain.comm.transport.buffer

import monix.execution.{Cancelable, Scheduler}
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishToOneSubject
import monix.reactive.Observable

trait LimitedBufferObservable[A] extends Observable[A] with LimitedBuffer[A]

object LimitedBufferObservable {
  def dropNew[A](bufferSize: Int)(implicit scheduler: Scheduler): LimitedBufferObservable[A] =
    new LimitedBufferObservable[A] {
      private[this] val subject = PublishToOneSubject[A]()
      private[this] val in      = new DropNewBuffer[A](Subscriber(subject, scheduler), bufferSize)

      def pushNext(elem: A): Boolean = in.pushNext(elem)
      def complete(): Unit           = in.complete()
      def unsafeSubscribeFn(subscriber: Subscriber[A]): Cancelable =
        subject.unsafeSubscribeFn(subscriber)
    }
}

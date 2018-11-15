package coop.rchain.comm.transport.buffer

import scala.concurrent.Future

import cats.implicits._

import monix.catnap.Semaphore
import monix.eval.Task
import monix.execution._
import monix.reactive._
import monix.reactive.observers.{BufferedSubscriber, Subscriber}
import monix.reactive.subjects._
import monix.reactive.OverflowStrategy._

trait Sequencer[I, +O] extends Observable[O] {
  def pushNext(elem: I): Task[Ack]
  def complete(): Task[Unit]
}

object Sequencer {

  def apply[A](): Task[Sequencer[A, A]] = create(PublishToOneSubject())

  def apply[I, O](subject: Subject[I, O]): Task[Sequencer[I, O]] = create(subject)

  def apply[A](bufferSize: Int)(implicit s: Scheduler): Task[Sequencer[A, A]] =
    create(new SubjectAsBuffered(PublishToOneSubject(), BackPressure(bufferSize), s))

  private def create[I, O](subject: Subject[I, O]): Task[Sequencer[I, O]] =
    Semaphore[Task](1) >>= (create(subject, _))

  private def create[I, O](
      subject: Subject[I, O],
      semaphore: Semaphore[Task]
  ): Task[Sequencer[I, O]] =
    Task.delay {
      new Sequencer[I, O] {
        def pushNext(elem: I): Task[Ack] =
          semaphore.withPermit(Task.fromFuture(subject.onNext(elem)))
        def complete(): Task[Unit] = Task.delay(subject.onComplete())
        def unsafeSubscribeFn(subscriber: Subscriber[O]): Cancelable =
          subject.unsafeSubscribeFn(subscriber)
      }
    }

  private class SubjectAsBuffered[I, +O](
      subject: Subject[I, O],
      overflowStrategy: OverflowStrategy[I],
      scheduler: Scheduler
  ) extends Subject[I, O] {

    private[this] val in: Subscriber[I] =
      BufferedSubscriber(Subscriber(subject, scheduler), overflowStrategy)

    def size: Int                    = subject.size
    def onNext(elem: I): Future[Ack] = in.onNext(elem)
    def onError(ex: Throwable): Unit = in.onError(ex)
    def onComplete(): Unit           = in.onComplete()
    def unsafeSubscribeFn(subscriber: Subscriber[O]): Cancelable =
      subject.unsafeSubscribeFn(subscriber)
  }
}

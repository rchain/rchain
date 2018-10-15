package coop.rchain.comm.transport.buffer

import scala.concurrent.{ExecutionContext, Future}

import monix.eval.{Callback, Task}
import monix.execution._
import monix.execution.Ack.Continue
import monix.reactive._
import monix.reactive.observers.{BufferedSubscriber, Subscriber}
import monix.reactive.subjects._
import monix.reactive.OverflowStrategy._

trait Sequencer[A] extends Observable[A] {
  def pushNext(elem: A): Task[Ack]
  def complete(): Task[Unit]
}

object Sequencer {

  def apply[A]()(implicit s: Scheduler): Sequencer[A] =
    create(SequencerSubject.publishToOne[A])

  def apply[A](bufferSize: Int)(implicit s: Scheduler): Sequencer[A] =
    create(SequencerSubject.publishToOne[A](BackPressure(bufferSize)))

  private def create[A](subject: SequencerSubject[A, A]): Sequencer[A] =
    new Sequencer[A]() {
      def pushNext(elem: A): Task[Ack] =
        Task.create { (_, cb) =>
          subject.onNext(SequencerMessage(elem, cb))
          Cancelable.empty
        }

      def unsafeSubscribeFn(subscriber: Subscriber[A]): Cancelable =
        subject.unsafeSubscribeFn(subscriber)

      def complete(): Task[Unit] = Task.delay(subject.onComplete())
    }
}

sealed abstract class SequencerSubject[I, +O] extends ConcurrentSubject[SequencerMessage[I], O]

final case class SequencerMessage[A](value: A, callback: Callback[Ack])

object SequencerSubject {

  def from[I, O](p: Subject[I, O])(implicit s: Scheduler): SequencerSubject[I, O] =
    new SubjectAsSequencer(p, s)

  def publish[A](implicit s: Scheduler): SequencerSubject[A, A] =
    from(PublishSubject())

  def publish[A](strategy: OverflowStrategy[A])(implicit s: Scheduler): SequencerSubject[A, A] =
    from(new SubjectAsBuffered(PublishSubject(), strategy, s))

  def publishToOne[A](implicit s: Scheduler): SequencerSubject[A, A] =
    from(PublishToOneSubject())

  def publishToOne[A](
      strategy: OverflowStrategy[A]
  )(implicit s: Scheduler): SequencerSubject[A, A] =
    from(new SubjectAsBuffered(PublishToOneSubject(), strategy, s))

  private final class SubjectAsSequencer[I, +O](
      subject: Subject[I, O],
      scheduler: Scheduler
  ) extends SequencerSubject[I, O] {

    private[this] val seq: Observer[SequencerMessage[I]] =
      new Observer[SequencerMessage[I]] {
        def onNext(elem: SequencerMessage[I]): Future[Ack] = {
          implicit val ec: ExecutionContext = scheduler
          subject.onNext(elem.value).andThen { case _ => elem.callback.onSuccess(Continue) }
        }

        def onError(ex: Throwable): Unit = subject.onError(ex)
        def onComplete(): Unit           = subject.onComplete()
      }

    private[this] val in: Subscriber.Sync[SequencerMessage[I]] =
      BufferedSubscriber.synchronous(Subscriber(seq, scheduler), Unbounded)

    def size: Int                              = subject.size
    def onNext(elem: SequencerMessage[I]): Ack = in.onNext(elem)
    def onError(ex: Throwable): Unit           = in.onError(ex)
    def onComplete(): Unit                     = in.onComplete()
    def unsafeSubscribeFn(subscriber: Subscriber[O]): Cancelable =
      subject.unsafeSubscribeFn(subscriber)
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

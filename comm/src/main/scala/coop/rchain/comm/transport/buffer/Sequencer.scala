package coop.rchain.comm.transport.buffer

import scala.concurrent.{ExecutionContext, Future}

import monix.eval.{Callback, Task}
import monix.execution._
import monix.execution.Ack.Continue
import monix.reactive._
import monix.reactive.observers.{BufferedSubscriber, Subscriber}
import monix.reactive.subjects._
import monix.reactive.OverflowStrategy._

/**
  * The Sequencer is a similar functionality as Monix ConcurrentSubject.
  * However, it allows to use any Monix OverflowStrategy and not only
  * the synchronous ones. Most importantly it allows to use backpressure
  * in a concurrent environment, where multiple threads are adding items
  * to the queue. In case of backpressure all adding threads are suspended
  * until the queue can accept further items.
  */
trait Sequencer[A] extends Observable[A] {
  def pushNext(elem: A): Task[Ack]
  def complete(): Task[Unit]
}

/**
  * Each time a thread is pushing a new element to the queue a
  * SequencerMessage gets created and enqueued to an unbounded
  * buffer. A SequencerMessage consists of the item and a
  * callback. The thread is suspended until the callback gets
  * called, either with a success message or a failure. The
  * buffer pushes the SequencerMessages one by one to a
  * backpressured observer which tries to push the message to the
  * underlying subject. On success the observer calls the callback
  * and the suspended thread (fiber) gets resumed and can enqueue
  * further items to the queue. See diagram:
  *
  *   +---+   SequencerMessage
  *   |   |   (item and callback)
  *   +-+-+
  *     |
  *     |
  * +---v---+
  * | +---+ |
  * | |   | |
  * | +---+ | Unbounded concurrent buffer
  * | +---+ | with SequencerMessages can
  * | |   | | enqueue from multiple threads
  * | +---+ |
  * | +---+ |
  * | |   | |
  * | +---+ |
  * +---+---+
  *     |
  *     |
  * +---v---+  Backpressured observer
  * | +---+ |  calls the callback after
  * | |   | |  pushing the item to the
  * | +---+ |  subject. One by one.
  * +---+---+
  *     |
  *     |
  * +---v---+
  * |       |
  * |  +-+  |  Backpressured subject with items
  * |  +-+  |  can have only one publisher with
  * |       |  respect to the backpressure contract
  * |  +-+  |
  * |  +-+  |
  * |       |
  * |  +-+  |
  * |  +-+  |
  * |       |
  * +---+---+
  *     |
  * +---v---+
  * |   +   |
  * |  +++  |  Consumer, can backpressure the source
  * |   +   |
  * +-------+
  *
  * Caveat: the sequencer should be used by a limited number of fibers
  * and/or a constraint parallelism. Otherwise the backpressure is pointless.
  */
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

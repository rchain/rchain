package coop.rchain.grpcmonix

import scala.concurrent.Future
import scala.util.control.NonFatal

import coop.rchain.shared.{Log, LogSource}

import com.google.common.util.concurrent.ListenableFuture
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution._
import monix.execution.Ack.{Continue, Stop}
import monix.reactive.Observable
import monix.reactive.Observable.Operator
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.PublishSubject
import org.reactivestreams.{Subscriber => SubscriberR}
import scalapb.grpc.Grpc

object GrpcMonix {

  private val logger                        = Log.logId
  private implicit val logSource: LogSource = LogSource(this.getClass)

  type GrpcOperator[I, O] = StreamObserver[O] => StreamObserver[I]
  type Transformer[I, O]  = Observable[I] => Observable[O]

  def guavaFutureToMonixTask[T](future: ListenableFuture[T]): Task[T] =
    Task.deferFuture {
      Grpc.guavaFuture2ScalaFuture(future)
    }

  def grpcOperatorToMonixOperator[I, O](grpcOperator: GrpcOperator[I, O]): Operator[I, O] = {
    outputSubsriber: Subscriber[O] =>
      val outputObserver: StreamObserver[O] = monixSubscriberToGrpcObserver(outputSubsriber)
      val inputObserver: StreamObserver[I]  = grpcOperator(outputObserver)
      grpcObserverToMonixSubscriber(inputObserver, outputSubsriber.scheduler)
  }

  def monixSubscriberToGrpcObserver[T](subscriber: Subscriber[T]): StreamObserver[T] =
    new StreamObserver[T] {
      override def onError(t: Throwable): Unit = subscriber.onError(t)
      override def onCompleted(): Unit         = subscriber.onComplete()
      override def onNext(value: T): Unit      = subscriber.onNext(value)
    }

  def reactiveSubscriberToGrpcObserver[T](subscriber: SubscriberR[_ >: T]): StreamObserver[T] =
    new StreamObserver[T] {
      override def onError(t: Throwable): Unit = subscriber.onError(t)
      override def onCompleted(): Unit         = subscriber.onComplete()
      override def onNext(value: T): Unit      = subscriber.onNext(value)
    }

  def grpcObserverToMonixSubscriber[T](observer: StreamObserver[T], s: Scheduler): Subscriber[T] =
    new Subscriber[T] {
      override implicit def scheduler: Scheduler = s
      override def onError(t: Throwable): Unit   = observer.onError(t)
      override def onComplete(): Unit            = observer.onCompleted()
      override def onNext(value: T): Future[Ack] =
        try {
          observer.onNext(value)
          Continue
        } catch {
          case t: Throwable =>
            observer.onError(t)
            Stop
        }
    }

  def grpcObserverToMonixCallback[T](observer: StreamObserver[T]): Callback[Throwable, T] =
    new Callback[Throwable, T] {
      override def onError(t: Throwable): Unit = observer.onError(t)
      override def onSuccess(value: T): Unit =
        try {
          observer.onNext(value)
          observer.onCompleted()
        } catch {
          case NonFatal(e) => logger.warn(s"Failed to send a response: ${e.getMessage}")
        }
    }

  def liftByGrpcOperator[I, O](
      observable: Observable[I],
      operator: GrpcOperator[I, O]
  ): Observable[O] =
    observable.liftByOperator(
      grpcOperatorToMonixOperator(operator)
    )

  def unliftByTransformer[I, O](
      transformer: Transformer[I, O],
      subscriber: Subscriber[O]
  ): Subscriber[I] =
    new Subscriber[I] {
      private[this] val subject = PublishSubject[I]()
      transformer(subject).subscribe(subscriber)

      override implicit def scheduler: Scheduler = subscriber.scheduler
      override def onError(t: Throwable): Unit   = subject.onError(t)
      override def onComplete(): Unit            = subject.onComplete()
      override def onNext(value: I): Future[Ack] = subject.onNext(value)
    }

}

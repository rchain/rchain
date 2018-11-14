package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import monix.eval.Task
import scala.concurrent.{ExecutionContext, Future}
import monix.execution.Scheduler

trait Futurable[F[_]] {
  def toFuture[A](fa: F[A]): Future[A]
}

object Futurable extends FuturableInstances {
  def apply[F[_]](implicit F: Futurable[F]): Futurable[F] = F
}

trait FuturableInstances extends FuturableInstances0 {
  implicit def taskFuturable(implicit scheduler: Scheduler): Futurable[Task] = new Futurable[Task] {
    def toFuture[A](fa: Task[A]): Future[A] = fa.runToFuture
  }
}

sealed trait FuturableInstances0 {
  implicit def eitherTFuturable[F[_]: Monad: Futurable, E](
      implicit ec: ExecutionContext
  ): Futurable[EitherT[F, E, ?]] =
    new Futurable[EitherT[F, E, ?]] {
      case class ToFutureException(e: E) extends RuntimeException

      def toFuture[A](fa: EitherT[F, E, A]): Future[A] =
        Futurable[F]
          .toFuture(fa.value)
          .flatMap {
            case Right(a) => Future.successful[A](a)
            case Left(e)  => Future.failed[A](ToFutureException(e))
          }

    }
}

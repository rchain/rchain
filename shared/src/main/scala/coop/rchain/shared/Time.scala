package coop.rchain.shared

import cats.Monad
import cats.data.EitherT
import cats.tagless._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS, NANOSECONDS}
import cats.effect.Temporal

// TODO: there is no reason for custom Timer definition, remove it
//  - for testing TestScheduler (monix) ot TestContext (cats-laws) (TestControl cats.effect 3) should be used
@autoFunctorK
@autoSemigroupalK
@autoProductNK
trait Time[F[_]] {
  def currentMillis: F[Long]
  def nanoTime: F[Long]
  def sleep(duration: FiniteDuration): F[Unit]
}

object Time extends TimeInstances {
  def apply[F[_]](implicit L: Time[F]): Time[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit TM: Time[F]): Time[T[F, *]] =
    new Time[T[F, *]] {
      def currentMillis: T[F, Long]                   = TM.currentMillis.liftM[T]
      def nanoTime: T[F, Long]                        = TM.nanoTime.liftM[T]
      def sleep(duration: FiniteDuration): T[F, Unit] = TM.sleep(duration).liftM[T]
    }

  /**
    * Default implementation from cats [[Timer]]
    */
  def fromTimer[F[_]](implicit timer: Temporal[F]): Time[F] =
    new Time[F] {
      def currentMillis: F[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: F[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): F[Unit] = timer.sleep(duration)
    }
}

sealed abstract class TimeInstances {
  implicit def eitherTTime[E, F[_]: Monad: Time[*[_]]]: Time[EitherT[F, E, *]] =
    Time.forTrans[F, EitherT[*[_], E, *]]
}

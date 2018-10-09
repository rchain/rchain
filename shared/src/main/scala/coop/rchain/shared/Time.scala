package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

import scala.concurrent.duration.FiniteDuration

trait Time[F[_]] {
  def currentMillis: F[Long]
  def nanoTime: F[Long]
  def sleep(duration: FiniteDuration): F[Unit]
}

object Time extends TimeInstances {
  def apply[F[_]](implicit L: Time[F]): Time[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit TM: Time[F]): Time[T[F, ?]] =
    new Time[T[F, ?]] {
      def currentMillis: T[F, Long]                   = TM.currentMillis.liftM[T]
      def nanoTime: T[F, Long]                        = TM.nanoTime.liftM[T]
      def sleep(duration: FiniteDuration): T[F, Unit] = TM.sleep(duration).liftM[T]
    }
}

sealed abstract class TimeInstances {
  implicit def eitherTTime[E, F[_]: Monad: Time[?[_]]]: Time[EitherT[F, E, ?]] = {
    import coop.rchain.catscontrib.eitherT._
    Time.forTrans[F, EitherT[?[_], E, ?]]
  }

  implicit def stateTTime[S, F[_]: Monad: Time[?[_]]]: Time[StateT[F, S, ?]] = {
    import coop.rchain.catscontrib.stateT._
    Time.forTrans[F, StateT[?[_], S, ?]]
  }
}

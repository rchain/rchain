package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Time[F[_]] {
  def currentMillis: F[Long]
  def nanoTime: F[Long]
}

object Time extends TimeInstances {
  def apply[F[_]](implicit L: Time[F]): Time[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit TM: Time[F]): Time[T[F, ?]] =
    new Time[T[F, ?]] {
      def currentMillis: T[F, Long] = TM.currentMillis.liftM[T]
      def nanoTime: T[F, Long]      = TM.nanoTime.liftM[T]
    }
}

sealed abstract class TimeInstances {
  implicit def eitherTTime[E, F[_]: Monad: Time[?[_]]]: Time[EitherT[F, E, ?]] =
    Time.forTrans[F, EitherT[?[_], E, ?]]
}

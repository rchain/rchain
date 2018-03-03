package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Metrics[F[_]] {
  def incrementCounter(name: String, delta: Long = 1): F[Unit]
  def setGauge(name: String, value: Long): F[Unit]
}

object Metrics extends MetricsInstances {
  def apply[F[_]](implicit M: Metrics[F]): Metrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit M: Metrics[F]): Metrics[T[F, ?]] =
    new Metrics[T[F, ?]] {
      def incrementCounter(name: String, delta: Long) = M.incrementCounter(name, delta).liftM[T]
      def setGauge(name: String, value: Long)         = M.setGauge(name, value).liftM[T]
    }
}

sealed abstract class MetricsInstances {
  implicit def eitherTMetrics[E, F[_]: Monad: Metrics[?[_]]]: Metrics[EitherT[F, E, ?]] =
    Metrics.forTrans[F, EitherT[?[_], E, ?]]
}

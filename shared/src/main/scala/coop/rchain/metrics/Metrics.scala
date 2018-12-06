package coop.rchain.metrics

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

trait MetricsSource {
  def name: String
}

object MetricsSource {
  def apply(source: String): MetricsSource = new MetricsSource {
    val name: String = source
  }
}

trait Metrics[F[_]] {
  // Counter
  def incrementCounter(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit]

  // RangeSampler
  def incrementSampler(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit]
  def sample(name: String)(implicit ev: MetricsSource): F[Unit]

  // Gauge
  def setGauge(name: String, value: Long)(implicit ev: MetricsSource): F[Unit]

  def incrementGauge(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit]

  def decrementGauge(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit]

  // Histogram
  def record(name: String, value: Long, count: Long = 1)(implicit ev: MetricsSource): F[Unit]
}

object Metrics extends MetricsInstances {
  def apply[F[_]](implicit M: Metrics[F]): Metrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](implicit M: Metrics[F]): Metrics[T[F, ?]] =
    new Metrics[T[F, ?]] {
      def incrementCounter(name: String, delta: Long)(implicit ev: MetricsSource) =
        M.incrementCounter(name, delta).liftM[T]
      def incrementSampler(name: String, delta: Long)(implicit ev: MetricsSource) =
        M.incrementSampler(name, delta).liftM[T]
      def sample(name: String)(implicit ev: MetricsSource) = M.sample(name).liftM[T]
      def setGauge(name: String, value: Long)(implicit ev: MetricsSource) =
        M.setGauge(name, value).liftM[T]
      def incrementGauge(name: String, delta: Long)(implicit ev: MetricsSource) =
        M.incrementGauge(name, delta).liftM[T]
      def decrementGauge(name: String, delta: Long)(implicit ev: MetricsSource) =
        M.decrementGauge(name, delta).liftM[T]
      def record(name: String, value: Long, count: Long)(implicit ev: MetricsSource) =
        M.record(name, value, count).liftM[T]
    }

  class MetricsNOP[F[_]: Applicative] extends Metrics[F] {
    def incrementCounter(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit] =
      ().pure[F]
    def incrementSampler(name: String, delta: Long = 1)(implicit ev: MetricsSource): F[Unit] =
      ().pure[F]
    def sample(name: String)(implicit ev: MetricsSource): F[Unit]                      = ().pure[F]
    def setGauge(name: String, value: Long)(implicit ev: MetricsSource): F[Unit]       = ().pure[F]
    def incrementGauge(name: String, delta: Long)(implicit ev: MetricsSource): F[Unit] = ().pure[F]
    def decrementGauge(name: String, delta: Long)(implicit ev: MetricsSource): F[Unit] = ().pure[F]
    def record(name: String, value: Long, count: Long = 1)(implicit ev: MetricsSource): F[Unit] =
      ().pure[F]
  }

}

sealed abstract class MetricsInstances {
  implicit def eitherT[E, F[_]: Monad: Metrics[?[_]]]: Metrics[EitherT[F, E, ?]] =
    Metrics.forTrans[F, EitherT[?[_], E, ?]]
}

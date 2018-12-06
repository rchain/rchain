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

  def timer[A](name: String, block: F[A])(implicit ev: MetricsSource): F[A]
}

object Metrics extends MetricsInstances {
  def apply[F[_]](implicit M: Metrics[F]): Metrics[F] = M

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
    def timer[A](name: String, block: F[A])(implicit ev: MetricsSource): F[A] = block
  }

}

sealed abstract class MetricsInstances {

  implicit def eitherT[E, F[_]: Monad](implicit evF: Metrics[F]): Metrics[EitherT[F, E, ?]] =
    new Metrics[EitherT[F, E, ?]] {
      def incrementCounter(name: String, delta: Long = 1)(
          implicit ev: MetricsSource
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementCounter(name, delta))

      def incrementSampler(name: String, delta: Long = 1)(
          implicit ev: MetricsSource
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementSampler(name, delta))

      def sample(name: String)(implicit ev: MetricsSource): EitherT[F, E, Unit] =
        EitherT.liftF(evF.sample(name))

      def setGauge(name: String, value: Long)(implicit ev: MetricsSource): EitherT[F, E, Unit] =
        EitherT.liftF(evF.setGauge(name, value))

      def incrementGauge(name: String, delta: Long = 1)(
          implicit ev: MetricsSource
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementGauge(name, delta))

      def decrementGauge(name: String, delta: Long = 1)(
          implicit ev: MetricsSource
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.decrementGauge(name, delta))

      def record(name: String, value: Long, count: Long = 1)(
          implicit ev: MetricsSource
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.record(name, count))

      def timer[A](name: String, block: EitherT[F, E, A])(
          implicit ev: MetricsSource
      ): EitherT[F, E, A] =
        EitherT(evF.timer(name, block.value))
    }
}

package coop.rchain.metrics

import cats._
import cats.data._
import cats.implicits._

trait Metrics[F[_]] {
  // Counter
  def incrementCounter(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit]

  // RangeSampler
  def incrementSampler(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit]
  def sample(name: String)(implicit ev: Metrics.Source): F[Unit]

  // Gauge
  def setGauge(name: String, value: Long)(implicit ev: Metrics.Source): F[Unit]

  def incrementGauge(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit]

  def decrementGauge(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit]

  // Histogram
  def record(name: String, value: Long, count: Long = 1)(implicit ev: Metrics.Source): F[Unit]

  def timer[A](name: String, block: F[A])(implicit ev: Metrics.Source): F[A]
}

object Metrics extends MetricsInstances {
  def apply[F[_]](implicit M: Metrics[F]): Metrics[F] = M

  class MetricsNOP[F[_]: Applicative] extends Metrics[F] {
    def incrementCounter(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit] =
      ().pure[F]
    def incrementSampler(name: String, delta: Long = 1)(implicit ev: Metrics.Source): F[Unit] =
      ().pure[F]
    def sample(name: String)(implicit ev: Metrics.Source): F[Unit]                      = ().pure[F]
    def setGauge(name: String, value: Long)(implicit ev: Metrics.Source): F[Unit]       = ().pure[F]
    def incrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] = ().pure[F]
    def decrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] = ().pure[F]
    def record(name: String, value: Long, count: Long = 1)(implicit ev: Metrics.Source): F[Unit] =
      ().pure[F]
    def timer[A](name: String, block: F[A])(implicit ev: Metrics.Source): F[A] = block
  }

  import shapeless.tag.@@
  sealed trait SourceTag
  type Source = String @@ SourceTag
  private def Source(name: String): Source         = name.asInstanceOf[Source]
  def Source(prefix: Source, name: String): Source = Source(s"$prefix.$name")
  val BaseSource: Source                           = Source("rchain")
}

sealed abstract class MetricsInstances {

  implicit def eitherT[E, F[_]: Monad](implicit evF: Metrics[F]): Metrics[EitherT[F, E, ?]] =
    new Metrics[EitherT[F, E, ?]] {
      def incrementCounter(name: String, delta: Long = 1)(
          implicit ev: Metrics.Source
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementCounter(name, delta))

      def incrementSampler(name: String, delta: Long = 1)(
          implicit ev: Metrics.Source
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementSampler(name, delta))

      def sample(name: String)(implicit ev: Metrics.Source): EitherT[F, E, Unit] =
        EitherT.liftF(evF.sample(name))

      def setGauge(name: String, value: Long)(implicit ev: Metrics.Source): EitherT[F, E, Unit] =
        EitherT.liftF(evF.setGauge(name, value))

      def incrementGauge(name: String, delta: Long = 1)(
          implicit ev: Metrics.Source
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.incrementGauge(name, delta))

      def decrementGauge(name: String, delta: Long = 1)(
          implicit ev: Metrics.Source
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.decrementGauge(name, delta))

      def record(name: String, value: Long, count: Long = 1)(
          implicit ev: Metrics.Source
      ): EitherT[F, E, Unit] =
        EitherT.liftF(evF.record(name, count))

      def timer[A](name: String, block: EitherT[F, E, A])(
          implicit ev: Metrics.Source
      ): EitherT[F, E, A] =
        EitherT(evF.timer(name, block.value))
    }
}

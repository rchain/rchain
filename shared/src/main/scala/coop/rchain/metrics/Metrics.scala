package coop.rchain.metrics

import cats._
import cats.implicits._
import coop.rchain.metrics.Metrics.Source

trait Span[F[_]] {
  def mark(name: String): F[Unit]
  def trace[A](source: Metrics.Source)(block: Span[F] => F[A]): F[A]
}

trait CloseableSpan[F[_]] extends Span[F] {
  def close(): F[Unit]
}

final case class NoopSpan[F[_]: Applicative]() extends CloseableSpan[F] {
  def mark(name: String): F[Unit]                            = ().pure[F]
  def close(): F[Unit]                                       = ().pure[F]
  def trace[A](source: Source)(block: Span[F] => F[A]): F[A] = block(this)
}

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

  def span(source: Metrics.Source): F[CloseableSpan[F]]

  def withSpan[A](source: Metrics.Source)(block: Span[F] => F[A]): F[A]
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
    def withSpan[A](source: Metrics.Source)(block: Span[F] => F[A]): F[A]      = block(NoopSpan[F]())

    def span(source: Metrics.Source): F[CloseableSpan[F]] =
      Applicative[F].pure(NoopSpan[F]())
  }

  import shapeless.tag.@@
  sealed trait SourceTag
  type Source = String @@ SourceTag
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def Source(name: String): Source         = name.asInstanceOf[Source]
  def Source(prefix: Source, name: String): Source = Source(s"$prefix.$name")
  val BaseSource: Source                           = Source("rchain")
}

sealed abstract class MetricsInstances {}

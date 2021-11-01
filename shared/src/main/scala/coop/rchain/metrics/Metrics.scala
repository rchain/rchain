package coop.rchain.metrics

import cats._
import cats.data.ReaderT
import cats.syntax.all._
import coop.rchain.metrics.Metrics.Source

trait Span[F[_]] {
  def mark(name: String): F[Unit]
  def trace[A](source: Metrics.Source)(block: F[A]): F[A]
  def withMarks[A](label: String)(block: F[A]): F[A]
}

object Span {
  def apply[F[_]](implicit S: Span[F]) = S
  def noop[F[_]: Applicative]: Span[F] = NoopSpan[F]
}

final case class NoopSpan[F[_]: Applicative]() extends Span[F] {
  override def mark(name: String): F[Unit]                    = ().pure[F]
  override def trace[A](source: Source)(block: F[A]): F[A]    = block
  override def withMarks[A](label: String)(block: F[A]): F[A] = block
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
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def Source(name: String): Source         = name.asInstanceOf[Source]
  def Source(prefix: Source, name: String): Source = Source(s"$prefix.$name")
  val BaseSource: Source                           = Source("rchain")
}

sealed abstract class MetricsInstances {
  def readerTMetrics[F[_], E](m: Metrics[F]): Metrics[ReaderT[F, E, ?]] =
    new Metrics[ReaderT[F, E, ?]] {
      override def incrementCounter(name: String, delta: Long)(
          implicit ev: Source
      ): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.incrementCounter(name, delta)(ev))

      override def incrementSampler(name: String, delta: Long)(
          implicit ev: Source
      ): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.incrementSampler(name, delta)(ev))

      override def sample(name: String)(implicit ev: Source): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.sample(name)(ev))

      override def setGauge(name: String, value: Long)(implicit ev: Source): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.setGauge(name, value)(ev))

      override def incrementGauge(name: String, delta: Long)(
          implicit ev: Source
      ): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.incrementGauge(name, delta)(ev))

      override def decrementGauge(name: String, delta: Long)(
          implicit ev: Source
      ): ReaderT[F, E, Unit] =
        ReaderT.liftF(m.decrementGauge(name, delta)(ev))

      override def record(name: String, value: Long, count: Long)(
          implicit ev: Source
      ): ReaderT[F, E, Unit] = ReaderT.liftF(m.record(name, value)(ev))

      override def timer[A](name: String, block: ReaderT[F, E, A])(
          implicit ev: Source
      ): ReaderT[F, E, A] = ReaderT { e =>
        m.timer(name, block.run(e))(ev)
      }
    }
}

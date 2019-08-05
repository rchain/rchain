package coop.rchain.metrics

import cats._
import cats.implicits._
import coop.rchain.metrics.Span.TraceId
import monix.execution.atomic.AtomicLong

trait Span[F[_]] {
  def mark(name: String)(implicit traceId: TraceId): F[Unit]
  def trace[A](source: Metrics.Source, parentId: TraceId)(block: TraceId => F[A]): F[A]
  def withMarks[A](label: String)(block: F[A])(implicit traceId: TraceId): F[A]
}

object Span {
  final case class TraceId(v: Long) extends AnyVal
  val empty: TraceId  = TraceId(0L)
  private val counter = AtomicLong(0L)
  def next: TraceId   = TraceId(counter.getAndIncrement())

  def apply[F[_]](implicit S: Span[F]): Span[F] = S
  def noop[F[_]: Applicative]: Span[F]          = NoopSpan[F]
}

final case class NoopSpan[F[_]: Applicative]() extends Span[F] {
  override def mark(name: String)(implicit traceId: TraceId): F[Unit] = ().pure[F]
  def trace[A](source: Metrics.Source, parentId: TraceId)(block: TraceId => F[A]): F[A] =
    block(parentId)
  override def withMarks[A](label: String)(block: F[A])(implicit traceId: TraceId): F[A] = block
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

sealed abstract class MetricsInstances {}

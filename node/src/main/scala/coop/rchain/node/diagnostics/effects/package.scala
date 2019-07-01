package coop.rchain.node.diagnostics

import cats.effect.Sync
import cats.implicits._
import cats.mtl.ApplicativeLocal
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.Metrics.Source
import kamon.Kamon
import kamon.trace.{Span => KSpan}

private object KamonTracer {
  def start[F[_]: Sync](source: Source, networkId: String, host: String): F[KSpan] = Sync[F].delay {
    Kamon
      .buildSpan(source)
      .withTag("network-id", networkId)
      .withTag("host", host)
      .start()
  }
  def start[F[_]: Sync](source: Source, parent: KSpan, networkId: String, host: String): F[KSpan] =
    Sync[F].delay {
      Kamon
        .buildSpan(source)
        .withTag("network-id", networkId)
        .withTag("host", host)
        .asChildOf(parent)
        .start()
    }

  def end[F[_]: Sync](span: KSpan): F[Unit] =
    Sync[F].delay { span.finish() }
  def mark[F[_]: Sync](span: KSpan, mark: String): F[Unit] =
    Sync[F].delay { span.mark(mark) }.as(())
}

trait Trace

object Trace {
  private[diagnostics] final case class DefaultTrace(s: Source) extends Trace
  private[diagnostics] final case class KamonTrace(s: KSpan)    extends Trace

  def source(s: Source): Trace = DefaultTrace(s)
  def kamon(s: KSpan): Trace   = KamonTrace(s)
}

package object effects {

  type AskTrace[F[_]] = ApplicativeLocal[F, Trace]
  object AskTrace {
    def apply[F[_]](implicit ev: AskTrace[F]): AskTrace[F] = ev
  }

  /**
    * kamon based implementation of tracing that uses the effect stack to track the current span
    *
    * if a trace is already available it will be used in `mark`
    * otherwise a `DefaultTrace` will be transformed into a KamonTrace and will be treated as current
    */
  def span[F[_]: Sync: AskTrace](networkId: String, host: String): Span[F] =
    new Span[F] {
      private[this] val A = AskTrace[F]
      import A._
      import Trace._

      override def mark(name: String): F[Unit] = {
        ask.map {
          case DefaultTrace(source) =>
            trace(source) {
              mark(name)
            }
          case KamonTrace(ks) => ks.mark(name)
        }
      }.as(())

      override def trace[A](source: Source)(block: F[A]): F[A] =
        for {
          span <- ask
          r <- Sync[F].bracket((span match {
                case DefaultTrace(_) => KamonTracer.start[F](source, networkId, host)
                case KamonTrace(ks)  => KamonTracer.start[F](source, ks, networkId, host)
              }).map(kamon)) {
                scope(_)(block)
              } {
                case KamonTrace(ks) => KamonTracer.end(ks)
              }
        } yield r
    }

  def metrics[F[_]: Sync]: Metrics[F] =
    new Metrics[F] {
      import kamon._

      private val m = scala.collection.concurrent.TrieMap[String, metric.Metric[_]]()

      private def source(name: String)(implicit ev: Metrics.Source): String = s"$ev.$name"

      def incrementCounter(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.counter(source(name))) match {
            case c: metric.Counter => c.increment(delta)
          }
        }

      def incrementSampler(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.rangeSampler(source(name))) match {
            case c: metric.RangeSampler => c.increment(delta)
          }
        }

      def sample(name: String)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.rangeSampler(source(name))) match {
            case c: metric.RangeSampler => c.sample
          }
        }

      def setGauge(name: String, value: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.gauge(source(name))) match {
            case c: metric.Gauge => c.set(value)
          }
        }

      def incrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.gauge(source(name))) match {
            case c: metric.Gauge => c.increment(delta)
          }
        }

      def decrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.gauge(source(name))) match {
            case c: metric.Gauge => c.decrement(delta)
          }
        }

      def record(name: String, value: Long, count: Long = 1)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.histogram(source(name))) match {
            case c: metric.Histogram => c.record(value, count)
          }
        }

      def timer[A](name: String, block: F[A])(implicit ev: Metrics.Source): F[A] =
        m.getOrElseUpdate(source(name), Kamon.timer(source(name))) match {
          case c: metric.Timer =>
            for {
              t <- Sync[F].delay(c.start())
              r <- block
              _ = t.stop()
            } yield r
        }
    }
}

package coop.rchain.node.diagnostics

import cats.effect.{Resource, Sync}
import cats.implicits._
import coop.rchain.metrics.{CloseableSpan, Metrics, Span}
import coop.rchain.metrics.Metrics.Source

package object effects {

  def metrics[F[_]: Sync]: Metrics[F] =
    new Metrics[F] {
      import kamon._
      import kamon.trace.{Span => KSpan}

      case class KamonSpan(span: KSpan) extends CloseableSpan[F] {
        @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
        override def mark(name: String): F[Unit] = Sync[F].delay { span.mark(name) }
        override def close(): F[Unit]            = Sync[F].delay { span.finish() }

        override def trace[A](source: Source)(block: Span[F] => F[A]): F[A] =
          Resource
            .make {
              Sync[F].delay {
                KamonSpan(Kamon.buildSpan(source).asChildOf(span).start())
              }
            }(s => s.close())
            .use(block)
      }

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

      def span(source: Source): F[CloseableSpan[F]] = Sync[F].delay {
        KamonSpan(Kamon.buildSpan(source).start())
      }

      def withSpan[A](source: Source)(block: Span[F] => F[A]): F[A] =
        Resource.make(span(source))(s => s.close()).use(block)
    }
}

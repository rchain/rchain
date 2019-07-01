package coop.rchain.node.diagnostics

import cats.effect.Sync
import cats.implicits._
import cats.mtl.{ApplicativeLocal}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.Metrics.Source
import kamon.Kamon
import kamon.trace.{Span => KSpan}

sealed trait Trace
final case object NoSpan              extends Trace
final case class KamonSpan(ks: KSpan) extends Trace

package object effects {

  def span[F[_]: Sync](implicit spanLocal: ApplicativeLocal[F, Trace]): Span[F] =
    new Span[F] {
      override def mark(name: String): F[Unit] =
        spanLocal.ask
          .map {
            case NoSpan        => ???
            case KamonSpan(ks) => ks.mark(name)
          }
          .as(())

      override def child[A](source: Source)(block: Span[F] => F[A]): F[A] =
        spanLocal.local {
          case NoSpan =>
            KamonSpan(Kamon.buildSpan(source).start())
          case KamonSpan(ks) =>
            KamonSpan(Kamon.buildSpan(source).asChildOf(ks).start())
        }(
          spanLocal.ask
            .flatMap {
              case NoSpan       => Sync[F].raiseError[A](new RuntimeException("missing trace"))
              case KamonSpan(_) => block(this)
            }
        )
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

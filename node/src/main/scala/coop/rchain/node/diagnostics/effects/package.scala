package coop.rchain.node.diagnostics

import java.lang.management.{ManagementFactory, MemoryType}

import scala.collection.JavaConverters._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.comm.discovery._
import coop.rchain.comm.rp.Connect.ConnectionsCell
import coop.rchain.metrics.{Metrics, Span}
import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import coop.rchain.metrics.Metrics.Source
import javax.management.ObjectName
import monix.eval.Task

package object effects {

  def metrics[F[_]: Sync](networkId: String, host: String): Metrics[F] =
    new Metrics[F] {
      import kamon._
      import kamon.trace.{Span => KSpan}

      case class KamonSpan(span: KSpan) extends Span[F] {
        @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
        override def mark(name: String): F[Unit] = Sync[F].delay { span.mark(name) }
        override def close(): F[Unit]            = Sync[F].delay { span.finish() }
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

      def span(source: Source): F[Span[F]] = Sync[F].delay {
        KamonSpan(
          Kamon
            .buildSpan(source)
            .withTag("network-id", networkId)
            .withTag("host", host)
            .start()
        )
      }
    }
}

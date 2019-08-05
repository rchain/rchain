package coop.rchain.node.diagnostics

import cats.effect.{ExitCase, Sync}
import cats.implicits._
import cats.mtl.ApplicativeLocal
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.Span.TraceId
import kamon.Kamon
import kamon.trace.{Span => KSpan}

import scala.collection.concurrent.TrieMap

private object KamonTracer {
  def end[F[_]: Sync](span: KSpan): F[Unit] =
    Sync[F].delay { span.finish() }
  def mark[F[_]: Sync](span: KSpan, mark: String): F[Unit] =
    Sync[F].delay { span.mark(mark) }.as(())
}

trait Trace

object Trace {
  private[diagnostics] final case class SourceTrace(
      s: Source,
      id: TraceId,
      networkId: String,
      host: String,
      parent: Option[SourceTrace] = None
  ) extends Trace {
    lazy val ks: KSpan = {
      parent match {
        case Some(st) =>
          Kamon
            .buildSpan(s)
            .withTag("network-id", networkId)
            .withTag("host", host)
            .withTag("trace-id", id.toString)
            .asChildOf(st.ks)
            .start()
        case None =>
          Kamon
            .buildSpan(s)
            .withTag("network-id", networkId)
            .withTag("host", host)
            .withTag("trace-id", id.toString)
            .start()
      }
    }

    def mark[F[_]: Sync](name: String): F[Unit] = KamonTracer.mark(ks, name)
    def end[F[_]: Sync](): F[Unit]              = KamonTracer.end(ks)
  }

  def source(s: Source, networkId: String, host: String): Trace =
    SourceTrace(s, Span.next, networkId, host)
}

package object effects {
  type AskTrace[F[_]] = ApplicativeLocal[F, TraceId]
  object AskTrace {
    def apply[F[_]](implicit ev: AskTrace[F]): AskTrace[F] = ev
  }

  /**
    * kamon based implementation of tracing that uses the effect stack to track the current span
    *
    * if a trace is already available it will be used in `mark`
    * otherwise a `DefaultTrace` will be transformed into a KamonTrace and will be treated as current
    */
  def span[F[_]: Sync](networkId: String, host: String): Span[F] =
    new Span[F] {
      import Trace._

      private val spans: TrieMap[TraceId, SourceTrace] = TrieMap.empty

      override def mark(name: String)(implicit traceId: TraceId): F[Unit] =
        Sync[F].delay(spans.get(traceId)).flatMap(_.map(_.mark(name)).getOrElse(Sync[F].unit))

      override def trace[A](source: Source, parentId: TraceId)(block: TraceId => F[A]): F[A] =
        for {
          r <- Sync[F].bracket(Sync[F].delay {
                val maybeParentTrace = spans.get(parentId)
                val traceId          = Span.next
                spans
                  .putIfAbsent(
                    traceId,
                    SourceTrace(source, traceId, networkId, host, maybeParentTrace)
                  )
                  .map(_.id)
                  .getOrElse(traceId)
              }) { traceId =>
                block(traceId)
              } { traceId =>
                Sync[F]
                  .delay(spans.remove(traceId))
                  .flatMap(_.map(_.end()).getOrElse(Sync[F].unit))
              }
        } yield r

      override def withMarks[A](label: String)(block: F[A])(implicit traceId: TraceId): F[A] =
        Sync[F].bracketCase(
          mark(s"started-$label")
        )(_ => block) {
          case (_, ExitCase.Completed) => mark(s"finished-$label")
          case (_, ExitCase.Error(_))  => mark(s"failed-$label")
          case (_, ExitCase.Canceled)  => mark(s"cancelled-$label")
        }
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

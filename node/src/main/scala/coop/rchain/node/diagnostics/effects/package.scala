package coop.rchain.node.diagnostics

import cats.effect.{ExitCase, Sync}
import cats.syntax.all._
import cats.mtl.ApplicativeLocal
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.node.NodeCallCtx
import kamon.Kamon
import kamon.trace.{Span => KSpan}
import monix.execution.atomic.AtomicLong

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
      networkId: String,
      host: String,
      parent: Option[SourceTrace] = None
  ) extends Trace {
    lazy val ks: KSpan = {
      parent match {
        case Some(st) =>
          Kamon
            .spanBuilder(s)
            .tag("network-id", networkId)
            .tag("host", host)
            .asChildOf(st.ks)
            .start()
        case None =>
          Kamon
            .spanBuilder(s)
            .tag("network-id", networkId)
            .tag("host", host)
            .start()
      }
    }

    def mark[F[_]: Sync](name: String): F[Unit] = KamonTracer.mark(ks, name)
    def end[F[_]: Sync](): F[Unit]              = KamonTracer.end(ks)
  }

  def source(s: Source, networkId: String, host: String): Trace = SourceTrace(s, networkId, host)

  def next: TraceId   = TraceId(counter.incrementAndGet())
  private val counter = AtomicLong(0L)

  final case class TraceId(id: Long) extends AnyVal
}

package object effects {

  type AskTrace[F[_]] = ApplicativeLocal[F, NodeCallCtx]
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

      private val spans: TrieMap[TraceId, SourceTrace] = TrieMap.empty

      override def mark(name: String): F[Unit] =
        ask.flatMap { e =>
          spans.get(e.trace).map(_.mark(name)).getOrElse(Sync[F].unit)
        }

      override def trace[A](source: Source)(block: F[A]): F[A] =
        for {
          r <- Sync[F].bracket(ask.map { parentEnvironment =>
                val parent      = spans.get(parentEnvironment.trace)
                val environment = parentEnvironment.next
                spans
                  .putIfAbsent(environment.trace, SourceTrace(source, networkId, host, parent))
                  .map(_ => environment)
                  .getOrElse(environment)
              })(scope(_)(withMarks("trace")(block))) { environment =>
                Sync[F]
                  .delay(spans.remove(environment.trace))
                  .flatMap(_.map(_.end()).getOrElse(Sync[F].unit))
              }
        } yield r

      override def withMarks[A](label: String)(block: F[A]): F[A] =
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

      private val m = scala.collection.concurrent.TrieMap[String, metric.Metric[_,_]]()

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
            case c: metric.Gauge => c.update(value.asInstanceOf[Double])
          }
        }

      def incrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.gauge(source(name))) match {
            case c: metric.Gauge => c.increment(delta.asInstanceOf[Double])
          }
        }

      def decrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        Sync[F].delay {
          m.getOrElseUpdate(source(name), Kamon.gauge(source(name))) match {
            case c: metric.Gauge => c.decrement(delta.asInstanceOf[Double])
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

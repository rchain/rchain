package coop.rchain.node.diagnostics

import cats.effect.{Outcome, Ref, Sync}
import cats.syntax.all._
import cats.mtl.ApplicativeLocal
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.node.runtime.NodeCallCtx
import coop.rchain.sdk.syntax.all.sdkSyntaxVoid
import kamon.Kamon
import kamon.metric.{Instrument, Tagging}
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
      networkId: String,
      host: String,
      parent: Option[SourceTrace] = None
  ) extends Trace {
    lazy val ks: KSpan = {
      parent match {
        case Some(st) =>
          Kamon
            .spanBuilder(s)
            .asChildOf(st.ks)
            .tag("network-id", networkId)
            .tag("host", host)
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

  private val counter = new java.util.concurrent.atomic.AtomicLong(0)
  def next: TraceId   = TraceId(counter.incrementAndGet())

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
          case (_, Outcome.Succeeded(_)) => mark(s"finished-$label")
          case (_, Outcome.Errored(_))   => mark(s"failed-$label")
          case (_, Outcome.Canceled())   => mark(s"cancelled-$label")
        }
    }

  def metrics[F[_]: Sync]: Metrics[F] =
    new Metrics[F] {
      import kamon._

      private val m = Ref.unsafe[F, Map[String, Tagging[_]]](Map())

      private def source(name: String)(implicit ev: Metrics.Source): String = s"$ev.$name"

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def incrementCounter(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.Counter => c.increment(delta)
              case x                 => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.counter(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def incrementSampler(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.RangeSampler => c.increment(delta)
              case x                      => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.rangeSampler(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def sample(name: String)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.RangeSampler => c.sample()
              case x                      => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.rangeSampler(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def setGauge(name: String, value: Long)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.Gauge => c.update(value.toDouble)
              case x               => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.gauge(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def incrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.Gauge => c.increment(delta.toDouble)
              case x               => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.gauge(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def decrementGauge(name: String, delta: Long)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.Gauge => c.decrement(delta.toDouble)
              case x               => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.gauge(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def record(name: String, value: Long, count: Long = 1)(implicit ev: Metrics.Source): F[Unit] =
        m.update { curV =>
          val k = source(name)
          val newM = curV
            .get(k)
            .map {
              case c: metric.Histogram => c.record(value, count)
              case x                   => throw new Exception(s"Wrong Metric: $x")
            }
            .getOrElse(Kamon.histogram(k).withoutTags())
          curV.updated(k, newM)
        }

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def timer[A](name: String, block: F[A])(implicit ev: Metrics.Source): F[A] =
        m.modify {
            case curV =>
              val k = source(name)
              lazy val missCase = {
                val newK: metric.Timer = Kamon.timer(k).withoutTags()
                (curV + (k -> newK)) -> newK
              }
              curV.get(k).fold(missCase) {
                case t: metric.Timer => curV -> t
                case x               => throw new Exception(s"Wrong Metric: $x")
              }
          }
          .flatMap { c =>
            for {
              t <- Sync[F].delay(c.start())
              r <- block
              _ = t.stop()
            } yield r
          }
    }
}

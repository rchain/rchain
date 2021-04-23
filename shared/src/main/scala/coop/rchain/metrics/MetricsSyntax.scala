package coop.rchain.metrics
import coop.rchain.metrics.Metrics.Source

trait MetricsSyntax[A, F[_]] {
  def block: F[A]

  def timer(name: String)(implicit M: Metrics[F], ms: Metrics.Source): F[A] = M.timer(name, block)
}

package object implicits {
  implicit final class MetricsSyntaxConversion[A, F[_]](val block: F[A]) extends MetricsSyntax[A, F]
  implicit final def SpanSyntax[F[_]](span: Span[F]) = new SpanOps[F](span)
}

final class SpanOps[F[_]](private val span: Span[F]) {
  def traceI[A](tag: String)(block: F[A])(implicit parentSource: Source): F[A] =
    span.trace[A](Source(parentSource, tag))(block)
}

package coop.rchain.metrics

trait MetricsSyntax[A, F[_]] {
  def block: F[A]

  def timer(name: String)(implicit M: Metrics[F], ms: Metrics.Source): F[A] = M.timer(name, block)
}

package object implicits {
  implicit final class MetricsSyntaxConversion[A, F[_]](val block: F[A]) extends MetricsSyntax[A, F]
}

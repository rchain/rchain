package coop.rchain.rspace

import coop.rchain.metrics.Span.TraceId

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam A A type representing data and match result
  */
trait Match[F[_], P, A] {

  def get(p: P, a: A)(implicit traceId: TraceId): F[Option[A]]
}
